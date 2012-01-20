(**************************************************************************)
(*                                                                        *)
(*  Ocamlgv: a native graph visualization library for OCaml               *)
(*  Copyright (C) 2008, 2009  Alex Leighton                               *)
(*                                                                        *)
(*  OCamlgv is free software: you can redistribute it and/or modify       *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(** Module containing functorial graph tools. Contains the [VirtualImageGraph]
    and [VirtualImageDigraph] that are the result of all design algorithms in
    the Algorithms module. *)

open Vector

(** Module containing utility functions such as string conversion functions
    and vertex and edge accessor functions for a virtual image graph. *)
module GraphAccess (G : Sig.VirtualImageGraph) = struct

  (** {2 Vertex Functions}

      Convenience functions for extracting data from the vertices. *)

  (** Return the label of the vertex, i.e. the full type of the vertex. *)
  let unpack_v v = G.V.label v

  (** Get the id from a vertex. @param v The vertex to get the id of. *)
  let get_id v = match G.V.label v with (id,_,_,_) -> id

  (** Get the center of a vertex. @param v The vertex to get the center of. *)
  let get_center v = match G.V.label v with (_,ctrpt,_,_) -> ctrpt

  (** Get the label of a vertex. @param v The vertex to get the label of. *)
  let get_label v = match G.V.label v with (_,_,label,_) -> label

  (** Is the given vertex a dummy vertex. @param v The vertex to check. *)
  let is_dummy v = match G.V.label v with (_,_,_,isdummy) -> isdummy

  (** Creates a dummy vertex with the given information, but does not add it
      to the given graph.
      @param g The graph to work on.
      @param p The location of the vertex.
      @param l The label of the dummy vertex. *)
  let make_dummy g x y l =
    G.V.create ((G.nb_vertex g),(Vector.create x y),l,true)

  (** Creates a vertex with the given information, but does not add it to the
      given graph.
      @param g The graph to work on.
      @param p The location of the vertex.
      @param l The label of the vertex. *)
  let make_vertex g x y l =
    G.V.create ((G.nb_vertex g),(Vector.create x y),l,false)

  (** {2 Edge Functions}

      Convenience functions for extracting data from the edges. *)

  (** Return the label of the edge, i.e. the full type of the edge. *)
  let unpack_e e = G.E.label e

  (** Get the pointlist of an edge.
      @param e The edge to get the pointlist from. *)
  let get_pointlist e = match G.E.label e with (ps,_,_) -> ps

  (** Is the given edge defined by bezier curves.
      @param e The edge to check. *)
  let is_curvy e = match G.E.label e with (_,iscurvy,_) -> iscurvy

  (** Get the label of an edge. @param e The edge to get the label of. *)
  let get_edgelabel e = match G.E.label e with (_,_,l) -> l

  (** Get the source vertex of an edge. *)
  let get_src e = G.E.src e

  (** Get the destination vertex of an edge. *)
  let get_dest e = G.E.dst e

  (** {2 String Conversion}

      Functions for converting vertices, edges, and graphs to strings. *)

  let string_of_vertex v = match G.V.label v with
      (id,v,l,isdummy) ->
        let soi = string_of_int in
        let sov = Vector.string_of_vector in
          "V "^(soi id)^": "^(sov v)^" - "^(if isdummy then "dummy" else l)

  let string_of_edge e = match G.E.label e with
      (vlist,curvy,l) ->
        let soi = string_of_int in
        let sovl = Utilities.string_of_list Vector.string_of_vector in
        let id1,id2 = (get_id (G.E.src e)),(get_id (G.E.dst e)) in
        let curvy = if curvy then "yes" else "no" in
        let l = "\""^l^"\"" in
          "E (V"^(soi id1)^"->V"^(soi id2)^") "^l^(sovl vlist)^"curvy? "^curvy

  let string_of_graph g =
    let sov = string_of_vertex in
    let soe = string_of_edge in
    let vlist = ref [] in
    let elist = ref [] in
    let auxv v = vlist := (sov v)::!vlist in
    let auxe e = elist := (soe e)::!elist in
      G.iter_vertex  auxv g;
      G.iter_edges_e auxe g;
      "Graph: " ^ (String.concat " " !vlist) ^ "\n" ^
        (String.concat " " !elist)

end

module V = struct
  type t = int * Vector.t * Sig.label * bool
  let compare x y = compare (fst x) (fst y)
  let hash x = Hashtbl.hash (fst x)
  let equal x y = (fst x) = (fst y)
  let default = -1,Vector.zero,"",false
end

module E = struct
  type t = Vector.t list * bool * Sig.label
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = [],false,""
end

module VirtualImageDigraph = Graph.Imperative.Digraph.AbstractLabeled(V)(E)
module VirtualImageGraph = Graph.Imperative.Graph.AbstractLabeled(V)(E)

module GraphTransformer
  (G : Sig.G) (VIG : Sig.ImperativeVirtualImageGraph)
  (T : sig

     type vshare

     (** Make a vertex given the previous graph, the new graph, the previous
         vertex, and the share. Return the new vertex and the share, modified
         or not. *)
     val make_vertex : G.t -> VIG.t -> G.V.t -> vshare -> VIG.V.t * vshare

     type eshare

     (** Make an edge given the previous graph, the new graph, the previous
         edge, it's source and destination in the new graph, and the share,
         modified or not. *)
     val make_edge : G.t -> VIG.t -> G.E.t -> VIG.V.t option -> VIG.V.t option -> eshare -> VIG.E.t * eshare

   end)
  =
struct

  let add_vertex g newg vtable v acc =
    if Hashtbl.mem vtable v then acc
    else begin
      let newv,acc = T.make_vertex g newg v acc in
        Hashtbl.add vtable v newv;
        VIG.add_vertex newg newv;
        acc
    end

  let add_edge g newg etable vtable e acc =
    if Hashtbl.mem etable e then acc
    else begin
      let s = try Some (Hashtbl.find vtable (G.E.src e))
              with Not_found -> None in
      let d = try Some (Hashtbl.find vtable (G.E.dst e))
              with Not_found -> None in
      let newe,acc = T.make_edge g newg e s d acc in
        Hashtbl.add etable e newe;
        VIG.add_edge_e newg newe;
        acc
    end

  let transform g vinit einit =
    let newg = VIG.create ~size:(G.nb_vertex g) () in
    let vconvtable = Hashtbl.create (G.nb_vertex g) in
    let econvtable = Hashtbl.create (G.nb_edges g) in
      ignore (G.fold_vertex (add_vertex g newg vconvtable)  g  vinit);
      ignore (G.fold_edges_e (add_edge g newg econvtable vconvtable)
                              g  einit);
      newg

end

module GraphScale (G : Sig.VirtualImageGraph) (VIG : Sig.ImperativeVirtualImageGraph) =
struct

  module GAccess = GraphAccess(G)

  let scale_point (sx,sy) v = Vector.scale v sx sy

  module Transformer = struct
    type vshare = float * float
    type eshare = float * float

    let make_vertex g newg v scale =
      let (id,p,lbl,isdummy) = GAccess.unpack_v v in
      let newp = scale_point scale p in
        (VIG.V.create (id,newp,lbl,isdummy)),scale

    let make_edge g newg e src dest scale =
      match src, dest with
          Some(s), Some(d) ->
            let (plist,iscurvy,lbl) = GAccess.unpack_e e in
            let newplist = List.map (scale_point scale) plist in
              (VIG.E.create s (newplist,iscurvy,lbl) d),scale
        | _ -> assert false

  end

  module Scaler = GraphTransformer (G) (VIG) (Transformer)

  let scale g sx sy = Scaler.transform g (sx,sy) (sx,sy)

end

module FindBounds (G : Sig.VirtualImageGraph) : sig

  type bounds

  val bounds : G.t -> bounds

  val top : bounds -> float
  val left : bounds -> float
  val bottom : bounds -> float
  val right : bounds -> float

  val window : bounds -> float * float

end
  =
struct

  module Access = GraphAccess(G)

  type bounds = { top : float; left : float;
                  bottom : float; right : float }

  let empty = { top = infinity; left = infinity;
                bottom = neg_infinity; right = neg_infinity; }

  let add b v =
    { top    = min v.y b.top;
      left   = min v.x b.left;
      bottom = max v.y b.bottom;
      right  = max v.x b.right }

  let count_v v b = add b (Access.get_center v)

  let count_e e b =
    let plist = Access.get_pointlist e in
      List.fold_left add b plist

  let bounds g =
    let b = G.fold_vertex count_v g empty in
      G.fold_edges_e count_e g b

  let top    b = b.top
  let left   b = b.left
  let bottom b = b.bottom
  let right  b = b.right

  let window b = (b.right,b.bottom)

end

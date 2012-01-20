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

(** Contains all of the layout algorithms in functor form. All of the
    algorithms position vertices and edges with the assumption that the
    origin is at the top left of the screen with a positive x-direction to
    the right and a positive y-direction downwards. Some functions are
    provided to translate from this orientation to the more mathematical
    orientation of the origin in the bottom left of the screen. *)

open List
open Vector

(**/**)
let int = int_of_float
(**/**)

module Graph = Graphtools.VirtualImageGraph
module Digraph = Graphtools.VirtualImageDigraph

module GAccess = Graphtools.GraphAccess(Graph)
module DAccess = Graphtools.GraphAccess(Digraph)

module GBounds = Graphtools.FindBounds(Graph)
module DBounds = Graphtools.FindBounds(Digraph)

(** Translates a y position from an orientation with the origin in the top
    left corner to a position where the origin is in the bottom left corner.
    @param ypos   The y coordinate to translate.
    @param height The height of the image. *)
let translate_y ypos height = (float height) -. ypos

(** Translates an x position like [translate_y]. Does nothing because the
    orientation switch does not change x coordinates. *)
let translate_x xpos = xpos

(** Translates a point from an orientation with the origin in the top left
    corner to a position where the origin is in the bottom left corner.
    @param height The height of the image.
    @param p      The point to translate. *)
let translate_point height v = sety v (translate_y v.y height)

(** {2 Algorithms} *)

(** Algorithm for drawing a tree. *)
module Tree =
  functor (G : Sig.G) ->
    functor (Labeler : Sig.Labeler with type v = G.V.label
                                   and  type e = G.E.label) ->
struct

  (** A tree. *)
  type 'a tree = Node of 'a * ('a tree list)

  let rec string_of_positioned_tree (Node ((label,(w,h)), subtrees)) =
    let subtreestring = (String.concat ", "
                           (map (string_of_positioned_tree) subtrees))
    in
      "("^(Labeler.string_of_vertex label)^"|("^(string_of_float w)^","^(string_of_float h)^"): "^subtreestring^")"

  let rec string_of_tree (Node ((label), subtrees)) =
    let subtreestring = (String.concat ", "
                           (map (string_of_tree) subtrees))
    in
      "("^(Labeler.string_of_vertex label)^": "^subtreestring^")"

  (**/**)

  let rec string_of_tree_x_positioned (Node ((label,pos), subtrees)) =
    let subtreestring = (String.concat ", "
                           (map (string_of_tree_x_positioned) subtrees))
    in
      "("^(Labeler.string_of_vertex label)^" ("^(string_of_float pos)^"): "^
        subtreestring^")"

  (** An extent of a tree. An extent is a left position
      and a right position. Both positions are absolute. *)
  type extent = (float * float) list

  (** Moves a tree by moving the root node. *)
  let move_tree ((Node ((label,pos), subtrees)),newx) =
    Node((label, pos+.newx), subtrees)

  (** Moves an extent by simple addition. *)
  let move_extent (extent,x) =
    map (fun (p,q) -> (p+.x,q+.x)) extent

  (** Merges two extents. *)
  let rec merge_extent = function
      [], qs -> qs
    | ps, [] -> ps
    | (p,_)::ps, (_,q)::qs -> (p,q) :: merge_extent (ps, qs)

  let curry f = fun x -> fun y -> f (x,y)

  (** Merge a list of extents. *)
  let merge_extent_list extents =
    fold_right (curry merge_extent) extents []

  (** Fit two extents together. *)
  let rec fit w pextent qextent =
    match pextent,qextent with
        ((_,p)::ps),((q,_)::qs) -> max (fit w ps qs) (p-.q +. w)
      | _,_                     -> 0.0

  (** Fit a list of extents together left justified. *)
  let fitlistl w extents =
    let rec fitlistl' acc treelist =
      match acc,treelist with
          acc,[]      -> []
        | acc,(e::es) ->
            let x = fit w acc e in
              x :: fitlistl' (merge_extent (acc, move_extent (e,x))) es
    in
      fitlistl' [] extents

  (** Fit a list of extents together right justified. *)
  let fitlistr w extents =
    let rec fitlistr' acc treelist =
      match acc,treelist with
          acc,[]      -> []
        | acc,(e::es) ->
            let x = -.(fit w e acc) in
              x :: fitlistr' (merge_extent (move_extent (e,x), acc)) es
    in
      rev (fitlistr' [] (rev extents))

  (** Return the arithmetic mean of two float values. *)
  let mean (x,y) = (x+.y) /. 2.0

  (** Fit a list of extents together by taking the mean of a left fitted list
      and right fitted list. *)
  let fitlist w extents =
    map mean (combine (fitlistl w extents) (fitlistr w extents))

  (** Takes a tree relatively positioned in it's x values and makes all
      x positions absolute.
      @param root_pos The absolute x value of the root of the tree. *)
  let rec make_absolute root_pos (Node ((label,pos),subtrees)) =
    let newpos = pos+.root_pos in
      (Node ((label,newpos),(map (make_absolute newpos) subtrees)))

  (** Sets the heights of all nodes in an absolutely positioned tree.
      @param initheight The height of the root node.
      @param dheight    The change in height from one level of the tree to the next.
      @param root       The tree. *)
  let set_heights initheight dheight root =
    let rec set_heights' h dh (Node ((label,pos),subtrees)) =
      let newsubs = (map (set_heights' (h+.dh) dh) subtrees) in
        Node ((label,(pos,h)),newsubs)
    in set_heights' initheight dheight root

  (** Gets the width of the widest half of the tree. Only works on
      relative positioned trees. *)
  let get_width root =
    let rec aux (Node ((label,pos),subtrees)) w =
      if List.length subtrees = 0 then abs_float (w +. pos)
      else fold_left (fun oldw node ->
                        let treew = (aux node (w +. pos)) in
                          if oldw < treew then treew else oldw) w subtrees
    in aux root 0.0

  (** Gets the length of the tree from root to lowest leaf. Only works on
      relative positioned trees. *)
  let get_height root dheight =
    let rec aux (Node ((label,pos),subtrees)) h =
      if List.length subtrees = 0 then h
      else fold_left (fun oldh node ->
                        let treeh = (aux node (h+.dheight)) in
                          if oldh < treeh then treeh else oldh) h subtrees
    in aux root 0.0

  exception Found of G.V.t

  let get_vertex g =
    try G.iter_vertex (fun v -> raise (Found v)) g;
      failwith "Could not find a root vertex."
    with Found v -> v

  (**/**)

  (** Turns a given graph into the internal tree type.
      @param graph The graph to work on.
      @param root  Vertex of the graph to treat as the root. *)
  let treeify graph node =
    let tbl = Hashtbl.create ((G.nb_vertex graph) + 1) in
    let isNotMember v = not (Hashtbl.mem tbl v) in
    let rec treeify' graph node =
      let children = G.succ graph node in
        Hashtbl.add tbl node true;
        let children = filter isNotMember children in
        let subtrees = map (treeify' graph) children in
          (Node ((G.V.label node), subtrees))
    in treeify' graph node

  (** Turns a positioned tree type into a drawable graph:
      Graphtools.VirtualImageGraph.t.
      @param root The tree to work on.
      @param w    The width of the tree in pixels.
      @param h    The height of the tree in pixels. *)
  let detreeify root w h =
    let nodeHash = Hashtbl.create 100 in
    let g = Graph.create ~size:100 () in
    let add_v = function (Node ((label,(x,y)),subtrees)) as n ->
      if Hashtbl.mem nodeHash n
      then Hashtbl.find nodeHash n
      else
        let id,loc = (Graph.nb_vertex g),(vector x y) in
        let label = Labeler.string_of_vertex label in
        let v = Graph.V.create (id,loc,label,false) in
          Hashtbl.add nodeHash n v; Graph.add_vertex g v; v
    in
    let add_e v1 v2 =
      let v1,v2 = (add_v v1),(add_v v2) in
        match (Graph.V.label v1),(Graph.V.label v2) with
            (_,p1,_,_),(_,p2,_,_) ->
              let e = Graph.E.create v1 ([p1;p2],false,"") v2 in
                Graph.add_edge_e g e
    in
    let rec make parent = function (Node ((label,(x,y)),subtrees)) as n ->
      match parent with
          None -> List.iter (make (Some n)) subtrees
        | Some p -> add_e p n; List.iter (make (Some n)) subtrees
    in
      make None root; (w,h,g)

  (** Takes a tree type and lays it out according to a special algorithm for
      trees.
      @param tree The tree to work on.
      @param root_x The x position of the root.
      @param root_y The y position of the root.
      @param dx     The change in x from vertex to vertex.
      @param dy     The change in y between each height of the tree. *)
  let layout_tree tree root_y dx dy =
    let rec design' (Node(label, subtrees)) =
      let trees,extents = split (map design' subtrees)                in
      let positions     = fitlist dx extents                          in
      let ptrees        = map move_tree (combine trees positions)     in
      let pextents      = map move_extent (combine extents positions) in
      let resultextent  = (0.0,0.0) :: merge_extent_list pextents     in
      let resulttree    = Node((label,0.0), ptrees)                   in
        (resulttree, resultextent)
    in
    let designtree    = fst (design' tree)                      in
    let width         = get_width designtree                    in
    let height        = get_height designtree dy                in
    let absolutetree  = make_absolute (width+.50.0) designtree  in
    let heightedtree  = set_heights root_y dy absolutetree      in
      match heightedtree with (Node ((label,(w,h)),subtrees)) ->
        let imgwidth = int ((width *. 2.0) +. 100.0) in
          heightedtree,imgwidth,(int_of_float (height+.50.))

  (** Returns (width * height * layout graph) that is the layout of the
      given tree.
      @param g      The graph to work on.
      @param root   Vertex of the graph to treat as the root.
      @param root_y The y position of the root.
      @param dx     The change in x from vertex to vertex.
      @param dy     The change in y between each height of the tree. *)
  let layout g root root_y dx dy =
    let root = match root with None -> get_vertex g | Some v -> v in
    let tree,w,h = layout_tree (treeify g root) root_y dx dy in
      detreeify tree w h
(*    let w,h = GBounds.window (GBounds.bounds g) in *)
(*      (int w),(int h),g *)

end

(** Layered layout for directed acyclic graphs. *)
module Layered =
  functor (G : Sig.G) ->
    functor (Labeler : Sig.Labeler with type v = G.V.label
                                   and  type e = G.E.label) ->
struct

  (**/**)

  (** An extension to the standard library's Set module. This module
      contains a comparison function to compare sets lexicographically,
      where the largest item of the set is the most significant. It also
      contains a few utility functions. *)
  module LexSet (Ord : Set.OrderedType) = struct
    include Set.Make(Ord)

    (** Lexicographic ordering on sets, where the largest
        item of the set is the most significant.
        Example: \{1,4,7\} < \{3,8\} and \{3,4,9\} < \{1,5,9\} *)
    let rec lexico_compare s1 s2 =
      if is_empty s1
      then (if is_empty s2 then 0 else -1)
      else
        if is_empty s2
        then 1
        else
          let max1,max2 = (max_elt s1),(max_elt s2) in
          let diff = Ord.compare max1 max2 in
            if diff = 0
            then lexico_compare (remove max1 s1) (remove max2 s2)
            else diff

    (** Creates a set of a given list. Tail recursive. *)
    let set_of_list list =
      List.fold_left (fun s elt -> add elt s) empty list

    let string_of_vertexset s f =
      (fold (fun elt str -> str ^ ", " ^ (f elt)) s "Set: [") ^ "]"

  end

  module VertexSet = LexSet(struct
                              type t = G.V.t
                              let compare = G.V.compare
                            end)

  module LabeledVertexSet = LexSet(struct
                                     type t = G.V.t * int
                                     let compare x y =
                                       Pervasives.compare (snd x) (snd y)
                                   end)

  exception Found of G.V.t

  (** Finds one source vertex in the given graph. A source is any vertex
      with an in-degree of 0. Raises [Not_found] if there are no source
      vertices. In other words, don't pass a cyclic graph as an argument
      (this function will terminate on a cyclic graph). *)
  let find_a_source g =
    let find v =
      if G.in_degree g v = 0
      then raise (Found v)
      else ()
    in
      try
        G.iter_vertex find g; raise Not_found
      with Found v -> v

  let string_of_list l f = String.concat "," (List.map f l)

  let string_of_hash h f1 f2 =
    (Hashtbl.fold
      (fun k v str -> Printf.sprintf "%s %s->%s " str (f1 k) (f2 v))
      h "[") ^ " ]"

  let get_one_vertex g =
    try G.iter_vertex (fun v -> raise (Found v)) g; raise Not_found
    with Found v -> v

  (** Given a graph, returns a set containing all vertices. *)
  let setofvertices_of_graph g =
    let add x s = VertexSet.add x s in
      G.fold_vertex add g VertexSet.empty

  (** Returns a list of labeled predecessors to a given vertex.
      @param g The graph to work on.
      @param h The hashtable containing all labeled vertices.
      @param v The vertex whose labeled predecessors are being returned. *)
  let get_list_of_labeled_predecessors g h v =
    let aux list v =
      if Hashtbl.mem h v
      then (v,(Hashtbl.find h v))::list
      else list
    in
      List.fold_left aux [] (G.pred g v)

  (** Returns a list of successors to a vertex that have been assigned to a
      layer.
      @param g The graph to work on.
      @param h The hashtable containing all vertices assigned to a layer.
      @param v The vertex whose layered successors are being returned. *)
  let get_list_of_layered_successors g h v =
    let aux list v =
      if Hashtbl.mem h v
      then (v,(Hashtbl.find h v))::list
      else list
    in
      List.fold_left aux [] (G.succ g v)

  (** Return the vertex with the best set of labeled predecessors.
      @param g The graph to work on.
      @param h The hashtable containing all labeled vertices.
      @param s The set of unlabeled vertices. *)
  let get_best_vertex_to_be_labeled g h s =
    let vs_and_ps =
      let has_all_preds_labeled v list =
        let preds  = G.pred g v in
        let lpreds = get_list_of_labeled_predecessors g h v in
          if (List.length preds) = (List.length lpreds)
          then (v,(LabeledVertexSet.set_of_list lpreds))::list
          else list
      in VertexSet.fold (has_all_preds_labeled) s []
    in
    let get_vertex_with_best_set (v1,s1) (v2,s2) =
      (* Choose the vertex whose set of labeled predecessors is the minimum
         in the lexicographic ordering over labels. *)
      let diff = LabeledVertexSet.lexico_compare s1 s2 in
        if diff <= 0 then (v1,s1) else (v2,s2)
    in
      try
        List.fold_left get_vertex_with_best_set (List.hd vs_and_ps) vs_and_ps
      with Failure msg -> (
        Utilities.report "get_best_vertex_to_be_labeled"
                         "algorithms.ml" "396" msg;
        failwith msg)

  (** Return the best vertex to be layered. This is determined by choosing a
      vertex that has not been assigned a layer yet, and for which all of its
      successors have been assigned a layer. If there is more than one
      vertex, the function returns the vertex with the largest label.
      @param g         The graph to work on.
      @param labels    The hashtable containing all labeled vertices.
      @param layered   The hashtable containing all layered vertices.
      @param unlayered The set containing all unlayered vertices. *)
  let get_best_vertex_to_be_layered g labels layered unlayered =
    let vs =
      let has_all_succs_layered v list =
        let succs  = G.succ g v in
        let lsuccs = get_list_of_layered_successors g layered v in
          if (List.length succs) = (List.length lsuccs)
          then v::list else list
      in VertexSet.fold (has_all_succs_layered) unlayered []
    in
    let get_vertex_with_best_label v1 v2 =
      (* Choose the vertex with the largest label. *)
      if (Hashtbl.find labels v1) >= (Hashtbl.find labels v2)
      then v1 else v2
    in
      if vs = [] then None else
        Some (List.fold_left get_vertex_with_best_label (List.hd vs) vs)

  (** Produces a set of vertices labeled with integers. Do not pass an
      undirected graph or cyclic digraph as argument (not guaranteed
      termination in that case). *)
  let label_vertices g =
    let unlabeled = ref (setofvertices_of_graph g) in
    let labeled = Hashtbl.create (G.nb_vertex g) in
    let label v i =
      Hashtbl.add labeled (v:G.vertex) i;
      unlabeled := VertexSet.remove v !unlabeled
    in
      for i = 0 to ((G.nb_vertex g)-1) do
        let v = (get_best_vertex_to_be_labeled g labeled !unlabeled) in
          label (fst v) i
      done ;
      labeled

  (** Layers all the vertices in a given acyclic digraph. The layering is
      a way of keeping the layout of the graph coherent. It can be shown
      that the height h, of the resulting layered output is such, given a
      minimum height hmin of a layer of width of w:
      [h <= (2 - 2/w) * hmin].
      @param g The graph to work on.
      @param w The width of each layer. The number of vertices allowed in
               each layer. *)
  let layer_vertices g w =
    let labeled_vertices = label_vertices g in
    let unlayered = ref (setofvertices_of_graph g) in
    let layertable = Hashtbl.create (G.nb_vertex g) in
    let num_in_current_layer = ref 0 in
    let current_layer = ref 0 in
    let layer v =
      Hashtbl.add layertable v !current_layer;
      num_in_current_layer := !num_in_current_layer + 1;
      unlayered := VertexSet.remove v !unlayered
    in
      while not(VertexSet.is_empty !unlayered) do
        match get_best_vertex_to_be_layered g labeled_vertices
                                              layertable
                                              !unlayered
        with
            Some(v) ->
              if !num_in_current_layer < w
              then layer v
              else ( current_layer := !current_layer + 1;
                     num_in_current_layer := 0;
                     layer v )
          | None ->
              let failstr = "layer_vertices: current layer: " ^
                (string_of_int !current_layer) ^
                ". You probably input either a cyclic digraph" ^
                " or an undirected graph."
              in failwith failstr
      done ;
      layertable,(!current_layer+1)

  module InitialTransform =
    Graphtools.GraphTransformer(G)(Digraph)
      (struct
         type vshare = (G.V.t, int) Hashtbl.t *
                       (Digraph.V.t, int) Hashtbl.t
         type eshare = unit

         let make_vertex g newg v (oldlayers,newlayers) =
           let lbl = Labeler.string_of_vertex (G.V.label v) in
           let newv = DAccess.make_vertex newg 0.0 0.0 lbl in
             Hashtbl.add newlayers newv (Hashtbl.find oldlayers v);
             newv,(oldlayers,newlayers)

         let make_edge g newg e src dest () =
           match src,dest with
               Some(s),Some(d) ->
                 let plist = [(DAccess.get_center s);
                              (DAccess.get_center d)] in
                 let lbl = Labeler.string_of_edge (G.E.label e) in
                 let newe = Digraph.E.create s (plist,false,lbl) d in
                   newe,()
             | _ -> assert false
       end)

  let trans_graph g layertable newlayertable =
    InitialTransform.transform g (layertable,newlayertable) ()

  let make_dummy_nodes g layers =
    Digraph.iter_edges_e
    (fun e ->
       let src,dest = (Digraph.E.src e),(Digraph.E.dst e) in
       let ls,ld = (Hashtbl.find layers src),(Hashtbl.find layers dest) in
         if ls - ld <= 1 then ()
         else begin
           Digraph.remove_edge_e g e;
           let cur_v = ref src in
           for i = ls-1 downto ld do
             let dummy = if i = ld then dest
                         else DAccess.make_dummy g 0.0 0.0 "__dummy" in
             let vp,dp = (DAccess.get_center !cur_v),Vector.zero in
             let newe = Digraph.E.create !cur_v ([vp;dp],false,"") dummy in
               Digraph.add_edge_e g newe;
               if i = ld then ()
               else Hashtbl.add layers dummy i;
               cur_v := dummy
           done
         end) g

  let group ~by l =
    let rec split e l g = match l with
      | x :: xs when by e x ->
          split e xs (x :: g)
      | l       -> List.rev g, l
    in
    let rec group l gs = match l with
      | []      -> List.rev gs
      | x :: xs ->
          let g, ys = split x xs [] in
            group ys ((x :: g) :: gs)
    in group l []

  let make_layer_array layertable numlayers =
    let layers = Array.make numlayers [] in
      Hashtbl.iter
        (fun v layer -> layers.(layer) <- v :: layers.(layer))
        layertable;
      let isdummy v1 v2 = (DAccess.is_dummy v1) = (DAccess.is_dummy v2) in
      Array.map (group ~by:isdummy) layers

  let rec assign_positions_to_dummys dlist x dx =
    let rec aux x dx = function [] -> []
      | d::rest -> [x] :: aux (x+.dx) dx rest in
    let num_ds = float (List.length dlist) in
    let dx = (x +. dx) /. num_ds in
      aux (x+.dx) dx dlist

  (* let make_graph g layers start_x start_y dx dy = *)
  (*   let newg = Digraph.create ~size:(Digraph.nb_vertex g) () in *)
  (*   let layerxs = Array.make (Array.length layers) start_x in *)
  (*   let assign_positions vs layer = *)
  (*     if DAccess.is_dummy (List.hd vs) *)
  (*     then begin *)
  (*       assign_positions_to_dummys vs layerxs.(layer) dx; *)
  (*       layerxs.(layer) <- layerxs.(layer) +. dx *)
  (*     end *)
  (*     else List.iter (fun v ->  *)

  module SetPositionsTransform =
    Graphtools.GraphTransformer(Digraph)(Digraph)
      (struct
         type vshare = (Digraph.V.t, int) Hashtbl.t *
                       int * float * float * float * float array
         type eshare = unit

         let make_vertex g newg v
             (layertable,numlayers,start_y,dx,dy,layerxs) =
           let layer = Hashtbl.find layertable v in
           let id,p,lbl,isdummy = DAccess.unpack_v v in
           let y = start_y +. ((float numlayers)-.1.0-.(float layer))*.dy in
           let x = if isdummy then layerxs.(layer) -. dx +. 50.0
                              else layerxs.(layer) in
           let newv = Digraph.V.create (id,(vector x y),lbl,isdummy) in
             (if isdummy then layerxs.(layer) <- layerxs.(layer) +. 30.0
                         else layerxs.(layer) <- layerxs.(layer) +. dx);
             newv,(layertable,numlayers,start_y,dx,dy,layerxs)

         let make_edge g newg e src dest () =
           match src,dest with
               Some(s),Some(d) ->
                 let ps,pd = (DAccess.get_center s),(DAccess.get_center d) in
                 let lbl = (DAccess.get_edgelabel e) in
                 let newe = Digraph.E.create s ([ps;pd],false,lbl) d in
                   newe,()
             | _ -> assert false
       end)

  let set_xs_and_ys g layertable numlayers start_x start_y dx dy =
    let layerxs = Array.make numlayers start_x in
      SetPositionsTransform.transform g
        (layertable,numlayers,start_y,dx,dy,layerxs) ()
(**/**)

  (** [design g w start_x start_y dx dy] Lays out a directed acyclic graph
      given [w] number of vertices per layer.
      @param g       The graph to work on.
      @param w       The number of vertices per layer.
      @param start_x The x location of the first vertex in a layer.
      @param start_y The y location of the vertices in the top layer.
      @param dx      The change in x between vertices in a layer.
      @param dy      The change in y between layers. *)
  let layout g w start_x start_y dx dy =
    assert (w > 0);
    let layertable,numlayers = layer_vertices g w in
    let newlayertable = Hashtbl.create (G.nb_vertex g) in
    let dgraph = trans_graph g layertable newlayertable in
      make_dummy_nodes dgraph newlayertable;
      let dgraph = set_xs_and_ys dgraph newlayertable numlayers
                                 start_x start_y dx dy
      in
      let w,h = DBounds.window (DBounds.bounds dgraph) in
        (int w),(int h),dgraph

end

module ForceDirected
  (G : Sig.G)
  (Labeler : Sig.Labeler with type v = G.V.label
                         and  type e = G.E.label) =
struct

  (**/**)

  module VertexMap = Map.Make(G.V)

  let _ = Random.init 0

  (** Rfloat returns a random floating-point number between 0 (inclusive) and
      [bound] (exclusive).
      @param bound The upper bound of the random range. *)
  let rfloat bound = Random.float bound

  (** Creates a random vector given a width and height. *)
  let random_vector w h = (vector (rfloat w) (rfloat h))

  (** Gives all vertices in a graph a random location. Returns a map from
      vertex to vector location.
      @param g The graph to work on.
      @param w The largest valid x-coordinate.
               i.e. the width of the virtual image to create.
      @param h The largest valid y-coordinate.
               i.e. the height of the virtual image to create. *)
  let set_random_positions g w h =
    let map = VertexMap.empty in
    let add m v vect = VertexMap.add v (vect,null_vector) m in
      G.fold_vertex (fun v map -> add map v (random_vector w h)) g map

  (** [make_graph_from_map g m] translates a given graph into a virtual image
      given a VertexMap [m] that maps the vertices in [g] to Vectors. *)
  let make_graph_from_map g m =
    let convtable = Hashtbl.create (G.nb_vertex g) in
    let newg = Graph.create ~size:(G.nb_vertex g) () in
    let make_vertex v =
      if Hashtbl.mem convtable v then Hashtbl.find convtable v
      else
        let pvect,vvect = VertexMap.find v m in
        let lbl = Labeler.string_of_vertex (G.V.label v) in
        let newv = Graph.V.create ((Graph.nb_vertex newg),pvect,lbl,false) in
          Hashtbl.add convtable v newv;
          newv
    in
    let add_edge e =
      let src,dest = (G.E.src e),(G.E.dst e) in
      let ns,nd = (make_vertex src),(make_vertex dest) in
      let (ps,vs),(pd,vd) = (VertexMap.find src m),(VertexMap.find dest m) in
      let lbl = Labeler.string_of_edge (G.E.label e) in
      let newe = Graph.E.create ns ([ps;pd],false,lbl) nd in
        Graph.add_edge_e newg newe
    in
      G.iter_edges_e add_edge g;
      newg

  module Scaler = Graphtools.GraphTransformer(Graph)(Graph)
    (struct
       type vshare = GBounds.bounds * float * float
       type eshare = vshare

       let scale_point (bounds,w,h) v =
         let left,right = GBounds.left bounds, GBounds.right bounds in
         let top,bottom = GBounds.top bounds, GBounds.bottom bounds in
         let left,right = left -. 50.0, right +. 50.0 in
         let top,bottom = top -. 50.0, bottom +. 50.0 in
         let newx = ((v.x -. left) *. w) /. (right -. left) in
         let newy = ((v.y -. top) *. w) /. (bottom -. top) in
           vector newx newy

       let make_vertex g newg v scale =
         let (id,p,lbl,isdummy) = GAccess.unpack_v v in
         let newp = scale_point scale p in
           (Graph.V.create (id,newp,lbl,isdummy)),scale

       let make_edge g newg e src dest scale =
         match src, dest with
             Some(s), Some(d) ->
               let (plist,iscurvy,lbl) = GAccess.unpack_e e in
               let newplist = List.map (scale_point scale) plist in
                 (Graph.E.create s (newplist,iscurvy,lbl) d),scale
           | _ -> assert false

     end)

  (**/**)

  type model = {
    coulomb : float;
    damping : float;
    dt : float;
    max_iterations : int;
    seed : int option;
    spring : float;
    spring_length : float;
  }

  let default = {
    coulomb = 1000.0;
    damping = 0.15;
    dt = 0.4;
    max_iterations = 110;
    seed = None;
    spring = 5.0;
    spring_length = 50.0;
  }

  let print_model m =
    let s = match m.seed with None -> 0 | Some i -> i in
      Printf.printf "Coulomb: %f, Damping: %f, dt: %f, Iter: %d, Seed: %d, Spring: %f, Spring Length: %f\n" m.coulomb m.damping m.dt m.max_iterations s m.spring m.spring_length

  let make_model ?(c=default.coulomb) ?(d=default.damping)
      ?(dt=default.dt) ?(m=default.max_iterations)
      ?(s=default.seed) ?(sp=default.spring) ?(sl=default.spring_length) () =
    { coulomb=c; damping=d; dt=dt; max_iterations=m;
      seed=s; spring=sp; spring_length=sl }

  (**/**)
  let setdt cs dt = {
    coulomb = cs.coulomb;
    damping = cs.damping;
    dt = dt;
    max_iterations = cs.max_iterations;
    seed = cs.seed;
    spring = cs.spring;
    spring_length = cs.spring_length;
  }

  let spring cs m v1 v2 =
    if v1 = v2 then null_vector
    else
      let (p1,v1),(p2,v2) = (VertexMap.find v1 m),(VertexMap.find v2 m) in
      let d = p2 -| p1 in
      let dist = dist p1 p2 in
      let d = d -| ((cs.spring_length *| d) /| dist) in
        cs.spring *| d

  let magnetic cs m v1 v2 =
    if v1 = v2 then null_vector
    else
      let (p1,v1),(p2,v2) = (VertexMap.find v1 m),(VertexMap.find v2 m) in
      let d = p2 -| p1 in
      let dist = dist p1 p2 in
        cs.coulomb *| (d /| (dist*.dist*.dist))

  let spring_repulse g m newm cs v1 =
    let fm =
      G.fold_vertex (fun v2 fm -> fm +| (magnetic cs m v1 v2)) g null_vector
    in
    let fs =
      G.fold_succ (fun v2 fs -> fs +| (spring cs m v1 v2)) g v1 null_vector
    in
    let f = (fs +| fm) in
      let pvect,vvect = VertexMap.find v1 m in
      let vvect = cs.damping *| (vvect +| (cs.dt *| f)) in
      let pvect = pvect +| (cs.dt *| vvect) in
        VertexMap.add v1 (pvect,vvect) newm

  (**/**)

  let layout g ?(constants=default) w h =
    (match constants.seed with None -> () | Some i -> Random.init i);
    let original_map = set_random_positions g w h in
    let old_map = ref original_map in
    let n = constants.max_iterations in
      for i = 1 to n do
        old_map :=
          G.fold_vertex
            (fun v map -> spring_repulse g !old_map map constants v)
            g VertexMap.empty ;
      done;
      let newg = make_graph_from_map g !old_map in
      let bounds = GBounds.bounds newg in
      let newg = Scaler.transform newg (bounds,w,h) (bounds,w,h) in
        (int_of_float w),(int_of_float h),newg

  let layout_list g ?(constants=default) w h =
    (match constants.seed with None -> () | Some i -> Random.init i);
    let result_list = ref [] in
    let original_map = set_random_positions g w h in
    let old_map = ref original_map in
    let n = constants.max_iterations in
      for count = 1 to n do
        old_map :=
          G.fold_vertex
            (fun v map -> spring_repulse g !old_map map constants v)
            g VertexMap.empty ;
        result_list := !old_map :: !result_list
      done;
      let glist = List.map (make_graph_from_map g) !result_list in
      let blist = List.map GBounds.bounds glist in
      let rlist = List.rev_map2
        (fun g b -> ((int_of_float w),
                     (int_of_float h),
                     (Scaler.transform g (b,w,h) (b,w,h)))) glist blist
      in rlist

end

module Spring (G : Sig.G) (Labeler : Sig.Labeler with type v = G.V.label
                                                 and  type e = G.E.label) =
struct

  module VertexMap = Map.Make(G.V)

  let _ = Random.init 0

  (** Rfloat returns a random floating-point number between 0 (inclusive) and
      [bound] (exclusive).
      @param bound The upper bound of the random range. *)
  let rfloat bound = Random.float bound

  (** Creates a random vector given a width and height. *)
  let random_vector w h = (vector (rfloat w) (rfloat h))

  (** Gives all vertices in a graph a random location. Returns a map from
      vertex to vector location and velocity vector.
      @param g The graph to work on.
      @param w The largest valid x-coordinate.
      i.e. the width of the virtual image to create.
      @param h The largest valid y-coordinate.
      i.e. the height of the virtual image to create. *)
  let set_random_positions g w h =
    let map = VertexMap.empty in
    let add m v vect = VertexMap.add v (vect,null_vector) m in
      G.fold_vertex (fun v map -> add map v (random_vector w h)) g map

  (** Gives all vertices in a graph a random location, then sets the
      locations of the vertices in the list of fixed vertices. Returns a map
      from vertex to vector location and velocity vector.
      @param g     The graph to work on.
      @param w     The largest valid x-coordinate.
      @param h     The largest valid y-coordinate.
      @param fixed The list of fixed vertices along with their positions. *)
  let rec set_fixed_positions g w h fixed =
    List.fold_left (fun map (v,x,y) ->
                      VertexMap.add v ((vector x y),null_vector) map)
      (set_random_positions g w h) fixed

  (** [make_graph_from_map g m] translates a given graph into a virtual image
      given a VertexMap [m] that maps the vertices in [g] to Vectors. *)
  let make_graph_from_map g m =
    let convtable = Hashtbl.create (G.nb_vertex g) in
    let newg = Graph.create ~size:(G.nb_vertex g) () in
    let make_vertex v =
      if Hashtbl.mem convtable v then Hashtbl.find convtable v
      else
        let pvect,vvect = VertexMap.find v m in
        let lbl = Labeler.string_of_vertex (G.V.label v) in
        let newv = Graph.V.create ((Graph.nb_vertex newg),pvect,lbl,false) in
          Hashtbl.add convtable v newv;
          newv
    in
    let add_edge e =
      let src,dest = (G.E.src e),(G.E.dst e) in
      let ns,nd = (make_vertex src),(make_vertex dest) in
      let (ps,vs),(pd,vd) = (VertexMap.find src m),(VertexMap.find dest m) in
      let lbl = Labeler.string_of_edge (G.E.label e) in
      let newe = Graph.E.create ns ([ps;pd],false,lbl) nd in
        Graph.add_edge_e newg newe
    in
      G.iter_edges_e add_edge g;
      newg

  exception Got3 of G.V.t list

  let get_three_vertices g = try
    ignore (G.fold_vertex
      (fun v rest ->
         if List.length rest = 3 then raise (Got3 rest) else v::rest) g []);
    failwith ("get_tree_vertices: there are less than three" ^
              " vertices in the graph.")
  with Got3 vs -> vs

  (** Returns a list of vertices with positions. Only pass a list containing
      three vertices. The positions chosen are the vertices of a triangle.
      @param fixed The list of vertices to position.
      @param w     The width of the image.
      @param h     The height of the image. *)
  let generate_fixed_positions fixed w h =
    match fixed with
        [v1;v2;v3] -> [
          v1, (w /. 2.0),   50.0;
          v2,  50.0,       (h -. 50.0);
          v3, (w -. 50.0), (h -. 50.0)
        ]
      | _ -> assert false

  let find v fixed =
    try ignore (List.find (fun (v',x,y) -> v = v') fixed); true
    with _ -> false

  let spring g m newm fixed v1 =
    let v1p,v1v = VertexMap.find v1 m in
      if find v1 fixed
      then VertexMap.add v1 (v1p,v1v) newm
      else
        let f = G.fold_succ (fun v2 f -> let v2p,v2v = VertexMap.find v2 m in
                               f +| (v2p -| v1p)) g v1 zero in
        let v1v = v1v +| f in
        let v1p = v1p +| v1v in
          VertexMap.add v1 (v1p,v1v) newm

  (** If you use the optional fixed parameter, make sure the shape that the
      fixed vertices describe is strictly convex. *)
  let layout g ?(fixed=None) ?(n=100) w h =
    let fixed = match fixed with
        None -> generate_fixed_positions (get_three_vertices g) w h
      | Some vs -> vs
    in
    let original_map = set_fixed_positions g w h fixed in
    let old_map = ref original_map in
      for i = 1 to n do
        old_map :=
          G.fold_vertex
            (fun v map -> spring g !old_map map fixed v) g VertexMap.empty
      done;
      let newg = make_graph_from_map g !old_map in
        (int_of_float w),(int_of_float h),newg

end

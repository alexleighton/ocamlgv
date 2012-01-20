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

(** Pack module containing graph and digraph implementations ready to use.
    Vertices are labeled with (string*string) and edges are labeled with
    strings. *)

open Graph.Dot_ast

(** Functor used to create the pack graph and digraph. *)
module MakePack(G : Sig.I with type V.label = (string*string) and type E.label = string) = struct

  include G

  module Labeler = struct
    type v = string * string
    type e = string
    let string_of_vertex (id,lbl) = lbl
    let string_of_edge s:string = s
  end

  exception Found of V.t
  exception Not_found_v of string

  let find_vertex g i =
    try
      iter_vertex (fun v -> if (fst (V.label v)) = i
                   then raise (Found v)) g;
      raise (Not_found_v i)
    with Found v -> v

  let root = ref ""

  (** Returns the vertex associated with the root of a tree.
      @raise Not_found if the root does not exist. *)
  let get_root g =
    try Some (find_vertex g !root)
    with Not_found_v i -> None

  (** Sets the vertex associated with the root of a tree. *)
  let set_root g v = root := v

  module Builder = Graph.Builder.I(G)

  module Dfs = Graph.Traverse.Dfs(G)
  module Bfs = Graph.Traverse.Bfs(G)

  module Components = Graph.Components.Make(G)

  include Graph.Oper.Make(Builder)

  module PathCheck = Graph.Path.Check(G)

  module Topological = Graph.Topological.Make(G)

  module DotParser =
    Graph.Dot.Parse
      (Builder)
      (struct
         let nodes = Hashtbl.create 97
         let node (id,_) _ =
           try
             Hashtbl.find nodes id
           with Not_found ->
             match id with
                 Ident str -> Hashtbl.add nodes id (str,str); str,str
               | Number str -> Hashtbl.add nodes id (str,str); str,str
               | String str -> Hashtbl.add nodes id (str,str); str,str
               | Html str -> Hashtbl.add nodes id (str,str); str,str
         let edge _ = ""
       end)

  module DotTreeParser =
    Graph.Dot.Parse
      (Builder)
      (struct
         let nodes = Hashtbl.create 97
         let get_id_contents = function
             Ident str | Number str | String str | Html str -> str
         let get_label attrs =
           let aux result =
             match result with
                 Some x -> fun y -> Some x
                 | None ->
                     function
                         (Ident "label"),(Some id) ->
                           Some (get_id_contents id)
                       | (String "label"),(Some id) ->
                           Some (get_id_contents id)
                       | _ -> None
           in List.fold_left aux None attrs
         let node (id,_) attrs =
           let node_aux attrs = match attrs with
               (Ident "root"),(Some i) -> root := (get_id_contents i)
             | (String "root"),(Some i) -> root := (get_id_contents i)
             | (Ident "root"),None -> root := (get_id_contents id)
             | (String "root"),None -> root := (get_id_contents id)
             | _ -> () in
           let attrs = List.flatten attrs in
             List.iter node_aux attrs;
             try
               Hashtbl.find nodes id
             with Not_found ->
               let str = get_id_contents id in
                 match get_label attrs with
                     Some lbl ->  Hashtbl.add nodes id (str,lbl); (str,lbl)
                   | None     ->  Hashtbl.add nodes id (str,str); (str,str)
         let edge _ = ""
       end)

  let parse_dot_file = DotTreeParser.parse
  let parse_dot_file_simple = DotParser.parse

  (**********************************************************************)
  (* Section for algorithms.                                            *)

  module TreeLayout = Algorithms.Tree(G)(Labeler)

  module LayeredLayout = Algorithms.Layered(G)(Labeler)

  module ForceLayout = Algorithms.ForceDirected(G)(Labeler)

  module SpringLayout = Algorithms.Spring(G)(Labeler)

end

module V = struct
  type t = (string * string)
  let get_id = fst
  let get_label = snd
  let compare x y = compare (fst x) (fst y)
  let hash x = Hashtbl.hash (fst x)
  let equal x y = (fst x) = (fst y)
  let default = "",""
end

module E = struct
  type t = string
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = ""
end

module Digraph = MakePack(Graph.Imperative.Digraph.AbstractLabeled(V)(E))

module Graph = MakePack(Graph.Imperative.Graph.AbstractLabeled(V)(E))

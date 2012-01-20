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

(** A module for drawing graphs using Extended Post Script. *)

open Pstools

let add = Buffer.add_string

(** The structure containing functions to draw a graph using the Cairo vector
    graphics library. *)
module PSDrawer (G : Sig.VirtualImageGraph) =
struct

  include Graphtools.GraphAccess(G)

  type graph  = G.t
  type edge   = G.edge
  type vertex = G.vertex

  type vimage = int * int * graph
  type canvas = Buffer.t

  let init (w,h,g) =
    let buffer = Buffer.create 100 in
      (* Make proper estimation based on graph size *)
      Buffer.add_string buffer (header w h);
      buffer

  let vs_fold v buffer =
    if is_dummy v then buffer else begin
      let label,center = (get_label v),(get_center v) in
        add buffer (empty_rectangle center 20.0 20.0 1.0);
        add buffer (draw_string label center 12);
        buffer
    end

  let es_fold_undirected e buffer =
    let plist = get_pointlist e in
      if is_curvy e
      then add buffer (curved_line plist 1.0)
      else add buffer (straight_line plist 1.0) ;
      buffer

  let es_fold_directed e buffer =
    let plist = get_pointlist e in
      if is_curvy e
      then add buffer (straight_arrow_line plist 1.0)
      else add buffer (straight_arrow_line plist 1.0) ;
      buffer

end

module DrawGraph =
  AbstractDrawer.Make(Graphtools.VirtualImageGraph)
    (PSDrawer(Graphtools.VirtualImageGraph))
module DrawDigraph =
  AbstractDrawer.Make(Graphtools.VirtualImageDigraph)
    (PSDrawer(Graphtools.VirtualImageDigraph))

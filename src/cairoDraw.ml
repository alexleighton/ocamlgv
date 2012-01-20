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

(** A module for drawing graphs using the Cairo vector graphics library. *)

open Cairotools

(** The structure containing functions to draw a graph using the Cairo vector
    graphics library. *)
module CairoDrawer (G : Sig.VirtualImageGraph) =
struct

  include Graphtools.GraphAccess(G)

  type canvas = Cairo.image_surface
  type graph = G.t
  type edge = G.E.t
  type vertex = G.V.t
  type vimage = int * int * graph

  (** Initializes the Cairo surface. *)
  let init (w,h,g) =
    let surface = create_surface w h in
    let context = get_context surface in
      fill_surface context Utilities.white;
      surface

  (** Draws a given vertex on a given Cairo surface. *)
  let vs_fold v surface =
    let context = get_context surface in
      if is_dummy v then ()
      else begin
        let label,center = (get_label v),(get_center v) in
        let x,y,w,h = get_textbox context label in
          fillrectangle context center Utilities.white w h 3.0 ;
          draw_string context label center 12.0
      end;
      surface

  (** Draws a given edge on a given Cairo surface. *)
  let es_fold_undirected e surface =
    let context = get_context surface in
    let plist = get_pointlist e in
      if is_curvy e
      then curved_line context plist 1.0
      else straight_line context plist 1.0 ;
      surface

  (** Draws a given edge on a given Cairo surface. *)
  let es_fold_directed e surface =
    let context = get_context surface in
    let plist = get_pointlist e in
      if is_curvy e
      then straight_arrow_line context plist 1.0
      else straight_arrow_line context plist 1.0 ;
      surface

end

(** Draw a virtual image graph onto a Cairo surface. *)
module DrawGraph =
  AbstractDrawer.Make(Graphtools.VirtualImageGraph)
                     (CairoDrawer(Graphtools.VirtualImageGraph))

(** Draw a virtual image digraph onto a Cairo surface. *)
module DrawDigraph =
  AbstractDrawer.Make(Graphtools.VirtualImageDigraph)
                     (CairoDrawer(Graphtools.VirtualImageDigraph))

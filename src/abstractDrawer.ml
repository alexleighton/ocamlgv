(**************************************************************************)
(*                                                                        *)
(*  Ocamlgv: a native graph visualization library for OCaml               *)
(*  Copyright (C) 2008, 2009  Alex Leighton                               *)
(*                                                                        *)
(*  Ocamlgv is free software: you can redistribute it and/or modify       *)
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

(** An abstract drawing functor. Takes a virtual image graph and a drawer
    structure and produces a module with a function to draw the graph. *)

(** A structure for drawing individual edges and vertices. In order to use
    the AbstractDrawer.Make functor, the following types and functions must
    be provided. *)
module type Drawer = sig

  type graph
  type edge
  type vertex

  (** A virtual image is a width and a height and a graph. *)
  type vimage = int * int * graph

  (** The canvas to draw on. *)
  type canvas

  (** Initialize the canvas given the virtual image. *)
  val init               : vimage -> canvas

  (** Draw an undirected edge given the edge and the canvas. *)
  val es_fold_undirected : edge   -> canvas -> canvas

  (** Draw a directed edge given the edge and the canvas. *)
  val es_fold_directed   : edge   -> canvas -> canvas

  (** Draw a vertex given the vertex and the canvas. *)
  val vs_fold            : vertex -> canvas -> canvas

end

(** The abstract drawing functor. *)
module Make (G : Sig.G)
            (D : Drawer with type graph  = G.t
                        and  type edge   = G.E.t
                        and  type vertex = G.V.t) =
struct

  include D

  (** Paint the vertices of a given graph, in the given context.
      @param g      The graph to draw.
      @param canvas The canvas to draw on. *)
  let draw_vertices g canvas = G.fold_vertex vs_fold g canvas

  (** Paint the edges of a given graph, in the given context.
      @param g      The graph to paint.
      @param canvas The canvas to draw on. *)
  let draw_edges g canvas =
    if G.is_directed
    then G.fold_edges_e es_fold_directed   g canvas
    else G.fold_edges_e es_fold_undirected g canvas

  (** Paints a graph.
      @param w The width of the image.
      @param h The height of the image.
      @param g The graph to draw. *)
  let draw_image = function (w,h,g) as vimage ->
    let canvas = draw_edges    g (init vimage) in
    let canvas = draw_vertices g  canvas  in
      canvas

end

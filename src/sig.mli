(**************************************************************************)
(*                                                                        *)
(*  Ocamlgv: a native graph visualization library for OCaml               *)
(*  Copyright (C) 2008, 2009  Alex Leighton                               *)
(*                                                                        *)
(*  The signatures for both G and I are Copyright (C) 2004-2008           *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
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

(** Common signatures used throughout Ocamlgv. *)

open Graph.Sig

(** Labeler signature for string conversion. Used to turn vertices and edges
    into strings which get drawn. *)
module type Labeler = sig
  type v
  type e
  val string_of_vertex : v -> string
  val string_of_edge : e -> string
end

(** Common signature for all graphs. Copied from Graph.Sig.G *)
module type G = sig

  (** {2 Graph structure} *)

  (** Abstract type of graphs *)
  type t

  (** Vertices have type [V.t] and are labeled with type [V.label]
      (note that an implementation may identify the vertex with its
      label) *)
  module V : VERTEX
  type vertex = V.t

  (** Edges have type [E.t] and are labeled with type [E.label].
      [src] (resp. [dst]) returns the origin (resp. the destination) of a
      given edge. *)
  module E : EDGE with type vertex = vertex
  type edge = E.t

  (** Is this an implementation of directed graphs? *)
  val is_directed : bool

  (** {2 Size functions} *)

  val is_empty : t -> bool
  val nb_vertex : t -> int
  val nb_edges : t -> int

  (** Degree of a vertex *)

  val out_degree : t -> vertex -> int
    (** [out_degree g v] returns the out-degree of [v] in [g].
        @raise Invalid_argument if [v] is not in [g]. *)

  val in_degree : t -> vertex -> int
    (** [in_degree g v] returns the in-degree of [v] in [g].
        @raise Invalid_argument if [v] is not in [g]. *)

  (** {2 Membership functions} *)

  val mem_vertex : t -> vertex -> bool
  val mem_edge : t -> vertex -> vertex -> bool
  val mem_edge_e : t -> edge -> bool

  val find_edge : t -> vertex -> vertex -> edge
    (** [find_edge g v1 v2] returns the edge from [v1] to [v2] if it exists.
        Unspecified behaviour if [g] has several edges from [v1] to [v2].
        @raise Not_found if no such edge exists. *)

  (** {2 Successors and predecessors} *)

  val succ : t -> vertex -> vertex list
    (** [succ g v] returns the successors of [v] in [g].
        @raise Invalid_argument if [v] is not in [g]. *)

  val pred : t -> vertex -> vertex list
    (** [pred g v] returns the predecessors of [v] in [g].
        @raise Invalid_argument if [v] is not in [g]. *)

  (** Labeled edges going from/to a vertex *)

  val succ_e : t -> vertex -> edge list
    (** [succ_e g v] returns the edges going from [v] in [g].
        @raise Invalid_argument if [v] is not in [g]. *)

  val pred_e : t -> vertex -> edge list
    (** [pred_e g v] returns the edges going to [v] in [g].
        @raise Invalid_argument if [v] is not in [g]. *)

  (** {2 Graph iterators} *)

  val iter_vertex : (vertex -> unit) -> t -> unit
    (** Iter on all vertices of a graph. *)

  val fold_vertex : (vertex -> 'a -> 'a) -> t  -> 'a -> 'a
    (** Fold on all vertices of a graph. *)

  val iter_edges : (vertex -> vertex -> unit) -> t -> unit
    (** Iter on all edges of a graph. Edge label is ignored. *)

  val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
    (** Fold on all edges of a graph. Edge label is ignored. *)

  val iter_edges_e : (edge -> unit) -> t -> unit
    (** Iter on all edges of a graph. *)

  val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
    (** Fold on all edges of a graph. *)

  val map_vertex : (vertex -> vertex) -> t -> t
    (** Map on all vertices of a graph. *)

  (** {2 Vertex iterators}

      Each iterator [iterator f v g] iters [f] to the successors/predecessors
      of [v] in the graph [g] and raises [Invalid_argument] if [v] is not in
      [g].

      it is the same for functions [fold_*] which use an additional
      accumulator. *)

  (** iter/fold on all successors/predecessors of a vertex. *)

  val iter_succ : (vertex -> unit) -> t -> vertex -> unit
  val iter_pred : (vertex -> unit) -> t -> vertex -> unit
  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

  (** iter/fold on all edges going from/to a vertex. *)

  val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
  val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
  val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

end

(** Signature for imperative (i.e. mutable) graphs. Copied from Graph.Sig.I*)
module type I = sig

  include G
    (** An imperative graph is a graph. *)

  val create : ?size:int -> unit -> t
    (** [create ()] returns an empty graph. Optionally, a size can be
        given, which should be on the order of the expected number of
        vertices that will be in the graph (for hash tables-based
        implementations).  The graph grows as needed, so [size] is
        just an initial guess. *)

  val copy : t -> t
    (** [copy g] returns a copy of [g]. Vertices and edges (and eventually
	marks, see module [Mark]) are duplicated. *)

  val add_vertex : t -> vertex -> unit
    (** [add_vertex g v] adds the vertex [v] from the graph [g].
	Do nothing if [v] is already in [g]. *)

  val remove_vertex : t -> vertex -> unit
    (** [remove g v] removes the vertex [v] from the graph [g]
	(and all the edges going from [v] in [g]).
	Do nothing if [v] is not in [g]. *)

  val add_edge : t -> vertex -> vertex -> unit
    (** [add_edge g v1 v2] adds an edge from the vertex [v1] to the vertex [v2]
	in the graph [g].
	Add also [v1] (resp. [v2]) in [g] if [v1] (resp. [v2]) is not in [g].
	Do nothing if this edge is already in [g]. *)

  val add_edge_e : t -> edge -> unit
    (** [add_edge_e g e] adds the edge [e] in the graph [g].
	Add also [E.src e] (resp. [E.dst e]) in [g] if [E.src e] (resp. [E.dst
	e]) is not in [g].
	Do nothing if [e] is already in [g]. *)

  val remove_edge : t -> vertex -> vertex -> unit
    (** [remove_edge g v1 v2] removes the edge going from [v1] to [v2] from the
	graph [g]. If the graph is labelled, all the edges going from [v1] to
	[v2] are removed from [g].
	Do nothing if this edge is not in [g].
	@raise Invalid_argument if [v1] or [v2] are not in [g]. *)

  val remove_edge_e : t -> edge -> unit
    (** [remove_edge_e g e] removes the edge [e] from the graph [g].
	Do nothing if [e] is not in [g].
	@raise Invalid_argument if [E.src e] or [E.dst e] are not in [g]. *)

end

(** {2 Common Types} *)

(** Point type. Requires float values for x and y. *)
type point = Point of float*float

(** Color type. Requires three float values for the rgb values. The range is
    [0 <= r,g,b <= 1]. *)
type color = Color of float*float*float

(** Label type. *)
type label = string

(** {2 Virtual Images} *)

module type VirtualImageGraph = sig
  include G with type V.label = (int*Vector.t*label*bool)
            and  type E.label = (Vector.t list*bool*label)

end

module type ImperativeVirtualImageGraph = sig
  include I with type V.label = (int*Vector.t*label*bool)
            and  type E.label = (Vector.t list*bool*label)
end

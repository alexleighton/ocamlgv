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

(** A Windowing module for interactive graph visualization. *)

open Graphics
open Windowtools
open Vector

(** Functor used to create an interactive graph visualization. *)
module WindowInteraction (G : Sig.ImperativeVirtualImageGraph) =
struct

  (** Window drawer. Contains the necessary functions to draw in a window,
      using the AbstractDrawer functor. *)
  module WindowDrawer (G : Sig.VirtualImageGraph) =
  struct

    include Graphtools.GraphAccess(G)

    type canvas = int * int
    type graph = G.t
    type edge = G.E.t
    type vertex = G.V.t
    type vimage = int * int * graph

    let init (w,h,g) =
      Graphics.set_window_title "OCamlgv graph visualization";
      Graphics.resize_window w h;
      Graphics.set_text_size 12;
      w,h

    let es_fold_undirected e (w,h) =
      let plist = List.map (Algorithms.translate_point h) (get_pointlist e) in
        if is_curvy e then curvelist plist else linelist plist ;
        w,h

    let es_fold_directed e (w,h) =
      let plist = List.map (Algorithms.translate_point h) (get_pointlist e) in
        if is_curvy e then arrow_line plist else arrow_line plist ;
        w,h

    let vs_fold v (w,h) =
      if is_dummy v then w,h
      else
        let vect,lbl = (get_center v),(get_label v) in
        let vect = Algorithms.translate_point h vect in
        let strx,stry,strw,strh = get_textloc lbl vect in
        let rx,ry = (((float strw)/.2.0)+.5.0),(((float strh)/.2.0)+.5.0) in
          fillrect (int (vect.x-.rx)) (int (vect.y-.ry)) (strw+10) (strh+10);
          Graphics.moveto strx stry;
          Graphics.draw_string lbl;
          w,h

  end

  module DrawGraph = AbstractDrawer.Make(G)(WindowDrawer(G))

  include Graphtools.GraphAccess(G)

  exception Found of G.V.t

  let dist (x1,y1) (x2,y2) = sqrt( (x1-.x2)**2.0 +. (y1-.y2)**2.0 )

  let get_one_vertex g =
    let aux v = raise (Found v) in
      try  G.iter_vertex aux g; None
      with Found v -> Some v

  let get_closest g x y =
    match (get_one_vertex g) with
        Some v ->
          let aux v (best,d) =
            let vect = get_center v in
            let dist = (dist (x,y) (vect.x,vect.y)) in
              if d > dist then v,dist else best,d
          in
          let v,d = G.fold_vertex aux g (v,infinity) in Some v
      | None -> None

  (** Returns true if the point x,y is in the bounding box of v.
      @param v The vertex to check if the point is within.
      @param x The x coordinate.
      @param y The y coordinate. *)
  let is_in v x y =
    let vect,lbl = (get_center v),(get_label v) in
    let strx,stry,strw,strh =
      get_textloc lbl vect in
    let halfw,halfh = (((float strw)/.2.0)+.5.0),(((float strh)/.2.0)+.5.0)
    in let strx,stry = (x-.halfw),(y-.halfh) in
      x > strx && x < (strx+.(float strw)) &&
        y > stry && y < (stry+.(float strh))

  type state = { selected: G.V.t option }

  let new_state = { selected = None }

  let get_selected s = s.selected
  let set_selected s v = { selected = v }

  let was_key_pressed status = status.keypressed
  let was_mouse_pressed status = status.button

  let handle_key_press () = failwith "done"

  let rec change_last a = function
      []      -> []
    | b::[]   -> a::[]
    | c::rest -> c::(change_last a rest)

  let replace_vertex g oldv newv =
    let vect = get_center newv in
    let succs = G.succ_e g oldv in
    let preds = if G.is_directed then G.pred_e g oldv else [] in
    let add_succ e =
      let plist,curvy,lbl = unpack_e e in
      let plist = vect::(List.tl plist) in
      let newe = G.E.create newv (plist,curvy,(get_edgelabel e)) (get_dest e)
      in
        G.add_edge_e g newe
    in
    let add_pred e =
      let plist,curvy,lbl = unpack_e e in
      let plist = change_last vect plist in
      let newe = G.E.create (get_src e) (plist,curvy,(get_edgelabel e)) newv
      in
        G.add_edge_e g newe
    in
      List.iter add_succ succs;
      List.iter add_pred preds

  let handle_mouse_pressed g status state =
    if was_mouse_pressed status
    then
      let x,y = (float status.mouse_x),(float status.mouse_y) in
        match get_closest g x y with
            Some v ->
              if is_in v x y
              then set_selected state (Some v)
              else state
          | None -> state
    else
      let x,y = (float status.mouse_x),(float status.mouse_y) in
        match get_selected state with
            Some v ->
              let (id,_,lbl,dummy) = unpack_v v in
              let newv = G.V.create (id,(vector x y),lbl,dummy) in
                replace_vertex g v newv;
                set_selected state None
          | None -> state

  let handle_events g status state =
    if was_key_pressed status
    then handle_key_press ()
    else handle_mouse_pressed g status state

  let events = [Graphics.Button_down;Graphics.Button_up;Graphics.Key_pressed]

  let interactive (w,h,g) =
    try
      Graphics.open_graph "";
      let state = ref new_state in
      while true do
        ignore (DrawGraph.draw_image (w,h,g));
        let status = Graphics.wait_next_event events in
          state := handle_events g status !state
      done;
      g
    with
      | Failure "done" -> g
      | Graphic_failure msg -> g

(*  let display (w,h,g) =
    try
      Graphics.open_graph "";
      ignore(DrawGraph.draw_image (w,h,g));
      while not (Graphics.key_pressed () || Graphics.button_down ())
      do () done
    with Graphic_failure msg -> ()
*)

  exception Close

  let default_handler status =
    if status.keypressed then raise Close else ()

  let default_events = [Button_down; Button_up; Key_pressed]

  let display ?(handler=default_handler)
              ?(events=default_events)
              (w,h,g) =
    try
      Graphics.open_graph "";
      ignore(DrawGraph.draw_image (w,h,g));
      while true do
        handler (Graphics.wait_next_event events)
      done
    with Graphic_failure msg -> ()
      |  Close               -> ()

end

(** Module containing code to draw a Graphtools.VirtualImageGraph in a
    window. *)
module DrawGraph =   WindowInteraction(Graphtools.VirtualImageGraph)
module DrawDigraph = WindowInteraction(Graphtools.VirtualImageDigraph)

(*
(** Include [InteractiveWindow]. *)
include WindowInteraction(Graphtools.VirtualImageGraph)
*)

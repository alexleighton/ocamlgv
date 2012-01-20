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

open Arg
open Pack.Digraph
open Pack.Digraph.ForceLayout

(** Module that parses and executes the ocamlgv commandline specification. *)

(* Algorithm specs *)

(** Algorithm set by the user. *)
let algorithm = ref "determine"

(** Set algorithm specification for the Arg module. *)
let set_algorithm =
  "-algorithm",(Set_string algorithm)," set the algorithm to use, options: [tree, layered, force-directed]"

(** Set algorithm specification for the Arg module. *)
let set_algo =
  "-algo",(Set_string algorithm)," alias for -algorithm."

(**************************************************)
(* Force-directed Model Specs *)

(** Model set by the user. *)
let model = ref ForceLayout.default

let o v = function None -> v | Some v' -> v'

(** Set model specification for the Arg module. *)
let set_model ?(c=None) ?(d=None) ?(dt=None)
              ?(mi=None) ?(s=None) ?(sp=None) ?(sl=None) () =
  let m = !model in
  let s = match s with None -> m.seed | Some v -> Some v in
    model :=
  { coulomb=(o m.coulomb c); damping=(o m.damping d); dt=(o m.dt dt);
    max_iterations=(o m.max_iterations mi); seed=s;
    spring=(o m.spring sp); spring_length=(o m.spring_length sl) }

let print_model () =
  let m = !model in
  let s = match m.seed with None -> 0 | Some i -> i in
    Printf.printf "Coulomb: %f, Damping: %f, dt: %f, Iter: %d, Seed: %d, Spring: %f, Spring Length: %f\n" m.coulomb m.damping m.dt m.max_iterations s m.spring m.spring_length

(** Sets the spring length in the model. *)
let slength_setter s = set_model ~sl:(Some s) ()
(** Set springlength specification for the Arg module. *)
let set_springlength =
  "-slength",(Float slength_setter)," set the length of the spring at rest. Default: 50.0 [force-directed]"

(** Sets the spring force in the model. *)
let spring_setter s = set_model ~sp:(Some s) ()
(** Set spring force specification for the Arg module. *)
let set_spring =
  "-spring",(Float spring_setter)," set the power of the spring force. Default: 5.0 [force-directed]"

(** Sets the coulomb force in the model. *)
let coulomb_setter c = set_model ~c:(Some c) ()
(** Set coulomb force specification for the Arg module. *)
let set_coulombs =
  "-coulomb",(Float coulomb_setter)," set the power of the electrical force. Default: 1000.0 [force-directed]"

(** Sets the change in time in the model. *)
let dt_setter d = set_model ~dt:(Some d) ()
(** Set dt specification for the Arg module. *)
let set_dt =
  "-dt",(Float dt_setter)," set the change in time. Default: 0.4 [force-directed]"

(** Sets the damping in the model. *)
let damping_setter d = set_model ~d:(Some d) ()
(** Set damping specification for the Arg module. *)
let set_damping =
  "-damping",(Float damping_setter)," set the amount of force that occurs. Choose a value between 0 and 1 inclusive. Default: 0.15 [force-directed]"

(** Sets the seed of the random generator in the model. *)
let seed_setter s = set_model ~s:(Some s) ()
(** Set seed specification for the Arg module. *)
let set_seed =
  "-seed",(Int seed_setter)," set the random seed used. Default: 0 [force-directed]"

(** Sets the max number of iterations in the model. *)
let iter_setter i = set_model ~mi:(Some i) ()
(** Set iter specification for the Arg module. *)
let set_iter =
  "-iter",(Int iter_setter)," set the max number of iterations. Default: 110 [force-directed]"

(** Toggles animation in the model. *)
let animate = ref false
(** Set animation specification for the Arg module. *)
let set_animate =
  "-animate",(Set animate)," toggle animation. Default: off [force-directed]"

(** The width. *)
let width = ref 450.0
(** The height. *)
let height = ref 450.0
(** Set width specification for the Arg module. *)
let set_width =
  "-width",(Set_float width)," set the width of the layout. Default: 450.0 [force-directed]"
(** Set height specification for the Arg module. *)
let set_height =
  "-height",(Set_float height)," set the height of the layout. Default: 450.0 [force-directed]"

(**************************************************)
(* Layered layout specs *)

(** The width of each layer. *)
let w = ref 3
(** Set layer width specification for the Arg module. *)
let set_w =
  "-w",(Set_int w)," set the width of each layer. [layered]"

(* Tree layout specs *)

(** The change in x. *)
let dx = ref 50.0
(** Set dx specification for the Arg module. *)
let set_dx =
  "-dx",(Set_float dx)," set the change in x between vertices at each height. Default: 50.0 [tree]"

(** The change in y. *)
let dy = ref 50.0
(** Set dy specification for the Arg module. *)
let set_dy =
  "-dy",(Set_float dy)," set the change in y between heights of the tree. Default: 50.0 [tree]"

(** The root of the tree. *)
let root : (string option ref) = ref None
let setroot s = root := Some s
let set_root =
  "-root",(String setroot)," set the root of the tree. [tree]"

(* Miscellaneous functions *)

let label_image sfc w h lbl =
  let ctx = Cairotools.get_context sfc in
  let p = Vector.vector (w-.25.0) (h-.25.0) in
    Cairotools.draw_string ctx lbl p 10.0;
    sfc

let dosave = ref false
(** Save to file specification for the Arg module. *)
let set_save =
  "-save",(Set dosave)," save the given file to a .png file of the same name"

let save_to_file dotfile surface =
  if !dosave || !animate then
    let newf = (Filename.chop_extension dotfile) ^ ".png" in
      Cairotools.save_to_pngfile surface newf
  else ()

(* Apply the algorithm to a file. *)

(** Applies an algorithm to a dot file. *)
let apply_algorithm file =
  match !algorithm with
      "tree" ->
        let graph = parse_dot_file file in
        let root = match !root with
          | None -> get_root graph | Some v -> Some (find_vertex graph v) in
        let w,h,g = TreeLayout.layout graph root 50.0 !dx !dy in
        let surface = CairoDraw.DrawGraph.draw_image (w,h+50,g) in
          save_to_file file surface;
          WindowDraw.DrawGraph.display (w,h+50,g)
    | "layered" ->
        let graph = parse_dot_file file in
        let w,h,g = LayeredLayout.layout graph !w 200.0 50.0 !dx !dy in
        let surface = CairoDraw.DrawDigraph.draw_image (w+100,h+100,g) in
          save_to_file file surface;
          WindowDraw.DrawDigraph.display (w+100,h+100,g)
    | "force-directed" | "determine" ->
        let graph = parse_dot_file file in
        let saveimage w h i vimage =
          let s = CairoDraw.DrawGraph.draw_image vimage in
          let s = label_image s w h (string_of_int i) in
            save_to_file ("timestep" ^ (string_of_int i) ^ file) s;
            i+1
        in
          print_model ();
          if !animate then
            let gs = ForceLayout.layout_list graph ~constants:(!model) !width !height in
              ignore (List.fold_left (saveimage !width !height) 0 gs);
          else
            let w,h,g = ForceLayout.layout graph ~constants:(!model) !width !height in
            let surface = CairoDraw.DrawGraph.draw_image (w+50,h+50,g) in
              save_to_file file surface;
              WindowDraw.DrawGraph.display (w+50,h+50,g)
    | _ -> raise (Bad "invalid algorithm.")

(** Rest specification for the Arg module. *)
let rest = "--",(Rest apply_algorithm)," apply the current algorithm to each file that follows"

(** List of all the specifications. *)
let spec = [set_algorithm; set_algo;
            set_seed; set_iter; set_damping; set_dt;
            set_coulombs; set_spring; set_springlength;
            set_animate; set_width; set_height;
            set_w; set_root; set_dx; set_dy;
            set_save;
            rest]

let _ = parse (align spec) apply_algorithm
        "Display a given graph formatted in dot notation."

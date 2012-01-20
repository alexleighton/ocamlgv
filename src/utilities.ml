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

(** Utility module. Contains many miscellaneous utility functions. *)

let string_of_list f l = "[" ^ String.concat ", " (List.map f l) ^ "]"

let string_of_point (Sig.Point (x,y)) =
  "(" ^ (string_of_float x) ^ ","
  ^ (string_of_float y) ^ ")"

let string_of_pointlist plist =
  let return = ref "[" in
  let aux p = return := (!return ^ (string_of_point p) ^ ",") in
    List.iter aux plist ;
    (String.set !return ((String.length !return) - 1) ']') ;
    !return

let string_of_color (Sig.Color (r,g,b)) =
  "(" ^ (string_of_float r) ^ ","
  ^ (string_of_float g) ^ ","
  ^ (string_of_float b) ^ ")"

let white = Sig.Color(1.0,1.0,1.0)
let black = Sig.Color(0.0,0.0,0.0)
let red   = Sig.Color(1.0,0.0,0.0)
let blue  = Sig.Color(0.0,0.0,1.0)
let green = Sig.Color(0.0,1.0,0.0)

let debug = true

let report f file line msg =
  print_endline ("File " ^ file ^ ", line " ^ line ^ ": " ^ msg)

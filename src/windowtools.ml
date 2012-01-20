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

(** Utility module for drawing interactive graphs. *)

open Vector

(**/**)
let int = int_of_float
(**/**)

(** Draws a line through a list of vectors.
    @param vlist The list of vectors. *)
let linelist vlist =
  let aux v = Graphics.lineto (int v.x) (int v.y) in
    match vlist with
        (v::rest) ->
          Graphics.moveto (int v.x) (int v.y);
          Graphics.set_color Graphics.black;
          List.iter aux rest
      | _ -> ()

let arrow_line vlist =
  let line v = Graphics.lineto (int v.x) (int v.y) in
  let arrowpoint p q =
    let v = 10.0 *| (normalize (p -| q)) in
    let left = translate (rotate_d v (-.45.0)) q.x q.y in
    let right = translate (rotate_d v 45.0) q.x q.y in
      left,right
  in
    match vlist with
        [p;q] ->
          Graphics.moveto (int p.x) (int p.y);
          Graphics.set_color Graphics.black;
          line q;
(*          let q = shorten q 10.0 in *)
          let left,right = arrowpoint p q in
            Graphics.moveto (int q.x) (int q.y); line left;
            Graphics.moveto (int q.x) (int q.y); line right
    | _ -> failwith "arrow_line: not implemented."

(** Draws a curved line by taking the point list 4 points at a time.
    If p,p1,p2,p3 are the points considered, it draws a cubic Bezier spline
    from p to p3 with p1,p2 as control points. The list of points should look
    like [[p;cp1;cp2;p1;cp3;cp4;p2 ... pn]] where cp are control points.
    @param vlist The list of points. *)
let curvelist vlist =
  let aux v1 v2 v3 =
    Graphics.curveto ((int v1.x),(int v1.y))
                     ((int v2.x),(int v2.y))
                     ((int v3.x),(int v3.y))
  in
  let rec iter3 = function
    | [] -> ()
    | a::[] -> ()
    | a::b::[] -> ()
    | a::b::c::rest -> aux a b c; iter3 rest
  in
    match vlist with
        v::rest ->
          Graphics.moveto (int v.x) (int v.y);
          Graphics.set_color Graphics.black;
          iter3 rest
      | _ -> ()

(** Returns the width and height of a box enclosing the given text.
    @param str The string to get the width and height of. *)
let get_textbox str = Graphics.text_size str

(** Get the x,y location to draw the given string at, in order for it to
    be centered on the given point.
    @param str The string to work on.
    @param v   The center point of the string.
    @return (x,y,w,h) where x,y are the location to draw the string at and w,h are the width and height of the string if it were drawn. *)
let get_textloc str v =
  let w,h = Graphics.text_size str in
  let halfw,halfh = ((float w)/.2.0),((float h)/.2.0) in
    (int (v.x-.halfw)),(int (v.y-.halfh)),w,h

(** Fills a white circle, then draws a black-outlined circle on top.
    @param x The x coordinate of the center of the circle.
    @param y The y coordinate of the center of the circle.
    @param r The radius of the circle. *)
let fillcircle x y r =
  Graphics.set_color Graphics.white;
  Graphics.fill_circle x y r;
  Graphics.set_color Graphics.black;
  Graphics.draw_circle x y r

(** Fills a white ellipse, then draws a black-outlined ellipse on top.
    @param x  The x coordinate of the center of the ellipse.
    @param y  The y coordinate of the center of the ellipse.
    @param rx The radius in the x direction.
    @param ry The radius in the y direction. *)
let fillellipse x y rx ry =
  Graphics.set_color Graphics.white;
  Graphics.fill_ellipse x y rx ry;
  Graphics.set_color Graphics.black;
  Graphics.draw_ellipse x y rx ry

(** Fills a white rectangle, then draws a black-outlined rectangle on top.
    @param x The x coordinate of the lower-left corner of the rectangle.
    @param y The y coordinate of the lower-left corner of the rectangle.
    @param w The width of the rectangle.
    @param h The height of the rectangle. *)
let fillrect x y w h =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect x y w h;
  Graphics.set_color Graphics.black;
  Graphics.draw_rect x y w h

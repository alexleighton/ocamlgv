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

(** Utility module for drawing using the Cairo library *)

open Cairo
open Vector

(** The constant PI. *)
let pi = 3.1415926535897932

(** Saves the current context and then restores it after applying the
    given function. Requires a function that takes unit as a parameter.
    @param context The Cairo context to save and restore.
    @param f       The function to wrap with the save and restore context commands. *)
let wrap_context context f =
  Cairo.save context ;
  f ();
  Cairo.restore context

(** Saves the given surface to the given file in *.png format.
    Will overrite a currently existing file.
    @param surface The Cairo surface to save.
    @param file    The string that represents the file to save to. *)
let save_to_pngfile surface file =
  Cairo_png.surface_write_to_file surface file

(** Creates a surface given a width and height.
    @param width  The width of the surface as an int.
    @param height The height of the surface as an int.
    @return       The Cairo surface. *)
let create_surface width height =
  Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width ~height

(** Gets the Cairo drawing context from a given surface.
    @param surface The Cairo surface. *)
let get_context surface = Cairo.create surface

(** Sets the current color to paint with.
    @param context The Cairo context.
    @param color   The color to set to. *)
let set_color context (Sig.Color (r,g,b)) =
  Cairo.set_source_rgb context r g b

(** Sets the background color to the given color.
    @param context The Cairo context.
    @param color   The color to set the background. *)
let fill_surface context (Sig.Color (r,g,b)) =
  wrap_context context
    (fun () ->
       Cairo.set_source_rgb context ~red:r ~green:g ~blue:b ;
       Cairo.paint context )

(**/**)
let linelist_aux context pointlist width close =
  let draw_line context v1 v2 =
    Cairo.line_to context v1.x v1.y;
    Cairo.line_to context v2.x v2.y;
    v2
  in
    Cairo.set_line_width context width ;
    match pointlist with
      | [] -> ()
      | v::rest ->
	  Cairo.move_to context v.x v.y ;
	  ignore (List.fold_left (draw_line context) v rest) ;
          (if (close) then Cairo.close_path context else ()) ;
	  Cairo.stroke context

(**/**)

(** Draws a polygon by drawing a straight line through each point in the list
    and then connecting the first and last point.
    @param context   The Cairo context.
    @param pointlist The list of points
    @param width     The width of the line. *)
let polygon context pointlist width =
  wrap_context context
    (fun () -> linelist_aux context pointlist width true )

(** Draws a straight line through each of the points in the list.
    @param context   The Cairo context.
    @param pointlist The list of points.
    @param width     The width of the line. *)
let straight_line context pointlist width =
  wrap_context context
    (fun () -> linelist_aux context pointlist width false )

(** Draws a straight line with an arrow on one end.
    @param context   The Cairo context.
    @param pointlist The list of points.
    @param width     The width of the line. *)
let straight_arrow_line context pointlist width =
  let line v = Cairo.line_to context v.x v.y in
  let arrowpoint p q =
    let v = 10.0 *| (normalize (p -| q)) in
    let left = translate (rotate_d v (-.45.0)) q.x q.y in
    let right = translate (rotate_d v 45.0) q.x q.y in
      left,right
  in
  match pointlist with
      [p;q] -> wrap_context context
        (fun () ->
           Cairo.move_to context p.x p.y ;
           Cairo.set_line_width context width ;
           set_color context Utilities.black;
           line q;
           let left,right = arrowpoint p q in
             Cairo.move_to context q.x q.y; line left;
             Cairo.move_to context q.x q.y; line right;
             Cairo.stroke context)
    | _ -> failwith "straight_arrow_line: not implemented."

(** Draws a point at a given location with a given radius.
    @param context The Cairo context.
    @param v       The center of the point.
    @param radius  The radius of the point. *)
let point context v radius =
  Cairo.arc context v.x v.y radius 0.0 (2.0*.pi) ;
  Cairo.fill context

(** Draws a curved line by taking the point list 4 points at a time.
    If p,p1,p2,p3 are the points considered, it draws a cubic Bezier spline
    from p to p3 with p1,p2 as control points. The list of points should look
    like [[p;cp1;cp2;p1;cp3;cp4;p2 ... pn]] where cp are control points.
    @param context   The Cairo context.
    @param pointlist The list of points.
    @param width     The width of the line. *)
let curved_line context pointlist width =
  let aux v v1 v2 v3 =
    Cairo.line_to context v.x v.y ;
    Cairo.curve_to context v1.x v1.y v2.x v2.y v3.x v3.y
  in
  let rec iter4 = function
    | [] -> ()
    | a::[] -> ()
    | a::b::[] -> ()
    | a::b::c::[] -> ()
    | a::b::c::d::rest -> aux a b c d; iter4 (d::rest)
  in
    Cairo.set_line_width context width ;
    match pointlist with
      | [] -> ()
      | v::rest ->
	  Cairo.move_to context v.x v.y ;
	  iter4 (v::rest) ;
	  Cairo.stroke context
;;

(** Draws a rectangle.
    @param context   The Cairo context.
    @param v         The center point of the rectangle.
    @param fillcolor The color of the inside of the rectangle.
    @param edgecolor The color of the edges.
    @param width     The width of the rectangle.
    @param height    The height of the rectangle.
    @param linewidth The width of edge. *)
let rectangle context v fillcolor edgecolor width height linewidth =
  let ulx,uly = (v.x -. (width /. 2.0)),(v.y -. (height /. 2.0)) in
  let urx,ury = (ulx +. width),uly in
  let brx,bry = urx,(ury +. height) in
  let blx,bly = ulx,bry in
    Cairo.set_line_width context linewidth ;
    wrap_context context
      (fun () ->
         Cairo.move_to context ulx uly ;
         Cairo.line_to context urx ury ;
         Cairo.line_to context brx bry ;
         Cairo.line_to context blx bly ;
         Cairo.close_path context ;
         set_color context fillcolor ;
         Cairo.fill_preserve context ;
         set_color context edgecolor ;
         Cairo.stroke context )

(** Draws a rectangle with a black edge and white interior. Convenience
    function.
    @param context   The Cairo context.
    @param ctrpoint  The center point of the rectangle.
    @param width     The width of the rectangle.
    @param height    The height of the rectangle.
    @param linewidth The width of the edge. *)
let empty_rectangle context ctrpoint width height linewidth =
  rectangle context ctrpoint
            Utilities.white Utilities.black
            width height linewidth

(** Draws a rectangle with a black edge and an interior of a given color.
    Convenience function.
    @param context   The Cairo context.
    @param ctrpoint  The center point of the rectangle.
    @param color     The color of the interior.
    @param width     The width of the rectangle.
    @param height    The height of the rectangle.
    @param linewidth The width of the edge. *)
let fillrectangle context ctrpoint color width height linewidth =
  rectangle context ctrpoint color Utilities.black width height linewidth

(** Returns a tuple containing x,y,width,height of the given string,
    were it to be rendered in the given context.
    @param context The Cairo context.
    @param s       The string to get the bounding box of.
    @return        The bounding box of the given string. *)
let get_textbox context s =
  let ext = Cairo.text_extents context s in
    (ext.x_bearing+.10.0),(ext.y_bearing+.5.0),
  (ext.text_width+.25.0),(ext.text_height+.20.0)

(** Draws a given string at a given centerpoint.
    @param context  The Cairo context.
    @param s        The string to draw.
    @param v        The center of the string.
    @param fontsize The size of the font of the string. *)
let draw_string context s v fontsize =
  wrap_context context
    (fun () ->
       Cairo.select_font_face context "Sans"
         Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL ;
       let ext = Cairo.text_extents context s in
       let cx = v.x -. (ext.text_width /. 2.0 +. ext.x_bearing) -. 5.0 in
       let cy = v.y -. (ext.text_height /.2.0 +. ext.y_bearing) in
         Cairo.set_font_size context fontsize ;
         Cairo.move_to context cx cy ;
         Cairo.show_text context s)

(*
  let oval context point w h width =
  match point with Sig.Point(x,y) ->
  Cairo.set_line_width context width ;
  Cairo.save context ;
  Cairo.scale context ~sx:w ~sy:1.0 ;
  Cairo.arc context x y h 0.0 (2.0*.pi) ;
  Cairo.restore context ;
  Cairo.stroke context
*)

(** Draws a rectangle with rounded edges.
    Requires a point at the upper-left corner of the rectangle.
    @param context The Cairo context.
    @param v       The upper-left corner of the rectangle.
    @param w       The width of the rectangle.
    @param h       The height of the rectangle.
    @param r       The radius of the circle sitting at the corners.
    @param width   The width of the line to draw. *)
let rounded_rectangle context v w h r width =
  Cairo.set_line_width context width ;
  let xw = v.x+.w in
  let yh = v.y+.h in
    wrap_context context
      (fun () ->
	 Cairo.move_to    context (v.x+.r) v.y ;
	 Cairo.line_to    context (xw-.r) v.y ;
	 Cairo.curve_to   context xw v.y xw v.y xw (v.y+.r) ;
	 Cairo.line_to    context xw (yh-.r) ;
	 Cairo.curve_to   context xw yh xw yh (xw-.r) yh ;
	 Cairo.line_to    context (v.x+.r) yh ;
	 Cairo.curve_to   context v.x yh v.x yh v.x (yh-.r) ;
	 Cairo.line_to    context v.x (v.y+.r) ;
	 Cairo.curve_to   context v.x v.y v.x v.y (v.x+.r) v.y ;
	 Cairo.close_path context ; Cairo.stroke context )

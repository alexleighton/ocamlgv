open Vector

let sprintf = Printf.sprintf

let iso_time () =
  if Sys.os_type = "Win32" then "unsupported"
  else let tm = Unix.gmtime (Unix.time ()) in
    sprintf "%04d%02d%02dT%02d%02d%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let header xmax ymax =
  let date = iso_time () in
    sprintf "%%!PS-Adobe-3.0 EPSF-3.0\n%%%%Creator: ocamlgv\n%%%%Title: Graph Drawing\n%%%%CreationDate: %s\n%%%%DocumentData: Clean7Bit\n%%%%BoundingBox: 0 0 %d %d\n%%%%EndComments\n" date xmax ymax

let footer = "showpage\n%%EOF"

let save_to_ps_file buffer file =
  Buffer.add_string buffer footer;
  Buffer.output_buffer (open_out file) buffer

let set_color (Sig.Color (r,g,b)) =
  sprintf "%f %f %f setrgbcolor\n" r g b

let linelist pointlist width close =
  let line str v = sprintf "%s%f %f lineto\n" str v.x v.y in
  let linewidth = sprintf "%f setlinewidth\n" width in
  let close = if close then "closepath" else "" in
    match pointlist with
      | [] -> ""
      | v::rest ->
          sprintf "newpath\n%s%f %f moveto\n%s%sstroke\n"
            linewidth v.x v.y (List.fold_left line "" rest) close

let polygon plist width = linelist plist width true

let straight_line plist width = linelist plist width false

let curved_line = straight_line

let straight_arrow_line = straight_line

(** Draws a rectangle.
    @param v         The center point of the rectangle.
    @param fillcolor The color of the inside of the rectangle.
    @param edgecolor The color of the edges.
    @param width     The width of the rectangle.
    @param height    The height of the rectangle.
    @param linewidth The width of edge. *)
let rectangle v fillcolor edgecolor width height linewidth =
  let ulx,uly = (v.x -. (width /. 2.0)),(v.y -. (height /. 2.0)) in
  let urx,ury = (ulx +. width),uly in
  let brx,bry = urx,(ury +. height) in
  let blx,bly = ulx,bry in
  let line x y = sprintf "%f %f lineto\n" x y in
    "newpath\n" ^
    sprintf "%f setlinewidth\n" linewidth ^
    sprintf "%f %f moveto\n" ulx uly ^
    line urx ury ^ line brx bry ^ line blx bly ^
    "closepath\n" ^
    (sprintf "gsave\n%sfill\ngrestore\n" (set_color fillcolor)) ^
    set_color edgecolor ^
    "stroke\n"

let empty_rectangle v width height linewidth =
  rectangle v Utilities.white Utilities.black width height linewidth

let draw_string s v fontsize =
  sprintf "/Times-Roman findfont\n%d scalefont\nsetfont\nnewpath\n%f %f moveto\n(%s) show\n"
    fontsize v.x v.y s

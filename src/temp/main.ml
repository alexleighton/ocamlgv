open Pack.Graph

(*
let _ =
  let graph = parse_tree_file "presentation/test.dot" in
  let w,h,g = ForceLayout.design graph 500.0 500.0 in
  let image = PsDraw.DrawGraph.draw_image (w,h,g) in
    Pstools.save_to_ps_file image "test.ps" *)
(*  let str = Algorithms.GAccess.string_of_graph g in *)
(*    print_endline str; *)
(*    WindowDraw.DrawGraph.display (w,h,g) *)

(*  let vimage = SpringModel.design graph 500.0 500.0 in *)
      (*    WindowDraw.DrawGraph.display vimage *)
(*  let gs = SpringModel.design_list graph 500.0 500.0 in
    List.fold_left display 0 gs *)

(*
  module Bounds = Graphtools.FindBounds(Algorithms.Digraph)
  let w,h = Bounds.window (Bounds.bounds g) in
  let int = int_of_float
let _ =
  (* let root = get_root graph in *)
  (* let w,h,g = TreeDrawing.design graph root 200.0 25.0 100.0 40.0 in *)
  let w,h,g = 500,500,(LayeredDigraph.design graph 3 50.0 50.0 50.0 50.0) in
    WindowDraw.InteractiveWindowDigraph.display (w,h,g)
*)

(*
  let string_of_list l f = String.concat "," (List.map f l)

  let _ =
  let g = SpringModel.design graph 500.0 500.0 in
  let surface = CairoDraw.DrawDigraph.draw_image (500,500,g) in
  Cairotools.save_to_pngfile surface img1 *)

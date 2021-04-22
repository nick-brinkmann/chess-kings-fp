open Game ;;
module G = Graphics ;;
module T = Registry ;;
module R = T.Registry ;;

let cX_DIMENSION = 100 ;;
let cY_DIMENSION = 100 ;;
let cPIXELS_PER_BLOCK = 8 ;;

let cSQUARE_WIDTH = (cX_DIMENSION*cPIXELS_PER_BLOCK) / 8 ;;
let cSQUARE_HEIGHT = (cY_DIMENSION * cPIXELS_PER_BLOCK) / 8 ;;

let cLINEWIDTH = 3 ;;

let cWHITECOLOR = G.white ;;
let cBLACKCOLOR = G.black ;;


let quit () =
  if G.read_key () = 'q' then true
  else false
;;


let light_up () = 
  let s = G.wait_next_event [G.Key_pressed] in
  G.moveto 550 550;
  G.set_color G.red;
  G.draw_string (String.make 1 s.key);; 
  

(* initialize () -- Establishes the graphics window and sets its
   properties.*)
let initialize () =
  (* open a graphics window to draw into and size it appropriately *)
  G.open_graph "";
  (* resize the window to a 1x1 aspect ratio, only printing
      the board. Might change to add moves history later *)
  G.resize_window (cX_DIMENSION * cPIXELS_PER_BLOCK)
                  (cY_DIMENSION * cPIXELS_PER_BLOCK);
  (* turn off auto-synchronizing; we'll handle double buffer
      synchronization ourselves *)
  G.set_window_title "Chess51";
  G.auto_synchronize false;
  G.display_mode true ;;

(* Drawing simple shapes*)
let draw_square (c : G.color) (y : int) (x : int) (w : int) (h : int) : unit =
  G.set_color c;
  G.fill_rect (x * w) ((cY_DIMENSION * cPIXELS_PER_BLOCK) - h - y * h) w h ;;

(* Draws the map for a given maze. *)
let draw_board (board : int array array)
              (elt_width : int) (elt_height : int)
            : unit =
  G.set_line_width cLINEWIDTH;
  Array.iteri (fun y m -> 
                Array.iteri (fun x _n -> 
                              if (x + y) mod 2 = 0 then draw_square cWHITECOLOR y x elt_width elt_height
                              else draw_square cBLACKCOLOR y x elt_width elt_height
                            ) m) board ;;

let render_pieces pieces =
  pieces
  |> List.iter (fun piece -> piece#draw) ;;

let render pieces 
           board 
           elt_width 
           elt_height = 
  G.clear_graph ();
  draw_board board elt_width elt_height;
  render_pieces pieces ;
  light_up ();
  (* G.moveto 250 250;
  G.set_color G.red;
  G.draw_string "Pawn"; *)
  G.synchronize () 
;;
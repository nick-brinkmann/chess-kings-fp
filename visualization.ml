open Game ;;
module G = Graphics ;;

let cX_DIMENSION = 200 ;;
let cY_DIMENSION = 200 ;;
let cPIXELS_PER_BLOCK = 3 ;;

let cLINEWIDTH = 3 ;;

let cWHITECOLOR = G.white ;;
let cBLACKCOLOR = G.black ;;

let any_key () =
  if G.key_pressed () then true
  else false ;;

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
  G.auto_synchronize false;
  G.display_mode false ;;

(* Drawing simple shapes*)
let draw_square (c : G.color) (y : int) (x : int) (w : int) (h : int) : unit =
  G.set_color c;
  G.fill_rect (x * w) (cFRAMESIZE - h - y * h) w h ;;

(* Draws the map for a given maze. *)
let draw_board (board : square array array)
              (elt_width : int) (elt_height : int)
            : unit =
  G.set_line_width cLINEWIDTH;
  Array.iteri (fun y m -> 
                Array.iteri (fun x n -> 
                              if (x + y) mod 2 = 0 then draw_square cWHITECOLOR y x elt_width elt_height
                              else draw_square cBLACKCOLOR y x elt_width elt_height
                            ) m) board ;;
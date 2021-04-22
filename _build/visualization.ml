open Game ;;
open Params ;;
module G = Graphics ;;
module T = Registry ;;
module R = T.Registry ;;


let move_on () =
  if G.read_key () = 'q' then false
  else true
;;
  

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


let get_bottom_left (x : int) (y : int) : int * int = 
  let (new_x, new_y) = (x - (x mod cSQUARE_WIDTH),
                          y - (y mod cSQUARE_HEIGHT)) in
  new_x, new_y ;;

let print_coords () = 
  let s = G.wait_next_event [G.Button_down] in
  let x = s.mouse_x in
  let y = s.mouse_y in
  let (corner_x, corner_y) = get_bottom_left x y in
  (* G.moveto x y ; *)
  (* G.set_color G.red; *)
  (* G.draw_string ((string_of_int x) ^ ", " ^ (string_of_int y)) *)
  G.set_color G.red;
  G.fill_rect corner_x corner_y cSQUARE_WIDTH cSQUARE_HEIGHT
;; 


(*  Takes list of all pieces and draws them on board *)
let render_pieces pieces =
  pieces
  |> List.iter (fun piece -> piece#draw) ;;

(* Renders the game, waits for next move *)
let render pieces board = 
  G.clear_graph ();
  draw_board board cSQUARE_WIDTH cSQUARE_HEIGHT;
  render_pieces pieces ;

  (* At this point we need functions for logic of game
      -- Await move = wait for some type of input
                      whether a click, string, etc.
      -- Make move = when a valid input is given check
                    that move can be made and then 
                    modify piece value or not *)
  print_coords ();

  G.synchronize () 
;;
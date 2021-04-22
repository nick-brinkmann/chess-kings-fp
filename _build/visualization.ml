open Game ;;
open Params ;;
module G = Graphics ;;
module T = Registry ;;
module R = T.Registry ;;
module P = Pieces ;;


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


let get_coords (x : int) (y : int) : coordinate = 
  let reduce_x = x - (x mod cSQUARE_WIDTH) in
  let reduce_y = y - (y mod cSQUARE_HEIGHT) in
  let f = (int_to_file ((reduce_x / cSQUARE_WIDTH) + 1)) in
  let r = (int_to_rank ((reduce_y / cSQUARE_HEIGHT) + 1)) in
  f, r
;;


let print_coords () = 
  let s = G.wait_next_event [G.Button_down] in
  let x = s.mouse_x in
  let y = s.mouse_y in
  let (f, r) = get_coords x y in
  G.set_color G.magenta;
  G.moveto x y;
  (* G.draw_string ((file_to_string f) ^ (rank_to_string r)) *)
  if not (R.find_piece (f, r)) then 
    (G.moveto x y;
    G.set_color G.red;
    G.draw_string "No piece here")
  else
    (G.set_color G.red;
    G.fill_rect x y cSQUARE_WIDTH cSQUARE_HEIGHT)
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

  (* TODO: Write function to get chess coordinate
      and then use set.find or set.filter to retrieve
      piece at this square or None if none is there *)
  print_coords ();

  G.synchronize () 
;;
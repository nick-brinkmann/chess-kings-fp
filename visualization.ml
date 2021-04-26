open Game ;;
open Params ;;
module G = Graphics ;;
module T = Registry ;;
module R = T.Registry ;;
module P = Pieces ;;


let move_on () =
  if G.read_key () = 'q' then false
  else true
  (* let s = G.wait_next_event [G.Poll] in
  if s.key = 'q' then
    false
  else 
    true *)
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


(* Draws the chess board *)
let draw_board (board : (T.piece_type option) array array) : unit =
  G.set_line_width cLINEWIDTH;
  Array.iteri (fun y m -> 
                Array.iteri (fun x piece_opt ->
                              if (x + y) mod 2 = 0 then draw_square cWHITECOLOR y x cSQUARE_WIDTH cSQUARE_HEIGHT
                              else draw_square cBLACKCOLOR y x cSQUARE_WIDTH cSQUARE_HEIGHT
                              (* match piece_opt with
                              | Some piece -> piece#draw
                              | None -> () *)
                            ) m) board ;
  R.get_pieces ()
  |> List.iter (fun piece -> piece#draw) ;;


(* Given the chess coordinates of a specific squares, outlines that square in
      a different color to indicate selection *)
let highlight_square (c : coordinate) : unit =
  let (x, y) = coord_to_int c in
  let (corner_x, corner_y) = (cSQUARE_WIDTH * x, cSQUARE_HEIGHT * y) in
  G.set_color G.magenta;
  G.draw_rect corner_x corner_y cSQUARE_WIDTH cSQUARE_HEIGHT
;;


(* Given (x, y) coordiantes in the graphics window, converts them 
    into (file, rank) chess coordinates *)
let get_coords (x : int) (y : int) : coordinate = 
  let reduce_x = x - (x mod cSQUARE_WIDTH) in
  let reduce_y = y - (y mod cSQUARE_HEIGHT) in
  let f = (int_to_file ((reduce_x / cSQUARE_WIDTH))) in
  let r = (int_to_rank ((reduce_y / cSQUARE_HEIGHT))) in
  f, r
;;


let move_piece (piece : T.piece_type) = 
  let s = G.wait_next_event [G.Button_down] in
  let x = s.mouse_x in
  let y = s.mouse_y in
  let (f, r) = get_coords x y in
  if not (piece#can_be_valid_move (f, r)) then
    (G.set_color G.red;
    G.draw_string "Cannot move here";)
  else
    (piece#make_move (f, r);
    G.clear_graph ();
    draw_board R.get_position)
;;


let rec pick_piece () = 
  let s = G.wait_next_event [G.Button_down] in
  let x = s.mouse_x in
  let y = s.mouse_y in
  let (f, r) = get_coords x y in
  G.set_color G.magenta;
  G.moveto x y;
  let p = R.find_piece (f, r) in
  match p with
  | None -> (G.moveto x y;
            G.set_color G.red;
            G.draw_string "No piece here";
            pick_piece ())
  | Some piece -> (highlight_square piece#get_pos;
                  move_piece piece)
;;


(*  Takes list of all pieces and draws them on board *)
(* let render_pieces pieces =
  pieces
  |> List.iter (fun piece -> piece#draw) ;; *)

let print_board (pos : (T.piece_type option) array array) : unit =
  Array.iteri (fun y m -> 
                Array.iteri (fun x piece_opt -> 
                      match piece_opt with
                      | None -> Printf.printf "O"
                      | Some _piece -> Printf.printf "X") m;
                Printf.printf "\n") pos 
;;

(* Renders the game, waits for next move *)
let render board = 
  G.clear_graph ();
  draw_board board;

  (* render_pieces pieces ; *)

  (* At this point we need functions for logic of game
      -- Await move = wait for some type of input
                      whether a click, string, etc.
      -- Make move = when a valid input is given check
                    that move can be made and then 
                    modify piece value or not *)

  (* TODO: Write function to get chess coordinate
      and then use set.find or set.filter to retrieve
      piece at this square or None if none is there *)
  pick_piece ();

  G.synchronize () 
;;
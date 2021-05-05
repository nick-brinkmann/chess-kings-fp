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
  G.fill_rect (x * w)  (y * h) w h ;;
(* ((cY_BOARDSIZE * cPIXELS_PER_BLOCK) - h - y * h) *)
(* Given the chess coordinates of a specific squares, outlines that square in
a different color to indicate selection *)
let highlight_square (c : coordinate) : unit =
  let (x, y) = coord_to_int c in
  let (corner_x, corner_y) = (cSQUARE_WIDTH * x, cSQUARE_HEIGHT * y) in
  G.set_color cSELECTED_COLOR;
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
  
(* Draws the chess board *)
let draw_board () : unit =
  G.set_line_width cLINEWIDTH;
  for x = 0 to 7 do
    for y = 0 to 7 do
      if (x + y) mod 2 = 1 then draw_square cWHITECOLOR y x cSQUARE_WIDTH cSQUARE_HEIGHT
      else draw_square cBLACKCOLOR y x cSQUARE_WIDTH cSQUARE_HEIGHT
    done;
  done;

  (* draw file coordinates *)
  G.set_color G.black;
  for f = 0 to 7 do
    G.moveto (cSQUARE_WIDTH * f + (cSQUARE_WIDTH / 2)) (cY_BOARDSIZE * cPIXELS_PER_BLOCK + 5);
    let fi = int_to_file f in
    G.draw_string (file_to_string fi);
  done;
  (* draw rank coordinates *)
  for r = 0 to 7 do
    G.moveto (cX_BOARDSIZE * cPIXELS_PER_BLOCK + 5) (cSQUARE_HEIGHT * r + (cSQUARE_HEIGHT / 2));
    let ra = int_to_rank r in
    G.draw_string (rank_to_string ra);
  done;
(* draw take back button *)
  let x, y = 9, 2 in
  draw_square cUNDO_BACKGROUND y x cSQUARE_WIDTH cSQUARE_HEIGHT;
  G.set_color cUNDO_TEXT_COLOR;
  G.moveto ((x * cSQUARE_WIDTH) + (cSQUARE_WIDTH / 2)) ((y*cSQUARE_HEIGHT) + (cSQUARE_HEIGHT / 2));
  G.draw_string "Take";
  G.moveto ((x * cSQUARE_WIDTH) + (cSQUARE_WIDTH / 2)) ((y*cSQUARE_HEIGHT) + (cSQUARE_HEIGHT / 2) - 15);
  G.draw_string "Back";
  (* draw pieces on board *)
  R.get_pieces ()
  |> List.iter (fun piece -> piece#draw) ;;




(* take_turn : Waits for the execution of a move which involves:
        - selecting one of your own pieces (can change as many times as you want)
        - selecting a valid square to which you move selected piece *)
let take_turn () =
  let moved = ref false in
  let selected : T.piece_type option ref = ref None in
  
  let move_piece (piece : T.piece_type) (f, r : coordinate) = 
    (* If piece is allowed to move to this square executes
        move; otherwise alerts user of invalid move *)
    let x, y = coord_to_int (f, r) in
    G.moveto ((x * cSQUARE_WIDTH) + (cSQUARE_WIDTH / 2)) ((y * cSQUARE_HEIGHT) + (cSQUARE_HEIGHT / 2));
    if not (piece#can_be_valid_move (f, r)) then
      (G.set_color G.red;
      G.draw_string "Cannot move here")
    else
      (* (let started_at : coordinate = piece#get_pos in *)
      (R.take_turn piece (f,r);
      piece#make_move (f, r);
      (* piece#make_move (f, r); *)
      if R.player_not_in_check piece#get_color then
        (G.clear_graph ();
        draw_board ();
        moved := true)
      else
        (R.take_back ();
        selected := None;
        G.clear_graph ();
        draw_board ();
        G.moveto (cX_BOARDSIZE*cPIXELS_PER_BLOCK / 2) (cY_BOARDSIZE * cPIXELS_PER_BLOCK / 2);
        G.set_color G.magenta;
        G.draw_string "You're in check!"))
  in
  

  (* Continously poll for clicks until a 
        piece has been moved *)
  while not !moved do
    let s = G.wait_next_event [G.Button_down] in
    let x, y = s.mouse_x, s.mouse_y in
    if x > (cX_BOARDSIZE * cPIXELS_PER_BLOCK) || y > (cY_BOARDSIZE * cPIXELS_PER_BLOCK) then
      if (x > 9 * cSQUARE_WIDTH) && 
         (x < 10 * cSQUARE_WIDTH + cSQUARE_WIDTH) && 
         (y > 2 * cSQUARE_HEIGHT) && 
         (y < 3 * cSQUARE_HEIGHT) then
        (R.take_back ();
        draw_board ())
      else
       ()
    else
      (let (f, r)  = get_coords x y in
        let p = R.find_piece (f, r) in
        match p with
        | None -> 
          begin
              match !selected with
              | None -> ()
              | Some s -> move_piece s (f, r)
          end
        | Some piece ->
          begin
            if piece#get_color = R.turn () then
              (selected := Some piece;
              G.clear_graph();
              draw_board ();
              highlight_square piece#get_pos)
            else
              match !selected with
              | None -> G.moveto x y;
                        G.set_color G.red;
                        G.draw_string "Not your turn"
              | Some s -> move_piece s (f, r)
          end)
  done;;

let print_board (pos : (T.piece_type option) array array) : unit =
  Array.iteri (fun _y m -> 
                Array.iteri (fun _x piece_opt -> 
                      match piece_opt with
                      | None -> Printf.printf "O"
                      | Some _piece -> Printf.printf "X") m;
                Printf.printf "\n") pos 
;;

(* Renders the game, waits for next move *)
let render () = 
  G.clear_graph ();
  draw_board ();

  take_turn ();
  if R.checkmate_check () then
    begin
      let x, y = 9, 5 in
      draw_square G.red y x cSQUARE_WIDTH cSQUARE_HEIGHT;
      G.set_color G.black;
      G.moveto (x * cSQUARE_WIDTH + (cSQUARE_WIDTH/2)) (y * cSQUARE_HEIGHT + (cSQUARE_HEIGHT / 2));
      G.draw_string "Checkmate!"
    end
  else if R.check_stalemate () then
    begin
      let x, y = 9, 5 in
      draw_square G.red y x cSQUARE_WIDTH cSQUARE_HEIGHT;
      G.set_color G.black;
      G.moveto (x * cSQUARE_WIDTH + (cSQUARE_WIDTH/2)) (y * cSQUARE_HEIGHT + (cSQUARE_HEIGHT / 2));
      G.draw_string "Stalemate!"
    end;

  G.synchronize ()
;;
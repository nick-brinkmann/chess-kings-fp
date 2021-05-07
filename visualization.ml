open Game ;;
open Params ;;
module G = Graphics ;;
module T = Registry ;;
module R = T.Registry ;;


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
  G.set_window_title "C(hes)S51";
  G.auto_synchronize false ;
  G.display_mode true ;;


(* Drawing simple shapes*)
let draw_square (c : G.color) (y : int) (x : int) (w : int) (h : int) : unit =
  G.set_color c;
  G.fill_rect (x * w)  (y * h) w h ;;


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
  

(* Draws game window *)
let draw_board () : unit =
  
  (* check state and draw accordingly *)
  let x, y = 9, 5 in
  (draw_square G.red y x cSQUARE_WIDTH cSQUARE_HEIGHT;
  G.set_color G.black;
  G.moveto (x * cSQUARE_WIDTH + (cSQUARE_WIDTH/2)) (y * cSQUARE_HEIGHT + (cSQUARE_HEIGHT / 2));
  match R.get_state () with
  | Play -> G.clear_graph ();                  
  | Checkmate -> G.draw_string "Checkmate!"
  | Check -> G.draw_string "Check!"
  | Stalemate -> G.draw_string "Stalemate!");
  
  (* draw chess board *)
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
  |> List.iter (fun piece -> piece#draw) 
;;


(* Returns true if user clicked on the take_back button *)
let clicked_take_back (x, y : int * int) : unit =
  if (x > 9 * cSQUARE_WIDTH) && 
     (x < 10 * cSQUARE_WIDTH + cSQUARE_WIDTH) && 
     (y > 2 * cSQUARE_HEIGHT) && 
     (y < 3 * cSQUARE_HEIGHT) then
     (R.take_back ();
     draw_board ())
 else
  ()
;;


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
    (* If the piece cannot legally move to square, alert user *)
    if not (piece#can_be_valid_move (f, r)) then
      (G.set_color G.red;
      G.draw_string "Cannot move here")
    (* If piece can legally move there, execute provisional move *)
    else
      (* Save the state of the game before the move *)
      (R.take_turn piece (f,r);
      piece#make_move (f, r);
      (* If the move does not leave the player in check, set moved to true *)
      if R.player_not_in_check piece#get_color then
        (G.clear_graph ();
        draw_board ();
        moved := true)
      (* IF the move leaves the player in check, take back move and alert user *)
      else
        (R.take_back ();
        selected := None;
        G.clear_graph ();
        draw_board ();
        G.set_color G.magenta;
        G.moveto (8 * cSQUARE_WIDTH + (cSQUARE_WIDTH/2)) (4 * cSQUARE_HEIGHT + (cSQUARE_HEIGHT / 2));
        G.draw_string "This move would";
        G.moveto (8 * cSQUARE_WIDTH + (cSQUARE_WIDTH/2)) (4 * cSQUARE_HEIGHT + (cSQUARE_HEIGHT / 2) - 10);
        G.draw_string "leave you in check!"))
  in
  

  (* Continously poll for clicks until a 
        piece has been moved *)
  while not !moved do
    let s = G.wait_next_event [G.Button_down] in
    let x, y = s.mouse_x, s.mouse_y in
    if x > (cX_BOARDSIZE * cPIXELS_PER_BLOCK) || y > (cY_BOARDSIZE * cPIXELS_PER_BLOCK) then
      clicked_take_back (x, y)
    else
      (let (f, r)  = get_coords x y in
        let p = R.find_piece (f, r) in
        match p with
        (* If player clicked on an empty square then either move selected 
            piece or do nothing*)
        | None -> 
          begin
              match !selected with
              | None -> ()
              | Some s -> move_piece s (f, r)
          end
        (* If player clicked on piece then execute following code *)
        | Some piece ->
          begin
            (* If the piece the user clicked on is of their color, select the piece *)
            if piece#get_color = R.turn () then
              (selected := Some piece;
              G.clear_graph();
              draw_board ();
              highlight_square piece#get_pos)
            else
              match !selected with
              (* If the user clicked on opponent's piece and no piece has been 
                  selected, indicate it is not the opponent's turn *)
              | None -> G.moveto x y;
                        G.set_color G.red;
                        G.draw_string "Not your turn"
              (* If the user clicked on opponent's piece and a piece is currently selected, 
                  attempt move *)
              | Some s -> move_piece s (f, r)
          end)
  done;;


(* Renders the game, waits for next move *)
let render () = 
  G.clear_graph ();
  draw_board ();

  take_turn ();
  (* After the player has taken their turn, check the state of the opponent's positin *)
  if R.checkmate_check () then
    R.set_state Checkmate
  else if R.check_stalemate () then
    R.set_state Stalemate
  else if not (R.player_not_in_check (R.turn ())) then
    R.set_state Check
  else if R.get_state () = Check then
    R.set_state Play;


;;
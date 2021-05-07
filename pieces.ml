open Game ;;
open Params ;;
module G = Graphics ;;
module T = Registry ;;
module R = T.Registry ;;
module Viz = Visualization ;;

(* ............................................................................
                        Class types for pieces
   ......................................................................... *)

class piece (initfile : file) (initrank : rank) (p : bool) = 
  object (self)
    (* player to which the piece belongs *) 
    val player = p
    val mutable f : file = initfile
    val mutable r : rank = initrank
    val mutable moves : int = 0
    
    method name : piece_name = Piece
    
    (* method player_color : bool = player *)
    val color = if p then cWHITE_PIECE_COLOR else cBLACK_PIECE_COLOR

    (* is of king type *)
    method is_king = false

    (* get_color -- returns true for a white piece, false for a black piece *)
    method get_color : bool = player

    (* get_pos -- returns current coordinate of a piece *)
    method get_pos : coordinate = 
      f, r

    (* get_moves -- returns number of moves a piece has taken *)
    method get_moves : int = moves

    (* make_move -- updates the coordinate of a piece *)
    method make_move ((new_f, new_r) as coord : coordinate) : unit = 
      (* if opponent piece at new coordinate, deregister that piece *)
      let delete_opp_piece () =
        match R.find_piece coord with 
        | None -> () 
        | Some piece -> 
          (if piece#get_color = self#get_color then 
            raise (Invalid_argument "make_move: trying to move onto own piece")
          else R.deregister piece)
      in
      (* R.take_turn is called only in Visualization. *)
      delete_opp_piece ();
      (* let old_pos = self#get_pos in *)
      f <- new_f;
      r <- new_r;
      moves <- moves + 1
      (*debugging purposes*)
      (* Printf.printf "%s %s-%s has been played\n" self#name 
      (coord_to_string old_pos) 
      (coord_to_string self#get_pos) *)

    (* checks whether a piece can move to a coordinate. False by default. *)
    method can_be_valid_move (_c : coordinate) : bool = false

    (* only really used for pawns. false by default. *)
    method attacks_square (_coord : coordinate) : bool = false

    method chebyshev_distance_to (end_coord : coordinate) : int = 
      let start_x, start_y = coord_to_int self#get_pos in 
      let end_x, end_y = coord_to_int end_coord in 
      max (abs (start_x - end_x)) (abs (start_y - end_y))

    (* draw -- does all logic of setting correct color and moving to correct location
                for drawing a piece. Then, in subclass, we call this method and then
                use draw_string to actually draw the piece type *)
    method draw : unit = 
      G.set_color color;
      let (f, r) = self#get_pos in
      let x = (cSQUARE_WIDTH * (file_to_int f) + (7 * cSQUARE_WIDTH / 16)) in
      let y = cSQUARE_HEIGHT * (rank_to_int r) + (7 * cSQUARE_HEIGHT / 16) in
      G.moveto x y;
      G.draw_string (piece_name_to_string self#name)

  end;;

class pawn (initfile : file) (initrank : rank) (player : bool) =
object (self)
  inherit piece initfile 
                initrank
                player
           as super

  method! name : piece_name = Pawn

  method! can_be_valid_move (end_file, end_rank as coord: coordinate) : bool = 
    let end_fi, end_ra = coord_to_int coord in
    let curr_fi, curr_ra = coord_to_int super#get_pos in 
    let is_at_starting_square = (moves = 0) in 
    (* moving directly forward 1 square *)
    if (end_ra - curr_ra = (if player then 1 else ~-1)) &&
      (end_fi = curr_fi) &&
      (not (R.contains_any_piece coord)) then true
    (* capturing diagonally *)
    else if (end_ra - curr_ra = (if player then 1 else ~-1)) && 
       abs (end_fi - curr_fi) = 1 && 
       (R.contains_enemy_piece super#get_color coord) then true 
    (* moving forward two squares *)
    else if is_at_starting_square && end_fi = curr_fi && 
      (end_ra - curr_ra = (if player then 2 else ~-2)) &&
      (not (R.is_piece_along_line_from coord super#get_pos)) &&
      (not (R.contains_any_piece coord)) then true 
    (* en passant *)
    else 
      match R.last_move () with
      | None -> false 
      | Some last_move ->
    last_move.player <> self#get_color && last_move.piece = Pawn &&
    (fst last_move.end_square) = end_file && 
    (rank_to_int (snd last_move.end_square)) = curr_ra &&
    end_rank = (if self#get_color then R6 else R3) &&
    abs (end_fi - curr_fi) = 1 &&
    (snd last_move.start_square) = (if last_move.player then R2 else R7)

  method! attacks_square (coord : coordinate) : bool = 
    let end_fi, end_ra = coord_to_int coord in
    let curr_fi, curr_ra = coord_to_int super#get_pos in 
    (end_ra - curr_ra = (if player then 1 else ~-1)) && 
    abs (end_fi - curr_fi) = 1 
    


  (* promotion, en passant *)
  method! make_move ((new_f, new_r) as coord : coordinate) : unit = 
    (* en passant *)
    let end_fi, end_ra = coord_to_int coord in 
    let curr_fi, curr_ra = coord_to_int super#get_pos in 

    let is_en_passant : bool = match R.two_moves_ago () with 
    | None -> false
    | Some last_move ->
      begin
<<<<<<< HEAD
=======
        (* if this logical statement returns true, then en passant is being played  *)
>>>>>>> 93bae1efb34dead0a1f8e48aa4ee45eed3cbf597
        last_move.player <> self#get_color && last_move.piece = Pawn &&
        (fst last_move.end_square) = new_f && 
        (rank_to_int (snd last_move.end_square)) = curr_ra &&
        new_r = (if self#get_color then R6 else R3) &&
        abs (end_fi - curr_fi) = 1 &&
<<<<<<< HEAD
        (snd last_move.start_square) = (if last_move.player then R2 else R7)
=======
        (snd last_move.start_square) = (if last_move.player then R2 else R7) 
>>>>>>> 93bae1efb34dead0a1f8e48aa4ee45eed3cbf597
      end
    in

    (* execute en passant*)
    if is_en_passant then
      begin
        match R.find_piece 
        (new_f, int_to_rank (end_ra + (if super#get_color then -1 else 1))) with 
        | None -> raise (Invalid_argument "en passant error")
        | Some opp_pawn -> R.deregister opp_pawn;
        super#make_move coord
      end
    else
      (* not en passant *)
      super#make_move coord;
      let new_rank_int = rank_to_int new_r in 
      (* promotion. currently auto-queen. *)
      if new_rank_int = 7 - (if super#get_color then 0 else 1) * 7 && R.is_real_board () then
        begin
          R.deregister (self :> T.piece_type);
          (* this function gets called when a pawn promotes *)
          let promote (pawn : T.piece_type) =
            let (end_f, end_r) = coord_to_int pawn#get_pos in
            let c = pawn#get_color in
            let direction = (if c then -1 else 1) in
            (* draw queen option *)
            Viz.draw_square cPROMOTE_COLOR (end_r + (direction * 1)) end_f cSQUARE_WIDTH cSQUARE_HEIGHT;
            G.moveto (end_f * cSQUARE_WIDTH + (cSQUARE_WIDTH / 2)) 
                    ((end_r + (direction * 1))*cSQUARE_HEIGHT + (cSQUARE_HEIGHT / 2));
            G.set_color G.black;
            G.draw_string "Queen";

            (* draw rook option *)
            Viz.draw_square cPROMOTE_COLOR (end_r + (direction * 2)) end_f cSQUARE_WIDTH cSQUARE_HEIGHT;
            G.moveto (end_f * cSQUARE_WIDTH + (cSQUARE_WIDTH / 2)) 
                    ((end_r + (direction * 2))*cSQUARE_HEIGHT + (cSQUARE_HEIGHT / 2));
            G.set_color G.black;
            G.draw_string "Rook";
            
            (* draw bishop option *)
            Viz.draw_square cPROMOTE_COLOR (end_r + (direction * 3)) end_f cSQUARE_WIDTH cSQUARE_HEIGHT;
            G.moveto (end_f * cSQUARE_WIDTH + (cSQUARE_WIDTH / 2)) 
                    ((end_r + (direction * 3))*cSQUARE_HEIGHT + (cSQUARE_HEIGHT / 2));
            G.set_color G.black;
            G.draw_string "Bishop";

            (* draw knight option *)
            Viz.draw_square cPROMOTE_COLOR (end_r + (direction * 4)) end_f cSQUARE_WIDTH cSQUARE_HEIGHT;
            G.moveto (end_f * cSQUARE_WIDTH + (cSQUARE_WIDTH / 2)) 
                    ((end_r + (direction * 4))*cSQUARE_HEIGHT + (cSQUARE_HEIGHT / 2));
            G.set_color G.black;
            G.draw_string "Knight";

            let rec choose_piece () = 
              let s = G.wait_next_event [G.Button_down] in
              let x, y = s.mouse_x / cSQUARE_WIDTH, s.mouse_y / cSQUARE_HEIGHT in
              if x <> end_f then
                choose_piece ()
              else
                begin
                  (* chose queen *)
                  if y = end_r + (direction*1) then
                    R.register ((new queen 
                                    (int_to_file end_f)
                                    (int_to_rank end_r) c) :> T.piece_type)
                  (* chose rook *)
                  else if y = end_r + (direction*2) then
                    R.register ((new rook 
                                    (int_to_file end_f)
                                    (int_to_rank end_r) c) :> T.piece_type)
                  (* chose bishop *)
                  else if y = end_r + (direction*3) then
                    R.register ((new bishop 
                                    (int_to_file end_f)
                                    (int_to_rank end_r) c) :> T.piece_type)
                  (* chose knight *)
                  else if y = end_r + (direction*4) then
                    R.register ((new knight 
                                    (int_to_file end_f)
                                    (int_to_rank end_r) c) :> T.piece_type)
                  (* clicked elsewhere *)
                  else 
                    choose_piece ()
                end
            in
            choose_piece ();
          in
          promote (self :> T.piece_type);
        end

end 

and (* class *) rook (initfile : file) (initrank : rank) (player : bool) =
object
  inherit piece initfile 
                initrank
                player 
          as super

  method! name : piece_name = Rook

  method! can_be_valid_move (end_file, end_rank as coord : coordinate) : bool =
    (* ensures exactly one of rank and file is unchanged *)
    let curr_file, curr_rank = super#get_pos in
    (* exclusive or, then negated. Checks that exactly one of current rank and
       current file is different to end rank and end file *) 
    if not ((curr_file <> end_file) <> (curr_rank <> end_rank)) then false else

    (* verifies that there is no piece between current square and final square, 
       and that no friendly piece on final square *)
    (not (R.is_piece_along_line_from coord super#get_pos)) && 
    (not (R.contains_own_piece super#get_color coord))

end

and (* class *) knight (initfile : file) (initrank : rank) (player : bool) =
object
  inherit piece initfile 
  initrank
  player 
  as super

  method! name : piece_name = Knight

  method! can_be_valid_move (coord : coordinate) : bool =
    let curr_file, curr_rank = coord_to_int super#get_pos in 
    let end_file, end_rank = coord_to_int coord in
    (* ensures sum of absolute value of changes to rank and file is 3 *)
    (abs (curr_file - end_file)) + (abs (curr_rank - end_rank)) = 3 &&
    (* and that you're moving both horizontally and vertically, not just in 
       a straight line *)
    (abs (curr_file - end_file)) <> 0 && (abs (curr_rank - end_rank)) <> 0 &&
    (* ensures no friendly piece at ending square *)
    (not (R.contains_own_piece super#get_color coord))

end

and (* class *) bishop (initfile : file) (initrank : rank) (player : bool) =
object
  inherit piece initfile 
  initrank
  player 
  as super

  method! name : piece_name = Bishop

  method! can_be_valid_move (coord : coordinate) : bool =
    (* convert to integer representation *)
    let curr_file, curr_rank = coord_to_int super#get_pos in 
    let end_file, end_rank = coord_to_int coord in 

    (* checks moving along a diagonal *)
    let dist_x = end_file - curr_file in 
    let dist_y = end_rank - curr_rank in 
    (abs dist_x = abs dist_y) && (dist_x <> 0) &&
    (* no pieces along the way *)
    (not (R.is_piece_along_line_from super#get_pos coord)) &&
    (* no friendly piece at ending square *)
    (not (R.contains_own_piece super#get_color coord))
end

and (* class *)  queen (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method! name : piece_name = Queen

  method! can_be_valid_move (coord : coordinate) : bool =
    (* creates a new instance of a bishop and a rook at the same starting square. If either 
      of them can move to the ending square, the move is valid. *)
    let new_rook = (new rook (fst super#get_pos) (snd super#get_pos) self#get_color) in 
    let new_bishop = (new bishop (fst super#get_pos) (snd super#get_pos) self#get_color) in 
    (new_rook#can_be_valid_move coord) || (new_bishop#can_be_valid_move coord)

end

and (* class *) king (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile
  initrank
  player
  as super

  method! name : piece_name = King

  method! is_king = true

  method! can_be_valid_move (coord : coordinate) : bool = 
    (* Checks that no opponent pieces attack the square the king moves to *)
    let all_opp_pieces = R.subset (not (super#get_color)) in 
    let opp_king : T.piece_type = 
      List.filter (fun obj -> obj#is_king) all_opp_pieces
      |> List.hd in
    (* have to treat pawns differently, since the king can move in front of
       pawns, which are counted as legal moves for pawns but don't put the king 
      in check *)
    let opp_pawns : T.piece_type list =
      List.filter (fun obj -> obj#name = Pawn) all_opp_pieces 
    in  
    let opp_other_pieces : T.piece_type list = 
      List.filter (fun obj -> (not obj#is_king) && (not (obj#name = Pawn))) 
      all_opp_pieces 
    in 
    let pieces_dont_attack (coord : coordinate) : bool = 
      List.for_all (fun obj -> not (obj#can_be_valid_move coord)) opp_other_pieces 
    in
    let pawns_dont_attack (coord : coordinate) : bool =
      List.for_all 
      (* either pawns cannot move to the square, OR the square is on the same
        file as the pawn, i.e. the pawn does not threaten the square. *)
      (fun obj -> 
        not (obj#attacks_square coord) || 
        (fst coord) = (fst obj#get_pos)) 
      opp_pawns
    in

    (* Castling related logic, used below *)
    let new_f, new_r = coord_to_int coord in
    let starting_rank : rank = if super#get_color then R1 else R8 in
    let is_kingside_castling : bool = 
      let h_rook_not_moved : bool =
        match R.find_piece (H, starting_rank) with 
        | None -> false 
        | Some piece -> 
          (piece#name = Rook) && (piece#get_moves = 0)
      in
      (new_f = 6) && (new_r = 7 - (if super#get_color then 7 else 0)) &&
      (super#get_moves = 0) && (h_rook_not_moved) && 
      (pieces_dont_attack (F, starting_rank)) &&
      (pawns_dont_attack (F, starting_rank)) &&
      (not (R.contains_any_piece (F, starting_rank))) && 
      (not (R.contains_any_piece (G, starting_rank)))
    in 
    let is_queenside_castling : bool = 
      let a_rook_not_moved : bool = 
        match R.find_piece (A, starting_rank) with 
        | None -> false 
        | Some piece -> 
          (piece#name = Rook) && (piece#get_moves = 0)
      in
      (new_f = 2) && (new_r = 7 - (if super#get_color then 7 else 0)) &&
      (super#get_moves = 0) && (a_rook_not_moved) && 
      (pieces_dont_attack (D, starting_rank)) &&
      (pawns_dont_attack (D, starting_rank)) &&
      (not (R.contains_any_piece (D, starting_rank))) &&
      (not (R.contains_any_piece (C, starting_rank))) &&
      (not (R.contains_any_piece (B, starting_rank)))
    in 
    
    (*
    - no pieces attacking ending square
    - Chebyshev distance = 1
    - No friendly piece at square *)
    (pieces_dont_attack coord) &&
    (pawns_dont_attack coord) &&
    ((opp_king#chebyshev_distance_to coord) > 1) && 
    (* above always holds. Now disjoint cases for castling and other *)
    (
      ((self#chebyshev_distance_to coord) = 1) &&
      (not (R.contains_own_piece super#get_color coord)) ||
      (* OR castling
      - king moving to g1 or c1 (or g8/c8)
      - no enemy pieces attacking ending square, starting square (no castling out
        of check), or intervening square
      - no pieces on intervening square/ending square
      - if queenside castling, no piece on b1/b8
      - neither king nor rook has moved yet  *)
      (* castling logic for both kingside and queenside *)
      ((self#chebyshev_distance_to coord) = 2) &&
      ((opp_king#chebyshev_distance_to coord) > 1) &&
      (pieces_dont_attack self#get_pos) &&
      (pieces_dont_attack self#get_pos) &&
      (
        is_kingside_castling || is_queenside_castling
      )
    )

  method! make_move (new_f, _new_r as coord : coordinate) : unit =
    (* the move is castling *)
    if (self#chebyshev_distance_to coord = 2) && (self#can_be_valid_move coord) then
      begin
        let starting_rank = if super#get_color then R1 else R8 in
        (* R.flip_turn (); *)
        (* kingside *)
        if new_f = C then
          begin 
            match R.find_piece (A, starting_rank) with 
            | None -> raise (Invalid_argument "castling error")
            | Some piece -> piece#make_move (D, starting_rank)
          end
        else if new_f = G then 
          begin
            match R.find_piece (H, starting_rank) with 
            | None -> raise (Invalid_argument "castling error")
            | Some piece -> piece#make_move (F, starting_rank)
          end
        else raise (Invalid_argument "castling error")
      end;
    super#make_move coord

end 

;;

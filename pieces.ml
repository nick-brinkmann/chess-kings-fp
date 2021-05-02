(* ............................................................................
                        Class types for pieces
   ......................................................................... *)

open Game ;;
open Params ;;
module G = Graphics ;;
module T = Registry ;;
module R = T.Registry ;;




class piece (initfile : file) (initrank : rank) (p : bool) = 
  object (self)
    (* player to which the piece belongs *) 
    val player = p
    val mutable f : file = initfile
    val mutable r : rank = initrank
    val mutable moves : int = 0
    
    method name : string = "piece"
    
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
      | None -> (* Printf.printf "No enemy piece here \n" *) () 
      | Some piece -> if piece#get_color <> self#get_color then 
        R.deregister piece
      in
      delete_opp_piece ();
      R.move_piece self#get_pos coord;
      f <- new_f;
      r <- new_r;
      moves <- moves + 1;

    method can_be_valid_move (_c : coordinate) : bool = 
      true

    method chebyshev_distance_to (_c : coordinate) : int = ~-1

    (* draw -- does all logic of setting correct color and moving to correct location
                for drawing a piece. Then, in subclass, we call this method and then
                use draw_string to actually draw the piece type *)
    method draw : unit = 
      G.set_color color;
      let (f, r) = self#get_pos in
      let x = cSQUARE_WIDTH * (file_to_int f) + (cSQUARE_WIDTH / 2) in
      let y = cSQUARE_HEIGHT * (rank_to_int r) + (cSQUARE_HEIGHT / 2) in
      G.moveto x y

  end;;

class pawn (initfile : file) (initrank : rank) (player : bool) =
object (self)
  inherit piece initfile 
                initrank
                player
           as super

  method! name : string = "pawn"

  method! can_be_valid_move (end_file, end_rank as coord: coordinate) : bool =
    if not (super#can_be_valid_move coord) then false else 
    let end_fi, end_ra = (file_to_int end_file), (rank_to_int end_rank) in
    let (curr_fi, curr_ra) = (file_to_int (fst super#get_pos)), 
                             (rank_to_int (snd super#get_pos)) 
      in 
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
    else is_at_starting_square && end_fi = curr_fi && 
      (end_ra - curr_ra = (if player then 2 else ~-2)) &&
      (not (R.is_piece_along_line_from coord super#get_pos)) &&
      (not (R.contains_any_piece coord))

  (* ensuring promotion works. Currently have auto-queening *)
  method! make_move ((new_f, new_r) as coord : coordinate) : unit = 
    super#make_move coord;
    let new_rank_int = rank_to_int new_r in 
    (* queening *)
    if new_rank_int = 7 - (if super#get_color then 0 else 1) * 7 then
      begin
        R.deregister (self :> T.piece_type);
        R.register ((new queen new_f new_r self#get_color) :> T.piece_type);
      end

  method! draw : unit = 
    super#draw;
    G.draw_string "Pawn"
      
end 

and (* class *) rook (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
                initrank
                player 
          as super

  method! name : string = "rook"

  method! can_be_valid_move (end_file, end_rank as coord : coordinate) : bool =
    if not (super#can_be_valid_move coord) then false else 
    (* ensures exactly one of rank and file is unchanged *)
    let curr_file, curr_rank = super#get_pos in
    (* exclusive or, then negated. Checks that exactly one of current rank and
       current file is different to end rank and end file *) 
    if not ((curr_file <> end_file) <> (curr_rank <> end_rank)) then false else

    (* verifies that there is no piece between current square and final square, 
       and that no friendly piece on final square *)
    (not (R.is_piece_along_line_from coord super#get_pos)) && 
    (not (R.contains_own_piece super#get_color coord))

   method! draw : unit = 
      super#draw;
      G.draw_string "Rook"

end

and (* class *) knight (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method! name : string = "knight"

  method! can_be_valid_move (coord : coordinate) : bool =
    if not (super#can_be_valid_move coord) then false else 
    let curr_file, curr_rank = coord_to_int super#get_pos in 
    let end_file, end_rank = coord_to_int coord in
    (* ensures sum of absolute value of changes to rank and file is 3 *)
    (abs (curr_file - end_file)) + (abs (curr_rank - end_rank)) = 3 &&
    (* ensures no friendly piece at ending square *)
    (not (R.contains_own_piece super#get_color coord))


  method! draw : unit = 
   super#draw;
    G.draw_string "Knight"
end

and (* class *) bishop (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method! name : string = "bishop"

  method! can_be_valid_move (coord : coordinate) : bool =
    if not (super#can_be_valid_move coord) then false else 
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


  method! draw : unit = 
   super#draw;
    G.draw_string "Bishop"
end

and (* class *)  queen (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method! name : string = "queen"

  method! can_be_valid_move (coord : coordinate) : bool =
    (* Registers a new bishop and rook at the same starting square. If either 
      of them can move to the ending square, the move is valid. *)
    let new_rook = (new rook (fst super#get_pos) (snd super#get_pos) self#get_color) in 
    let new_bishop = (new bishop (fst super#get_pos) (snd super#get_pos) self#get_color) in 
    (new_rook#can_be_valid_move coord) || (new_bishop#can_be_valid_move coord)


  method! draw : unit = 
   super#draw;
    G.draw_string "Queen"
end

and (* class *) king (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile
  initrank
  player
  as super

  method! name : string = "king"

  (* QUESTION: Is there any reason we override this here, instead of just defining
      it in the super class?  *)
  method! chebyshev_distance_to (end_coord : coordinate) : int = 
    let start_x, start_y = coord_to_int super#get_pos in 
    let end_x, end_y = coord_to_int end_coord in 
    max (abs (start_x - end_x)) (abs (start_y - end_y))

  method! is_king = true

  method! can_be_valid_move (coord : coordinate) : bool = 
    (* Checks that no opponent pieces attack the square the king moves to *)
    let opponent_pieces = R.subset (not (super#get_color)) in 
    let opp_king = 
      List.filter (fun obj -> obj#is_king) opponent_pieces
      |> List.hd 
    in 
    let opp_pieces_not_king = 
      List.filter (fun obj -> not obj#is_king) opponent_pieces in 
    let is_not_attacked (coord : coordinate) (pieces : T.piece_type list) : bool = 
      List.for_all (fun obj -> not (obj#can_be_valid_move coord)) pieces in
    (* Castling related logic, used below *)
    let new_f, new_r = coord_to_int coord in
    let starting_rank : rank = if super#get_color then R1 else R8 in
    let is_kingside_castling : bool = 
      let h_rook_not_moved : bool =
        match R.find_piece (H, starting_rank) with 
        | None -> false 
        | Some piece -> 
          (piece#name = "rook") && (piece#get_moves = 0)
      in
      (new_f = 6) && (new_r = 7 - (if super#get_color then 7 else 0)) &&
      (super#get_moves = 0) && (h_rook_not_moved) && 
      (is_not_attacked (F, starting_rank) opp_pieces_not_king) &&
      (not (R.contains_any_piece (F, starting_rank))) && 
      (not (R.contains_any_piece (G, starting_rank)))
    in 
    let is_queenside_castling : bool = 
      let a_rook_not_moved : bool = 
        match R.find_piece (A, starting_rank) with 
        | None -> false 
        | Some piece -> 
          (piece#name = "rook") && (piece#get_moves = 0)
      in
      (new_f = 2) && (new_r = 7 - (if super#get_color then 7 else 0)) &&
      (super#get_moves = 0) && (a_rook_not_moved) && 
      (is_not_attacked (D, starting_rank) opp_pieces_not_king) &&
      (not (R.contains_any_piece (D, starting_rank))) &&
      (not (R.contains_any_piece (C, starting_rank))) &&
      (not (R.contains_any_piece (B, starting_rank)))
    in 
    
    (*
    - no pieces attacking ending square
    - Chebyshev distance = 1
    - No friendly piece at square *)
    (is_not_attacked coord opp_pieces_not_king) &&
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
    (is_not_attacked coord opp_pieces_not_king) &&
    ((self#chebyshev_distance_to coord) = 2) &&
    ((opp_king#chebyshev_distance_to coord) > 1) &&
    (
      is_kingside_castling || is_queenside_castling
    )
    )




  method! draw : unit = 
   super#draw;
    G.draw_string "King"
end 

;;

(* let rec print_opp_pieces lst =
  match lst with 
  | [] -> ()
  | obj :: tl -> 
    (let file = file_to_string (fst obj#get_pos) in 
      let rank = rank_to_string (snd obj#get_pos) in 
      Printf.printf "File: %s" file;
      Printf.printf "Rank: %s" rank;
      Printf.printf "\n";
      print_opp_pieces tl
    )
in *)
(* print_opp_pieces opponent_pieces; *)
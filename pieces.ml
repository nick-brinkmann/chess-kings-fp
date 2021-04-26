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
    
    (* method player_color : bool = player *)
    val color = if p then cWHITE_PIECE_COLOR else cBLACK_PIECE_COLOR

    (* get_color -- returns true for a white piece, false for a black piece *)
    method get_color : bool = player

    (* get_pos -- returns current coordinate of a piece *)
    method get_pos : coordinate = 
      f, r

    (* update_pos -- updates the coordinate of a piece *)
    method make_move ((new_f, new_r) : coordinate) = 
      f <- new_f;
      r <- new_r;
      moves <- moves + 1;
      R.move_piece self#get_pos (new_f, new_r)

    method can_be_valid_move (_c : coordinate) : bool = true

    (* draw ?color -- draws a piece, with an optional new color *)
    method draw : unit = 
      (* if player then G.set_color cWHITE_PIECE_COLOR 
      else G.set_color cBLACK_PIECE_COLOR; *)
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

  method! can_be_valid_move (end_file, end_rank as coord: coordinate) : bool = 
    let end_fi, end_ra = (file_to_int end_file), (rank_to_int end_rank) in
    let (curr_fi, curr_ra) = (file_to_int (fst super#get_pos)), 
                             (rank_to_int (snd super#get_pos)) 
      in 
    let is_at_starting_square = (moves = 0) in 
    (* moving directly forward 1 square *)
    if (end_ra - curr_ra = (if player then 1 else ~-1)) &&
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


   method! draw : unit = 
      super#draw;
      G.draw_string "Pawn"
   (* method make_move ((new_f, new_r) : coordinate) =  *)
      
end 

and (* class *) rook (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
                initrank
                player 
          as super

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


  (* method can_be_valid_move (end_file, end_rank : coordinate) : bool = 
    let (curr_file, curr_rank) = super#get_pos in  *)
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

  method! can_be_valid_move (coord : coordinate) : bool =
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

  method! can_be_valid_move (coord : coordinate) : bool =
    (* Registers a new bishop and rook at the same starting square. If either 
      of them can move to the ending square, the move is valid. *)
    let new_rook = (new rook (fst super#get_pos) (snd super#get_pos) true) in 
    let new_bishop = (new bishop (fst super#get_pos) (snd super#get_pos) true) in 
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

  method! can_be_valid_move (coord : coordinate) : bool = 
    (* Checks that no opponent pieces attack the square the king moves to *)
    let opponent_pieces = R.subset (not (super#get_color)) in 
    Printf.printf "%d" (R.size_of_registrants ());
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
    let is_not_attacked (coord : coordinate) (pieces : T.piece_type list) : bool = 
      List.for_all (fun obj -> not (obj#can_be_valid_move coord)) pieces in
    let chebyshev_distance_to (end_coord : coordinate) : int = 
      let start_x, start_y = coord_to_int super#get_pos in 
      let end_x, end_y = coord_to_int end_coord in 
      max (abs (start_x - end_x)) (abs (start_y - end_y)) in 
    (* no pieces attacking ending square and Chebyshev distance = 1 *)
    (is_not_attacked coord opponent_pieces) && ((chebyshev_distance_to coord) = 1)
    (* NEED TO STILL ACCOUNT FOR CASTLING*)


  method! draw : unit = 
   super#draw;
    G.draw_string "King"
end 

;;
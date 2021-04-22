(* ............................................................................
                        Class type for pieces
   ......................................................................... *)

open Game ;;
open Params ;;
module G = Graphics ;;

class piece (initfile : file) (initrank : rank) (p : bool) = 
  object (self)
    (* player to which the piece belongs *) 
    val player = p
    val mutable f : file = initfile
    val mutable r : rank = initrank
    val mutable moves : int = 0
    
    (* method player_color : bool = player *)
    val color = if p then cWHITE_PIECE_COLOR else cBLACK_PIECE_COLOR

    method get_color = color

    method get_pos : coordinate = 
      f, r

    method update_pos ((new_f, new_r) : coordinate) = 
      f <- new_f;
      r <- new_r;
      moves <- moves + 1

    method draw ?(color=color) : unit = 
      G.set_color color;
      let (f, r) = self#get_pos in
      let x = cSQUARE_WIDTH * ((file_to_int f) - 1) + (cSQUARE_WIDTH / 2) in
      let y = cSQUARE_HEIGHT * ((rank_to_int r) - 1) + (cSQUARE_HEIGHT / 2) in
      G.moveto x y

  end;;

class pawn (initfile : file) (initrank : rank) (player : bool) =
object (self)
  inherit piece initfile 
                 initrank
                 player
           as super

  method can_be_valid_move (end_file, end_rank : coordinate) : bool = 
    let end_fi, end_ra = (file_to_int end_file), (rank_to_int end_rank) in
    let (curr_fi, curr_ra) = (file_to_int (fst super#get_pos)), 
                             (rank_to_int (snd super#get_pos)) 
      in 
    (* let is_at_starting_square = (curr_ra = 7 - (if player then 1 else 0) * 5) in  *)
    let is_at_starting_square = (moves = 0) in 
    if (end_ra - curr_ra = (if player then 1 else ~-1) * 1) && 
       abs (end_fi - curr_fi) <= 1 then true 
    else is_at_starting_square && end_fi = curr_fi && 
      (end_ra - curr_ra = (if player then 1 else ~-1) * 2)


   method! draw ?(color=self#color) : unit = 
      super#draw color;
      G.draw_string "Pawn"
   (* method make_move ((new_f, new_r) : coordinate) =  *)
      
end 

and (* class *) rook (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
                initrank
                player 
          as super

  (* method can_be_valid_move (end_file, end_rank : coordinate) : bool = 
    let (curr_file, curr_rank) = super#get_pos in  *)
   method! draw ?(color=self#color): unit = 
      super#draw color;
      G.draw_string "Rook"

end

and (* class *) knight (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method! draw ?(color=self#color): unit = 
   super#draw color;
    G.draw_string "Knight"
end

and (* class *) bishop (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method! draw ?(color=self#color): unit = 
   super#draw color;
    G.draw_string "Bishop"
end

and (* class *)  queen (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method! draw ?(color=self#color): unit = 
   super#draw color;
    G.draw_string "Queen"
end

and (* class *) king (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method! draw ?(color=self#color): unit = 
   super#draw color;
    G.draw_string "King"
end 

;;
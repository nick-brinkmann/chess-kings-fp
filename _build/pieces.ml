(* ............................................................................
                        Class type for pieces
   ......................................................................... *)

open Game ;;
module Viz = Visualization ;;
module G = Graphics ;;

class piece (initfile : file) (initrank : rank) (player : bool) = 
  object (self)
    (* player to which the piece belongs *) 
    val player = player
    val mutable f : file = initfile
    val mutable r : rank = initrank
    val mutable moves : int = 0
    
    method get_pos : coordinate = 
      f, r

    method update_pos ((new_f, new_r) : coordinate) = 
      f <- new_f;
      r <- new_r;
      moves <- moves + 1

  end;;

class pawn (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
                 initrank
                 player
           as super

  (* method can_be_valid_move (end_file, end_rank : coordinate) : bool = 
    let (curr_file, curr_rank) = super#get_pos in 
    let is_at_starting_square = (curr_rank = 7 - (if player then 1 else 0) * 5) in 
    if (end_rank - curr_rank = (if player then 1 else ~-1) * 1) && 
       abs (end_file - curr_file) <= 1 then true 
    else if is_at_starting_square && end_file = curr_file && 
      (end_rank - curr_rank = (if player then 1 else ~-1) * 2) then true *)


   method draw : unit = 
      if player then G.set_color G.red
      else G.set_color G.blue;
      let (f, r) = self#get_pos in
      let x = Viz.cSQUARE_WIDTH * ((file_to_int f) - 1) + (Viz.cSQUARE_WIDTH / 2) in
      let y = Viz.cSQUARE_HEIGHT * ((rank_to_int r) - 1) + (Viz.cSQUARE_HEIGHT / 2) in
      G.moveto x y;
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
   method draw : unit = 
      if player then G.set_color G.red
      else G.set_color G.blue;
      let (f, r) = self#get_pos in
      let x = Viz.cSQUARE_WIDTH * ((file_to_int f) - 1) + (Viz.cSQUARE_WIDTH / 2) in
      let y = Viz.cSQUARE_HEIGHT * ((rank_to_int r) - 1) + (Viz.cSQUARE_HEIGHT / 2) in
      G.moveto x y;
      G.draw_string "Rook"

end

and (* class *) knight (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method draw : unit = 
   if player then G.set_color G.red
   else G.set_color G.blue;
   let (f, r) = self#get_pos in
   let x = Viz.cSQUARE_WIDTH * ((file_to_int f) - 1) + (Viz.cSQUARE_WIDTH / 2) in
   let y = Viz.cSQUARE_HEIGHT * ((rank_to_int r) - 1) + (Viz.cSQUARE_HEIGHT / 2) in
   G.moveto x y;
   G.draw_string "Knight"
end

and (* class *) bishop (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method draw : unit = 
   if player then G.set_color G.red
   else G.set_color G.blue;
   let (f, r) = self#get_pos in
   let x = Viz.cSQUARE_WIDTH * ((file_to_int f) - 1) + (Viz.cSQUARE_WIDTH / 2) in
   let y = Viz.cSQUARE_HEIGHT * ((rank_to_int r) - 1) + (Viz.cSQUARE_HEIGHT / 2) in
   G.moveto x y;
   G.draw_string "Bishop"
end

and (* class *)  queen (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method draw : unit = 
   if player then G.set_color G.red
   else G.set_color G.blue;
   let (f, r) = self#get_pos in
   let x = Viz.cSQUARE_WIDTH * ((file_to_int f) - 1) + (Viz.cSQUARE_WIDTH / 2) in
   let y = Viz.cSQUARE_HEIGHT * ((rank_to_int r) - 1) + (Viz.cSQUARE_HEIGHT / 2) in
   G.moveto x y;
   G.draw_string "Queen"
end

and (* class *) king (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super

  method draw : unit = 
   if player then G.set_color G.red
   else G.set_color G.blue;
   let (f, r) = self#get_pos in
   let x = Viz.cSQUARE_WIDTH * ((file_to_int f) - 1) + (Viz.cSQUARE_WIDTH / 2) in
   let y = Viz.cSQUARE_HEIGHT * ((rank_to_int r) - 1) + (Viz.cSQUARE_HEIGHT / 2) in
   G.moveto x y;
   G.draw_string "King"
end 

;;
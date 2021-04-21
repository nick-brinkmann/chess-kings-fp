(* ............................................................................
                        Class type for pieces
   ......................................................................... *)

open Game ;;

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

  method can_be_valid_move (end_file, end_rank : coordinate) : bool = 
    let (curr_file, curr_rank) = super#get_pos in 
    let is_at_starting_square = (curr_rank = 7 - (if player then 1 else 0) * 5) in 
    if (end_rank - curr_rank = (if player then 1 else ~-1) * 1) && 
       abs (end_file - curr_file) <= 1 then true 
    else if is_at_starting_square && end_file = curr_file && 
      (end_rank - curr_rank = (if player then 1 else ~-1) * 2) then true


   
   (* method make_move ((new_f, new_r) : coordinate) =  *)
      
end 

and (* class *) rook (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
                initrank
                player 
          as super

  method can_be_valid_move (end_file, end_rank : coordinate) : bool = 
    let (curr_file, curr_rank) = super#get_pos in 

end

and (* class *) knight (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super
end

and (* class *) bishop (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super
end

and (* class *)  queen (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super
end

and (* class *) king (initfile : file) (initrank : rank) (player : bool) =
object(self)
  inherit piece initfile 
  initrank
  player 
  as super
end 

;;
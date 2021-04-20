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
   
   (* method make_move ((new_f, new_r) : coordinate) =  *)
      
end 

and (* class *) rook (initfile : file) (initrank : rank) (player : bool) =
object(self)
end

and (* class *) knight (initfile : file) (initrank : rank) (player : bool) =
object(self)
end

and (* class *) bishop (initfile : file) (initrank : rank) (player : bool) =
object(self)
end

and (* class *)  queen (initfile : file) (initrank : rank) (player : bool) =
object(self)
end

and (* class *) king (initfile : file) (initrank : rank) (player : bool) =
object(self)
end ;;
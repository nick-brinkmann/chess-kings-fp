(* ............................................................................
                        Class type for pieces
   ......................................................................... *)
class piece (initfile : int) (initrank : int) (player : bool) = 
  object (self)
    (* player to which the piece belongs *) 
    val player = color 




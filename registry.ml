(*

-- registering and deregistering pieces
-- obtaining a list of all currently registered pieces
-- obtaining currently registered piece on a certain square.

Stores the registered objects in two data structures, a `Set` for easy
access to the full set of objects and a 2D array by location allowing
for more efficient access to the neighboring objects. *)

open Array ;;
open CS51Utils ;;
open Game ;;
module G = Graphics ;;

(* module Viz = Visualization ;; *)
  
(*....................................................................
  The objects in the world
 *)
 
class type piece_type =
  object
    method get_pos : coordinate

    method make_move : coordinate -> unit

    method draw : unit

    method get_color : bool

  end ;;

(*....................................................................
  A piece_type registry
 *)

module type REGISTRY =
  sig
    (* register obj -- Adds the `obj` to both the registry and the
       map. *)
    val register : piece_type -> unit
      
    (* deregister obj -- Removes the `obj` from both data
       structures. Raises `Not_found` if the `obj` was not previously
       registered. *)
    val deregister : piece_type -> unit

    (* get_pieces () -- Returns a list of all of the curently 
        registered pieces. *)
    val get_pieces : unit -> piece_type list
    
    (* Returns piece_type option:
        - None if there is no piece on corresponding square
        - Some Piece if a piece is found *)
    val find_piece : coordinate -> (piece_type option)

    (* contains_enemy_piece your_color coordinate -- returns true if square 
      contains an enemy piece, else false *)
    val contains_enemy_piece : bool -> coordinate -> bool

    (* returns true if square contains a friendly piece, else false *)
    val contains_own_piece : bool -> coordinate -> bool 

    (* returns true if square contains any piece *)
    val contains_any_piece : coordinate -> bool
  end

module Registry : REGISTRY =
  struct
    (* registrants -- An updatable set of all of the registered objects *)
    module Registrants =
      Set.Make (struct type t = piece_type
                       let compare = compare
                end) ;;

    let registrants = ref Registrants.empty ;;

    (* position -- A "map" of all the pieces, organized by
       2-D location *)
    let position : (piece_type option) array array =
      make_matrix 8 8 None ;;

    (* The required REGISTRY functions. See REGISTRY module signature
       for documentation *)
      
    let register (obj : piece_type) : unit =
      registrants := Registrants.add obj !registrants;
      let f, r = obj#get_pos in
      let fi = file_to_int f in
      let ra = rank_to_int r in
      position.(fi).(ra) <- Some obj ;;

    let deregister (obj : piece_type) : unit =
      let new_registrants = Registrants.remove obj !registrants in
      if new_registrants == !registrants then
        (* no obj removed; as of v4.03, physical equality guaranteed *)
        raise (Invalid_argument "deregister: object not in registry to begin with")
      else
        begin
          registrants := new_registrants;
          let f, r = obj#get_pos in
          let fi = file_to_int f in
          let ra = rank_to_int r in
          position.(fi).(ra) <- None
        end ;;
      
    let get_pieces () = Registrants.elements !registrants ;;
  
    (* let find_piece coord = 
      let subset = Registrants.filter (fun obj -> obj#get_pos = coord) !registrants in
      Registrants.choose_opt subset
    ;; *)

    let find_piece (c : coordinate) : piece_type option =
      let fi, ra = coord_to_int c in 
      position.(fi).(ra) ;;

    let contains_enemy_piece (your_color : bool) (coord : coordinate) : bool = 
      match find_piece coord with 
      | None -> false 
      | Some piece -> piece#get_color <> your_color ;;

    let contains_own_piece (your_color : bool) (coord : coordinate) : bool = 
      match find_piece coord with 
      | None -> false 
      | Some piece -> piece#get_color = your_color ;;

    let contains_any_piece (coord : coordinate) : bool = 
      match find_piece coord with 
      | None -> false 
      | Some _piece -> true ;;

    (* checks whether there's a piece along the given line or diagonal *)
    let is_piece_along_line_from (start_coord : coordinate) 
                                 (end_coord : coordinate) : bool =
      (* turns into int representation *)
      let sf, sr = coord_to_int start_coord in
      let ef, er = coord_to_int end_coord in

      (* helper function to generate list *)

      (* checks that you're using a vertical line *)
      if sf - ef = 0 then 
        begin

        end




  end
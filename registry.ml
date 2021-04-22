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

    method update_pos : coordinate -> unit
    method draw : unit
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
  
    (* occupied -- Return piece at given location*)
    (* val occupied : coordinate -> piece_type option *)
  end

module Registry : REGISTRY =
  struct
    (* registrants -- An updatable set of all of the registered objects *)
    module Registrants =
      Set.Make (struct type t = piece_type
                       let compare = compare
                end) ;;

    let registrants = ref Registrants.empty ;;

    (* map -- A "map" of all the registered objects, organized by
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
      position.(fi - 1).(ra - 1) <- Some obj ;;

    let deregister (obj : piece_type) : unit =
      let new_registrants = Registrants.remove obj !registrants in
      if new_registrants == !registrants then
        (* no obj removed; as of v4.03, physical equality guaranteed *)
        raise Not_found
      else
        begin
          registrants := new_registrants;
          let f, r = obj#get_pos in
          let fi = file_to_int f in
          let ra = rank_to_int r in
          position.(fi - 1).(ra - 1) <- None
        end ;;
      
    let get_pieces () = Registrants.elements !registrants ;;
  
    (* let occupied (obj : piece_type) : piece_type list =
      let dist_squared = cNEIGHBOR_RADIUS * cNEIGHBOR_RADIUS in
      let x, y = obj#pos in
      
      (* all coordinates in a block around `obj` *)
      let open Utilities in
      cross_product (Absbook.range (x_bounded (x - cNEIGHBOR_RADIUS))
                                   (x_bounded (x + cNEIGHBOR_RADIUS)))
                    (Absbook.range (y_bounded (y - cNEIGHBOR_RADIUS))
                                   (y_bounded (y + cNEIGHBOR_RADIUS)))
      (* extract object lists at those coordinates if within the
         radius *)
      |> List.map (fun (nx, ny) ->
                   if (nx - x) * (nx - x) + (ny - y) * (ny - y)
                      <= dist_squared
                   then
                     map.(nx).(ny)
                   else [])
      (* and concatenate them *)
      |> List.concat ;; *)

  end
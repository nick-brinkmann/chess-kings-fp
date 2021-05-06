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
    method is_king : bool

    method get_pos : coordinate

    method make_move : coordinate -> unit

    method draw : unit

    method name : piece_name

    method get_color : bool

    method get_moves : int

    method can_be_valid_move : coordinate -> bool

    method attacks_square : coordinate -> bool

    method chebyshev_distance_to : coordinate -> int

  end ;;

(* Global variable for whether en-passant is possible. *)
type move_memory = 
{
mutable player : bool;
mutable piece : piece_name;
mutable start_square : coordinate;
mutable end_square : coordinate;
mutable state : game_state
}
;;

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

    (* When called, checks whose turn it is to play by checking boolean *)
    val turn : unit -> bool

    (* flips whose move it is *)
    val flip_turn : unit -> unit

    (* Copies the current registry to allow takeback of moves *)
    val copy_pieces : unit -> piece_type list
    
    (* Stores copy of previous state of the game into 'prev_positions' 
        list, and flips boolean to indicate next players turn *)
    val take_turn : piece_type -> coordinate -> unit

    (* Returns piece_type option:
        - None if there is no piece on corresponding square
        - Some Piece if a piece is found *)
    val find_piece : coordinate -> (piece_type option)

    
    (* Returns pieces of the given color *)
    val subset : bool -> (piece_type list)

    (* contains_enemy_piece your_color coordinate -- returns true if square 
      contains an enemy piece, else false *)
    val contains_enemy_piece : bool -> coordinate -> bool

    (* returns true if square contains a friendly piece, else false *)
    val contains_own_piece : bool -> coordinate -> bool 

    (* returns true if square contains any piece *)
    val contains_any_piece : coordinate -> bool

    (* is_piece_along_line_from square1 square2 -- returns true if there is 
      any piece on the line from square1 to square2 EXCLUDING the starting and 
      ending squares *)
    val is_piece_along_line_from : coordinate -> coordinate -> bool

    (* is_player_in_check player -- returns true if the player of the given 
      color is currently in check. *)
    val player_not_in_check : bool -> bool

    (* prints the registry *)
    val print_registry : unit -> unit

    (* takes back the last move *)
    val take_back : unit -> unit 

    (* returns the last move, if possible *)
    val last_move : unit -> move_memory option

    (* because of quirk of storage, need access to this for en passant *)
    val two_moves_ago : unit -> move_memory option

    (* checks if position is stalemate *)
    val check_stalemate : unit -> bool

    (* checks if position is checkmate *)
    val checkmate_check : unit -> bool

    (* sets the state of the game to one of Play, Check, Checkmate, or Stalemate *)
    val set_state : game_state -> unit

    (* gets current state of the game *)
    val get_state : unit -> game_state

    (* determines whether working on real registry or not *)
    val is_real_board : unit -> bool
  end

(* order function of Set*)
let order_pieces (p1 : piece_type) (p2 : piece_type) = 
  if p1 == p2 then 0 else compare p1 p2 ;;

module Registry : REGISTRY =
  struct
    (* registrants -- A set of all of the registered objects *)
    module Registrants =
      Set.Make (struct type t = piece_type
                       let compare = order_pieces
                end) ;;

    (* initialize set of pieces to empty set *)
    let registrants = ref Registrants.empty ;;

    let print_registry () : unit = 
      let color_to_string (b : bool) : string = 
        if b then "white" else "black" in
      let all_pieces = Registrants.elements !registrants in 
      let num_pieces = Registrants.cardinal !registrants in
      let rec print_list (lst : piece_type list) : unit =
        match lst with 
        | [] -> ()
        | hd :: tl -> 
          Printf.printf "%s %s %s \n" 
          (color_to_string hd#get_color) 
          (piece_name_to_string hd#name)
          (coord_to_string hd#get_pos);
          print_list tl
      in
      print_list all_pieces;
      Printf.printf "%d \n" num_pieces;
      Printf.printf "-------------------------\n"
    ;;

    (* whose_turn -- controls whose move it is *)
    let whose_turn = ref true ;;

    (* initialize game state to play state *)
    let state = ref Play ;;

    let set_state (s : game_state) : unit =
      state := s ;;
    
    let get_state () : game_state = !state;;

    (* initialize real_board to true *)
    let real_board = ref true ;;
    
    let is_real_board () = !real_board ;;
    

    (* turn () : checks the number of moves that have been made thus far. If
                even then it's white's turn, otherwise it's black's turn *)
    let turn () =
      !whose_turn ;;
      
      let flip_turn () = 
        whose_turn := not !whose_turn ;;
        
        (* let prev_positions = ref [] ;; *)

    (* move history stores a list of:
      - moves (player, piece, start and end square) 
      - the positions that those moves result in, 
          stored as the registry in list form.
      Initialized as the empty list.
    *)
    let move_history : (move_memory option * (piece_type list)) list ref = 
      ref [] ;;
        
    (* register obj : updates the registry by adding a new piece *)
    let register (obj : piece_type) : unit =
      registrants := (Registrants.add obj !registrants) ;;

    (*  deregister obj : updates registry by removing a piece, alerts user if
                        attempting to remove piece not in registry *)
    let deregister (obj : piece_type) : unit =
      Printf.printf "Deregistering %s at %s now \n" 
      (piece_name_to_string obj#name) (coord_to_string obj#get_pos);
      let new_registrants = Registrants.remove obj !registrants in
      if new_registrants == !registrants then
        (* no obj removed; as of v4.03, physical equality guaranteed *)
        (print_registry ();
        raise (Invalid_argument "deregister: object not in registry to begin with"))
      else
        begin
          registrants := new_registrants;
        end ;;

    (* get_pieces () : Returns a list of the pieces in the registry *)
    let get_pieces () = Registrants.elements !registrants ;;


    (* last_move () -- if there has been a last move (i.e. the game has started) 
      returns the move. Because of a quirk of storage, the real last move is the 
      hd of the tl of the list (2nd in the list). *)
    let last_move () : move_memory option = 
      match !move_history with 
      | [] -> None
      | (move, _position) :: _tl -> move
      (* | [_elt] -> None
      | (_move1, _position1) :: (move2, _position2) :: _tl -> move2 *)
    ;;

    let two_moves_ago () : move_memory option = 
      match !move_history with 
      | [] -> None 
      | [_elt] -> None 
      | (_move1, _pos1) :: (move2, _pos2) :: _tl -> move2 
    ;;

    let find_piece coord : piece_type option = 
      (* filter piece registry by pieces on coord *)
      let subset = Registrants.filter (fun obj -> obj#get_pos = coord) !registrants in
      (* if the subset has more than one piece, then we have broken an invariant *)
      if Registrants.cardinal subset > 1 then
        raise (Invalid_argument "find_piece: Multiple pieces on the same square")
      else
        Registrants.choose_opt subset
    ;;

    (* copy_pieces (): helper function that returns a list of pieces as they 
                       currently are in the registry, essentially taking a copy
                       of the current state of the game *)
    let copy_pieces () : piece_type list = 
      List.map Oo.copy (get_pieces ())
    ;;


    let take_turn (piece : piece_type) (going_to : coordinate) : unit =
      (* initializes move_history if empty *)
      (* if !move_history = [] then 
        move_history := [None, copy_pieces ()]; *)
      (* adds a move to the move history. *)
      let add_move (piece : piece_type) (coord : coordinate) : unit = 
        let to_add : move_memory = 
          {
            player = piece#get_color;
            piece = piece#name;
            start_square = piece#get_pos;
            end_square = coord;
            state = !state
          }
        in
        move_history := (Some to_add, copy_pieces ()) :: !move_history 
      in
      add_move piece going_to;
      (* prev_positions := (copy_pieces ()) :: !prev_positions; *)
      flip_turn ()
    ;;

    (* take_back () -- takes back the last move, if possible. *)
    let take_back () =
      (* if no moves made yet, do nothing. otherwise take back a move. *)
      match !move_history with 
      | [] -> ()
      | (last_move, position) :: tl ->
        (
          (* empty registry and repopulate it *)
          registrants := Registrants.empty;
          List.iter (fun obj -> register obj) position;
          (* remove head from move_history *)
          move_history := tl;
          (* set turn back to other player *)
          flip_turn ();
          (* set game state to previous state *)
          match last_move with 
          | None -> ()
          | Some move -> set_state move.state
        )
    ;;

    (* filter pieces by color of player, returning a list. *)
    let subset (color : bool) : piece_type list = 
      let s  = Registrants.filter (fun obj -> obj#get_color = color) !registrants in
      Registrants.elements s ;;

    (* finds piece on coordinate (if any), returns true if piece exists
          and is of opposite color to specified player *)
    let contains_enemy_piece (your_color : bool) (coord : coordinate) : bool = 
      match find_piece coord with 
      | None -> false 
      | Some piece -> piece#get_color <> your_color ;;
      
    (* finds piece on coordinate (if any), returns true if piece exists
        and is of same color to specified player *)
    let contains_own_piece (your_color : bool) (coord : coordinate) : bool = 
      match find_piece coord with 
      | None -> false 
      | Some piece -> piece#get_color = your_color ;;

    (* determines whether a square contains a piece. *)
    let contains_any_piece (coord : coordinate) : bool = 
      match find_piece coord with 
      | None -> false 
      | Some _piece -> true ;;

    (* checks whether there's a piece along the given line or diagonal
       EXCLUDING the starting square AND EXCLUDING the ending square. *)
    let is_piece_along_line_from (start_coord : coordinate) 
                                 (end_coord : coordinate) : bool =
      (* turns into int representation *)
      let sf, sr = coord_to_int start_coord in (* starting file and rank *)
      let ef, er = coord_to_int end_coord in (* ending file and rank *)

      (* checks that the coordinates given do, in fact, form a line *)
      if not ((sf - ef = 0) (* vertical *)
            || (sr - er = 0) (* horizontal *)
            || (abs (sf - ef) = abs (sr - er))) (* diagonal *) then 
      raise (Invalid_argument "is_piece_along_line_from: not a line or diagonal");

      let dist_x = ef - sf in (* horizontal distance travelled *)
      let dist_y = er - sr in (* vertical distance *)

      (* helper function to generate list of squares to check *)
      let rec make_lst_to_check 
        (start_x : int) (start_y : int) (* starting square *)
        (dx : int) (dy : int) (* distances needed to travel in x and y direction *)
        (acc : (int * int) list) =

        (* returns a length 1 step in direction of z *)
        let norm (z : int) : int = 
          if z = 0 then 0 else z / (abs z) in 

        (* finished moving *)
        if dx = 0 && dy = 0 then acc else (* note this EXCLUDES ending square *) 
        let norm_x = norm dx in 
        let norm_y = norm dy in 
        make_lst_to_check (start_x + norm_x) (start_y + norm_y) 
                     (dx - norm_x) (dy - norm_y) ((start_x, start_y) :: acc)
        (* note this INCLUDES the starting square. We manually remove it in the
           next lines *)
      in 
      let lst_to_check = make_lst_to_check sf sr dist_x dist_y [] 
        |> List.rev 
        |> List.tl (* removes starting square. MAYBE BAD DESIGN *)
      in 
      lst_to_check
        |> (List.map int_to_coord) (* converts to coordinate list *)
        |> (List.map contains_any_piece) (* checks whether piece on each square. now bool list *)
        |> (List.for_all not) (* returns true if and only if all elements are false *)
        |> not (* if above was true, then NO pieces were in between. So we flip *)
    ;;

    (* Checks if player is in check by checking whether pieces are attacking
          the square that their king is occupying *)
    let player_not_in_check (player : bool) : bool = 
      (* Gets list of opponent piece objects *)
      let opponent_pieces = subset (not player) in 
      (* Gets opponent king from list *)
      let opp_king = List.find (fun obj -> obj#is_king) opponent_pieces in
      (* Removes opponent king from list *)
      let opp_pieces_not_king = 
        List.filter (fun obj -> not obj#is_king) opponent_pieces in 
      (* Get player's king *)
      let my_king = 
        subset player 
        |> List.find (fun obj -> obj#is_king)
      in
      (* Gets king's position *)
      let my_king_pos = my_king#get_pos in
      (* If no pieces can move to king's position, and the opponent king's square is not
            adjacent to king's square, then return true *)
      (List.for_all (fun obj -> not (obj#can_be_valid_move my_king_pos)) opp_pieces_not_king) &&
      (opp_king#chebyshev_distance_to my_king_pos > 1)
    ;;


    (* Checks all of current player's pieces to see if move exists.
       This modifies the objects properties, so we copy registry
       beforehand and reset it after all testing is done. *)
    let valid_moves_exist () : bool =
      (* sets board to virtual board, so promotion is not triggered
          when checking all possible moves *)
      real_board := false;
      (* Copy of current registry *)
      let curr_registry = copy_pieces () in 

      (* checks if a single piece has any valid moves *)
      let single_piece_valid_moves (piece : piece_type) : bool = 
        let next_ghost_piece = 
          match find_piece piece#get_pos with
          | None -> raise (Invalid_argument "error in single_piece_valid_moves")
          | Some piece -> piece in
        let curr_ghost_piece = ref next_ghost_piece in
        if !curr_ghost_piece#get_color <> !whose_turn then
          false
        else
          begin
            let has_valid_move = ref false in   (* used to break out of loop as soon as move is found *)
            let i = ref 0 in                    (* iterates through possible files *)
            let j = ref 0 in                    (* iterates through possible ranks *)
            (* iterate through all squares on chessboard, if any square is 
                a valid move break out of loop *)
            while (not !has_valid_move) && (!i < 8) do
              while (not !has_valid_move) && (!j < 8) do
                let (f, r) = int_to_coord (!i, !j) in
                j := !j + 1;
                if !curr_ghost_piece#can_be_valid_move (f, r) then
                  (* make provisional move and verify that wouldn't put player in check *)
                  (* remember current position so we can update correctly later *)
                  (let prev = !curr_ghost_piece#get_pos in
                  take_turn !curr_ghost_piece (f, r);
                  !curr_ghost_piece#make_move (f, r);
                  let _at_d8 = 
                    match find_piece (D, R8) with
                    | None -> Printf.printf "There ain't shit at D8 \n"
                    | Some piece -> Printf.printf "Found %s at D8\n" (piece_name_to_string piece#name)
                  in
                  if player_not_in_check (not !whose_turn) then
                    begin
                      has_valid_move := true;
                      Printf.printf "valid move is %s %s-%s for %b \n"
                      (piece_name_to_string !curr_ghost_piece#name)
                      (coord_to_string prev)
                      (coord_to_string !curr_ghost_piece#get_pos) 
                      (not !whose_turn);
                      let _dat_rook = 
                        match find_piece (D,R1) with 
                        | None -> Printf.printf "rook is a hoax \n"
                        | Some piece -> Printf.printf "rook is %b %s. Can
                        move to d8?: %b \n"
                        (piece#get_color) (piece_name_to_string piece#name)
                        (piece#can_be_valid_move (D,R8))
                      in
                      if get_state () = Checkmate || get_state () = Check then 
                        Printf.printf "Thinks this gets out of check: %s %s \n"
                      (piece_name_to_string !curr_ghost_piece#name)
                      (coord_to_string !curr_ghost_piece#get_pos)
                    end;
                    
                  take_back ();
                  let next_next_ghost_piece = 
                    (match find_piece prev with
                    | None -> raise (Invalid_argument "error in has_valid_move")
                    | Some piece -> piece)
                  in
                  curr_ghost_piece := next_next_ghost_piece);
              done;
              i := !i + 1;
              j := 0;
            done;
            !has_valid_move;
          end
      in
      
      (* check if any piece has a valid move *)
      let exists_move = Registrants.exists single_piece_valid_moves !registrants in
      (* resets registry to state before testing *)
      registrants := Registrants.empty;
      List.iter (fun obj -> register obj) curr_registry;
      (* reset boolean so that promotion is once again considered for pawns *)
      real_board := true;
      exists_move
    ;;

    (* If the current player has no valid moves, and the king is currently 
        under attack, return true. Otherwise false *)
    let checkmate_check () : bool = 
      (not (valid_moves_exist ())) && (not (player_not_in_check !whose_turn)) 
    ;;

    (* If the current player has no valid moves, and the king is not currently
        under attack, return true. Otherwise false *)
    let check_stalemate () : bool =
      (not (valid_moves_exist ())) && player_not_in_check !whose_turn
    ;;
    
  end
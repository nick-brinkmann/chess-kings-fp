(* White is true, Black is false *)
(* type player = bool ;; *)
type rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 ;;
type file = A | B | C | D | E | F | G | H ;;
type coordinate = file * rank ;;
type piece_name = Piece | Pawn | Knight | Bishop | Rook | Queen | King ;;
type game_state = Play | Check | Checkmate | Stalemate ;;

(* file_to_int f -- returns zero-indexed int *)
let file_to_int (f : file) : int = 
  match f with 
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3
  | E -> 4
  | F -> 5
  | G -> 6
  | H -> 7 ;;

(* rank_to_int r -- returns zero-indexed int *)
let rank_to_int (r : rank) : int =
  match r with 
  | R1 -> 0
  | R2 -> 1
  | R3 -> 2
  | R4 -> 3
  | R5 -> 4
  | R6 -> 5
  | R7 -> 6
  | R8 -> 7 ;;

let coord_to_int (file, rank : coordinate) : int * int = 
  (file_to_int file, rank_to_int rank) ;;

let int_to_file (i : int) : file = 
    match i with
    | 0 -> A
    | 1 -> B
    | 2 -> C
    | 3 -> D
    | 4 -> E
    | 5 -> F
    | 6 -> G
    | 7 -> H 
    | _ -> raise (Invalid_argument "Not a valid file") ;;

let int_to_rank (i : int) : rank = 
    match i with
    | 0 -> R1
    | 1 -> R2
    | 2 -> R3
    | 3 -> R4
    | 4 -> R5
    | 5 -> R6
    | 6 -> R7
    | 7 -> R8
    | _ -> raise (Invalid_argument "Not a valid rank") ;;

let int_to_coord (fi, ra : int * int) : coordinate =
  (int_to_file fi, int_to_rank ra) ;;

let file_to_string (f : file) : string =
  match f with
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"
  | G -> "G"
  | H -> "H" ;;

let rank_to_string (r : rank) : string = 
  match r with
  | R1 -> "1"
  | R2 -> "2"
  | R3 -> "3"
  | R4 -> "4"
  | R5 -> "5"
  | R6 -> "6"
  | R7 -> "7"
  | R8 -> "8" ;;

let coord_to_string (f, r : coordinate) : string =
  (file_to_string f) ^ (rank_to_string r) ;;

let is_on_board (x, y : int * int) : bool = 
  x >= 0 && x <= 7 && y >= 0 && y <= 7 ;;

let piece_name_to_string (p : piece_name) : string = 
  match p with 
  | Piece -> "Piece"
  | Pawn -> "Pawn"
  | Knight -> "Knight"
  | Bishop -> "Bishop"
  | Rook -> "Rook"
  | Queen -> "Queen"
  | King -> "King"
;;
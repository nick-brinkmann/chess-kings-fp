(* Module file that handles all the game elements *)
type player = bool ;;
type rank = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 ;;
type file = A | B | C | D | E | F | G | H ;;
type coordinate = file * rank ;;
type pieceType = Pawn | Knight | Bishop | Rook | Queen | King ;;
type piece = None | Piece of pieceType * player ;;
type square = coordinate * piece ;;
type position = square array array ;;

let new_piece (pt : pieceType) (pl : player) : piece =
  Piece (pt, pl) ;;

let new_square (c : coordinate) (p : piece) : square = 
  c, p ;;

  

let init_position () : position = 
  [| [| (A, 1), (new_piece Rook White); (B, 1), ; (C, 1),  ; (D, 1), ; (E, 1), ; (F, 1), ; (G, 1), ; (H, 1), |];
     [| (A, 2), (new_piece ) |]
  |] ;;




let file_to_int (f : file) : int = 
  match f with 
  | A -> 1
  | B -> 2
  | C -> 3
  | D -> 4
  | E -> 5
  | F -> 6
  | G -> 7
  | H -> 8 ;;

let int_to_file (i : int) : file =
  match i with 
  | 1 -> A
  | 2 -> B
  | 3 -> C
  | 4 -> D
  | 5 -> E
  | 6 -> F
  | 7 -> G
  | 8 -> H ;;

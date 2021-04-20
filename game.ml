open Pieces ;;

(* Module file that handles all the game elements *)
(* White is true, Black is false *)
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

  

let init_position : position = 
  [| 
    [| (A, 8), (new rook A 8 false); (B, 8), (new knight B 8 false); (C, 8), (new bishop C 8 false)  ; (D, 8), (new queen D 8 false); 
            (E, 8), (new king E 8 false); (F, 8), (new bishop F 8 false); (G, 8), (new knight G 8 false) ; (H, 8), (new rook H 8 false) |];
    [| (A, 7), (new pawn A 7 false) ; (B, 7), (new pawn B 7 false) ; (C, 7), (new pawn C 7 false) ; (D, 7), (new pawn D 7 false);
            (E, 7), (new pawn E 7 false) ; (F, 7), (new pawn F 7 false); (G, 7), (new pawn G 7 false); (H, 7), (new pawn H 7 false) |];
    [| (A, 6), ; (B, 6), ; (C, 6),  ; (D, 6), ; (E, 6), ; (F, 6), ; (G, 6), ; (H, 6), |];
    [| (A, 5), ; (B, 5), ; (C, 5),  ; (D, 5), ; (E, 5), ; (F, 5), ; (G, 5), ; (H, 5), |];
    [| (A, 4), ; (B, 4), ; (C, 4),  ; (D, 4), ; (E, 4), ; (F, 4), ; (G, 4), ; (H, 4), |];
    [| (A, 3), ; (B, 3), ; (C, 3),  ; (D, 3), ; (E, 3), ; (F, 3), ; (G, 3), ; (H, 3), |];
    [| (A, 2), (new pawn A 2 true); (B, 2), (new pawn B 2 true); (C, 2), (new pawn C 2 true) ; (D, 2), (new pawn D 2 true); 
            (E, 2), (new pawn E 2 true); (F, 2), (new pawn F 2 true); (G, 2), (new pawn G 2 true); (H, 2), (new pawn H 2 true) |];
    [| (A, 1), (new rook A 1 true) ; (B, 1), (new knight B 1 true) ; (C, 1), (new bishop C 1 true) ; (D, 1), (new queen D 1 true);
            (E, 1), (new king E 1 true); (F, 1), (new bishop F 1 true) ; (G, 1), (new knight G 1 true); (H, 1), (new rook H 1 true) |];
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

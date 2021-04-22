(* open Pieces ;; *)

(* Module file that handles all the game elements *)
(* White is true, Black is false *)
type player = bool ;;
type rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 ;;
type file = A | B | C | D | E | F | G | H ;;
type coordinate = file * rank ;;
(* type pieceType = Pawn | Knight | Bishop | Rook | Queen | King ;;
type piece = None | Piece of pieceType * player ;; *)
(* type square = coordinate * piece ;; *)
(* type position = square array array ;; *)

(* let new_piece (pt : pieceType) (pl : player) : piece =
  Piece (pt, pl) ;;

let new_square (c : coordinate) (p : piece) : square = 
  c, p ;; *)


  

(* let init_position : position = 
  [| 
    [| (A, R8), (new rook A R8 false); (B, R8), (new knight B R8 false); (C, R8), (new bishop C R8 false)  ; (D, R8), (new queen D R8 false); 
            (E, R8), (new king E R8 false); (F, R8), (new bishop F R8 false); (G, R8), (new knight G R8 false) ; (H, R8), (new rook H R8 false) |];
    [| (A, R7), (new pawn A R7 false) ; (B, R7), (new pawn B R7 false) ; (C, R7), (new pawn C R7 false) ; (D, R7), (new pawn D R7 false);
            (E, R7), (new pawn E R7 false) ; (F, R7), (new pawn F R7 false); (G, R7), (new pawn G R7 false); (H, R7), (new pawn H R7 false) |];
    [| (A, R6), None ; (B, R6), None; (C, R6), None ; (D, R6), None ; (E, R6), None ; (F, R6), None ; (G, R6), None ; (H, R6), None |];
    [| (A, R5), None ; (B, R5), None; (C, R5), None ; (D, R5), None ; (E, R5), None ; (F, R5), None ; (G, R5), None ; (H, R5), None|];
    [| (A, R4),None ; (B, R4),None ; (C, R4), None ; (D, R4), None; (E, R4), None; (F, R4),None ; (G, R4),None ; (H, R4), None |];
    [| (A, R3), None ; (B, R3),None  ; (C, R3),None   ; (D, R3),None  ; (E, R3),None  ; (F, R3),None  ; (G, R3),None  ; (H, R3),None  |];
    [| (A, R2), (new pawn A R2 true); (B, R2), (new pawn B R2 true); (C, R2), (new pawn C R2 true) ; (D, R2), (new pawn D R2 true); 
            (E, R2), (new pawn E R2 true); (F, R2), (new pawn F R2 true); (G, R2), (new pawn G R2 true); (H, R2), (new pawn H R2 true) |];
    [| (A, R1), (new rook A R1 true) ; (B, R1), (new knight B R1 true) ; (C, R1), (new bishop C R1 true) ; (D, R1), (new queen D R1 true);
            (E, R1), (new king E R1 true); (F, R1), (new bishop F R1 true) ; (G, R1), (new knight G R1 true); (H, R1), (new rook H R1 true) |]
  |] ;; *)



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

let rank_to_int (i : rank) : int =
  match i with 
  | R1 -> 1
  | R2 -> 2
  | R3 -> 3
  | R4 -> 4
  | R5 -> 5
  | R6 -> 6
  | R7 -> 7
  | R8 -> 8 ;;

let int_to_file (i : int) : file = 
  if i < 1 || i > 8 then raise (Invalid_argument "Not a valid rank")
  else
    match i with
    | 1 -> A
    | 2 -> B
    | 3 -> C
    | 4 -> D
    | 5 -> E
    | 6 -> F
    | 7 -> G
    | 8 -> H ;;
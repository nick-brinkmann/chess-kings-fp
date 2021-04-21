(* open Pieces ;; *)

(* Module file that handles all the game elements *)
(* White is true, Black is false *)
type player = bool ;;
type rank = One | Two | Three | Four | Five | Six | Seven | Eight ;;
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

  

(* let init_position : position = 
  [| 
    [| (A, Eight), (new rook A Eight false); (B, Eight), (new knight B Eight false); (C, Eight), (new bishop C Eight false)  ; (D, Eight), (new queen D Eight false); 
            (E, Eight), (new king E Eight false); (F, Eight), (new bishop F Eight false); (G, Eight), (new knight G Eight false) ; (H, Eight), (new rook H Eight false) |];
    [| (A, Seven), (new pawn A Seven false) ; (B, Seven), (new pawn B Seven false) ; (C, Seven), (new pawn C Seven false) ; (D, Seven), (new pawn D Seven false);
            (E, Seven), (new pawn E Seven false) ; (F, Seven), (new pawn F Seven false); (G, Seven), (new pawn G Seven false); (H, Seven), (new pawn H Seven false) |];
    [| (A, Six), None ; (B, Six), None; (C, Six), None ; (D, Six), None ; (E, Six), None ; (F, Six), None ; (G, Six), None ; (H, Six), None |];
    [| (A, Five), None ; (B, Five), None; (C, Five), None ; (D, Five), None ; (E, Five), None ; (F, Five), None ; (G, Five), None ; (H, Five), None|];
    [| (A, Four),None ; (B, Four),None ; (C, Four), None ; (D, Four), None; (E, Four), None; (F, Four),None ; (G, Four),None ; (H, Four), None |];
    [| (A, Three), None ; (B, Three),None  ; (C, Three),None   ; (D, Three),None  ; (E, Three),None  ; (F, Three),None  ; (G, Three),None  ; (H, Three),None  |];
    [| (A, Two), (new pawn A Two true); (B, Two), (new pawn B Two true); (C, Two), (new pawn C Two true) ; (D, Two), (new pawn D Two true); 
            (E, Two), (new pawn E Two true); (F, Two), (new pawn F Two true); (G, Two), (new pawn G Two true); (H, Two), (new pawn H Two true) |];
    [| (A, One), (new rook A One true) ; (B, One), (new knight B One true) ; (C, One), (new bishop C One true) ; (D, One), (new queen D One true);
            (E, One), (new king E One true); (F, One), (new bishop F One true) ; (G, One), (new knight G One true); (H, One), (new rook H One true) |]
  |] ;; *)



(* let file_to_int (f : file) : int = 
  match f with 
  | A -> 1
  | B -> Two
  | C -> Three
  | D -> Four
  | E -> Five
  | F -> Six
  | G -> Seven
  | H -> Eight ;;

let int_to_file (i : int) : file =
  match i with 
  | 1 -> A
  | Two -> B
  | Three -> C
  | Four -> D
  | Five -> E
  | Six -> F
  | Seven -> G
  | Eight -> H ;; *)

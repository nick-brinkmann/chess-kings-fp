open Game ;;
open Params ;;
module T = Registry ;;
module R = T.Registry ;;
module P = Pieces ;;
module Viz = Visualization ;;
  
let test_array = [|
    [| 1; 1; 1; 1; 1; 1; 1; 1|];
    [| 1; 1; 1; 1; 1; 1; 1; 1|];
    [| 1; 1; 1; 1; 1; 1; 1; 1|];
    [| 1; 1; 1; 1; 1; 1; 1; 1|];
    [| 1; 1; 1; 1; 1; 1; 1; 1|];
    [| 1; 1; 1; 1; 1; 1; 1; 1|];
    [| 1; 1; 1; 1; 1; 1; 1; 1|];
    [| 1; 1; 1; 1; 1; 1; 1; 1|];
|]

let initialize_pieces () = 
  (* initialize pawns *)
  for i = 0 to 7 do
    let f = int_to_file i in
    R.register ((new P.pawn f R2 true) :> T.piece_type);
    R.register ((new P.pawn f R7 false) :> T.piece_type);
  done;

  (* initialize white pieces *)
  R.register ((new P.rook A R1 true) :> T.piece_type);
  R.register ((new P.rook H R1 true) :> T.piece_type);
  R.register ((new P.knight B R1 true) :> T.piece_type);
  R.register ((new P.knight G R1 true) :> T.piece_type);
  R.register ((new P.bishop C R1 true) :> T.piece_type);
  R.register ((new P.bishop F R1 true) :> T.piece_type);
  R.register ((new P.queen D R1 true) :> T.piece_type);
  R.register ((new P.king E R1 true) :> T.piece_type);

  (* initialize black pieces *)
  R.register ((new P.rook A R8 false) :> T.piece_type);
  R.register ((new P.rook H R8 false) :> T.piece_type);
  R.register ((new P.knight B R8 false) :> T.piece_type);
  R.register ((new P.knight G R8 false) :> T.piece_type);
  R.register ((new P.bishop C R8 false) :> T.piece_type);
  R.register ((new P.bishop F R8 false) :> T.piece_type);
  R.register ((new P.queen D R8 false) :> T.piece_type);
  R.register ((new P.king E R8 false) :> T.piece_type)
;;


let run () =
  Viz.initialize ();

  initialize_pieces ();
  
  let end_game = ref false in
  while not !end_game do
    Graphics.clear_graph ();
    (* Viz.draw_board test_array (Viz.cX_DIMENSION) (Viz.cY_DIMENSION);
    Viz.light_up (); *)
    Viz.render  (R.get_pieces ()) 
                test_array ;

    if not (Viz.move_on ()) then end_game := true
  done;;

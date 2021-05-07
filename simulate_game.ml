open Game ;;
open Params ;;
module T = Registry ;;
module R = T.Registry ;;
module P = Pieces ;;
module Viz = Visualization ;;

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

    Viz.render () ;

    if R.get_state () = Checkmate || R.get_state () = Stalemate then
      (Viz.draw_board ();
      Viz.draw_square G.green 7 9 cSQUARE_WIDTH cSQUARE_HEIGHT;
      G.set_color G.black;
      G.moveto (9 * cSQUARE_WIDTH + (cSQUARE_WIDTH / 2))
       (7 * cSQUARE_HEIGHT + (cSQUARE_HEIGHT / 2));
      G.draw_string "Exit";
      let s = G.wait_next_event [G.Button_down] in
      let x, y = s.mouse_x , s.mouse_y in
      (* we clicked the quit button *)
      if (x > 9 * cSQUARE_WIDTH) && 
         (x < 10 * cSQUARE_WIDTH + cSQUARE_WIDTH) && 
         (y > 7 * cSQUARE_HEIGHT) && 
         (y < 8 * cSQUARE_HEIGHT) then
        end_game := true
      else
        Viz.clicked_take_back (x, y));

  done;;

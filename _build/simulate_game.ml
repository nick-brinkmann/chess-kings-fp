open Game ;;
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


let run () =
  Viz.initialize ();

  let leave_early = ref false in
  while not !leave_early do
    Viz.draw_board (* init_position *) test_array (Viz.cX_DIMENSION) (Viz.cY_DIMENSION);

    Graphics.set_color Graphics.red;
    Graphics.moveto 150 150;

    Graphics.draw_string "K";
    if Viz.any_key () then leave_early := true
  done;;


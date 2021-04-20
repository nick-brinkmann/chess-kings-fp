open Game ;;
module Viz = Visualization ;;
  
let run () =
  Viz.initialize ();

  let leave_early = ref false in
  while not !leave_early do
    Viz.draw_board init_position (cX_DIMENSION / 8) (cY_DIMENSION / 8);
    if Viz.any_key () then leave_early := true
  done;;


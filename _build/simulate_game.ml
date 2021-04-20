
module Viz = Visualization ;;
  
let run () =
  Viz.initialize ();

  let leave_early = ref false in
  while not !leave_early do
    if Viz.any_key () then leave_early := true
  done;;


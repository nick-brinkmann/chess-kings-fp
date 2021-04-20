module G = Graphics ;;

let cX_DIMENSION = 200 ;;
let cY_DIMENSION = 200 ;;
let cPIXELS_PER_BLOCK = 3 ;;


(* let await_key () =
  if !cVISUALIZE then
     ;; *)

(* initialize () -- Establishes the graphics window and sets its
   properties.*)
let initialize () =
  (* open a graphics window to draw into and size it appropriately *)
  G.open_graph "";
  (* resize the window to a 1x1 aspect ratio, only printing
      the board. Might change to add moves history later *)
  G.resize_window (cX_DIMENSION * cPIXELS_PER_BLOCK)
                  (cY_DIMENSION * cPIXELS_PER_BLOCK);
  (* turn off auto-synchronizing; we'll handle double buffer
      synchronization ourselves *)
  G.auto_synchronize false;
  G.display_mode false ;;

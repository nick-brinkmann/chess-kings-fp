(* Declaration of global variable *)
module G = Graphics ;;

let cX_DIMENSION = 100 ;;
let cY_DIMENSION = 100 ;;
let cPIXELS_PER_BLOCK = 8 ;;

let cSQUARE_WIDTH = (cX_DIMENSION*cPIXELS_PER_BLOCK) / 8 ;;
let cSQUARE_HEIGHT = (cY_DIMENSION * cPIXELS_PER_BLOCK) / 8 ;;

let cLINEWIDTH = 3 ;;

let cWHITECOLOR = G.white ;;
let cBLACKCOLOR = G.black ;;
(* Declaration of global variable *)
module G = Graphics ;;

let cX_DIMENSION = 100 ;;
let cY_DIMENSION = cX_DIMENSION ;;
let cPIXELS_PER_BLOCK = 8 ;;

let cSQUARE_WIDTH = (cX_DIMENSION*cPIXELS_PER_BLOCK) / 8 ;;
let cSQUARE_HEIGHT = (cY_DIMENSION * cPIXELS_PER_BLOCK) / 8 ;;

let cLINEWIDTH = 3 ;;

let cWHITECOLOR = G.rgb 207 185 151 ;;
let cBLACKCOLOR = G.rgb 101 67 33 ;;

let cWHITE_PIECE_COLOR = G.white ;;
let cBLACK_PIECE_COLOR = G.black ;;
let cSELECTED_COLOR = G.red ;;
let cHOVER_COLOR = G.rgb 214 213 212;;
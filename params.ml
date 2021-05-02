(* Declaration of global variable *)
module G = Graphics ;;

let cX_DIMENSION = 100 ;;
let cY_DIMENSION = cX_DIMENSION ;;
let cPIXELS_PER_BLOCK = 8 ;;

let cSQUARE_WIDTH = (cX_DIMENSION*cPIXELS_PER_BLOCK) / 8 ;;
let cSQUARE_HEIGHT = (cY_DIMENSION * cPIXELS_PER_BLOCK) / 8 ;;

let cLINEWIDTH = 3 ;;

let cWHITECOLOR = G.rgb 245 229 213 ;;
let cBLACKCOLOR = G.rgb 237 169 104 ;;

let cWHITE_PIECE_COLOR = G.white ;;
let cBLACK_PIECE_COLOR = G.black ;;
let cSELECTED_COLOR = G.magenta ;;
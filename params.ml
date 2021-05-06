(* Declaration of global variable *)
module G = Graphics ;;

let cX_BOARDSIZE = 100 ;;
let cY_BOARDSIZE = cX_BOARDSIZE ;;

(* Do not change *)
let cX_DIMENSION = cX_BOARDSIZE + cX_BOARDSIZE / 4 ;;
let cY_DIMENSION = cY_BOARDSIZE + 3 ;;
let cPIXELS_PER_BLOCK = 8 ;;

let cSQUARE_WIDTH = (cX_BOARDSIZE * cPIXELS_PER_BLOCK) / 8 ;;
let cSQUARE_HEIGHT = (cY_BOARDSIZE * cPIXELS_PER_BLOCK) / 8 ;;

let cLINEWIDTH = 3 ;;

let cWHITECOLOR = G.rgb 207 185 151 ;;
let cBLACKCOLOR = G.rgb 101 67 33 ;;

let cWHITE_PIECE_COLOR = G.white ;;
let cBLACK_PIECE_COLOR = G.black ;;
let cSELECTED_COLOR = G.red ;;
(* let cHOVER_COLOR = G.rgb 214 213 212;; *)
let cUNDO_BACKGROUND = G.yellow ;;
let cUNDO_TEXT_COLOR = G.black ;;
let cPROMOTE_COLOR = G.rgb 212 211 210 ;;
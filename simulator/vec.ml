

let vec_minus (x1,y1) (x2,y2) =
  ((x1 -. x2),(y1 -. y2));;

let vec_add (x1,y1) (x2,y2) =
  ((x2 +. x1),(y2 +. y1));;


let vec_scalar_mult s (x,y) = 
  (s*.x,s*.y)

let vec_length (x,y) = 
  sqrt (x*.x+.y*.y)

let vec_square_length (x,y) = 
  (x*.x+.y*.y)
  
let vec_scale_to len vec = 
  vec_scalar_mult (len/.(vec_length vec)) vec 

let vec_neg (x,y) = 
  ((-.x),(-.y))

let vec_mult (x1,y1) (x2,y2) = 
  x1 *. x2 +. y1 *. y2

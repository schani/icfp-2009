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


let get_steigung (x1,y1) (x2,y2) =
  (y2 -. y1) /. (x2 -. x1);;

let get_lage  (x1,y1) (x2,y2) =
  y1 -. x1 *. (get_steigung (x1,y1) (x2,y2));;

let get_gerade  (x1,y1) (x2,y2) =
  ((get_steigung (x1,y1) (x2,y2)), (get_lage  (x1,y1) (x2,y2)));;


(*
  y = kx + d
  schnittpunkt zweier gerader
*)

let get_schnittpunkt (k1,d1) (k2,d2) =
  let x = (d2 -. d1) /. (k1-.k2) in
  let y = k1 *. x +. d1 
  in
  (x,y);;

let get_mitte  (x1,y1) (x2,y2) =
  ( ((x1 +. x2) /. 2.0),((y1 +. y2) /. 2.0) );;



(*
  tangentenschnittpunkt
*)

let get_T  p1 p3  q1 q3 =
  let gerade1 = get_gerade p1 p3 in
  let gerade2 = get_gerade q1 q3 in
  get_schnittpunkt gerade1 gerade2;;

let get_symmetrale  p1 p2 p3 q1 q2 q3 =
  let m = get_mitte p2 q2 
  in
  get_gerade m (get_T  p1 p3 q1 q3);;



(*
  ellipsenmittelpunkt
*)

let get_mittelpunkt  (p1,p2,p3) (q1,q2,q3) (r1,r2,r3) =
  get_schnittpunkt 
    (get_symmetrale p1 p2 p3 q1 q2 q3)
    (get_symmetrale  q1 q2 q3 r1 r2 r3)
;;

let get_focus (x,y) =
  ((0.0 -. x),(0.0 -. y));;


let pi = 4.0 *. atan 1.0;;

let get_punkt r winkel =
  let bogen =  pi *. winkel /. 180.0 in
  (r *. (cos bogen) , r *. (sin bogen) );;

(* we are geocentric here! *)
let calculate_perige points1 point2 points3 = 
  let p1,_,_ = points1 in
  let mittel = get_mittelpunkt points1 point2 points3 in
  let brenn2 = vec_add mittel mittel in
  let r1 = vec_length p1 in
  let r2 = vec_length (vec_minus brenn2 p1) in
  let ee = vec_length brenn2 in
  let rp = r1 +. r2 -. ee in
  let perige = vec_scale_to rp (vec_neg brenn2) in
  perige

let calculate_period points1 points2 points3 = 
  let perige = calculate_perige points1 points2 points3 in
  let mittel = get_mittelpunkt points1 points2 points3 in
  let a = vec_length (vec_add perige mittel) in
  let t = 2.*.pi*.sqrt(a*.a*.a/.Hohmann.mu) in
  t

  

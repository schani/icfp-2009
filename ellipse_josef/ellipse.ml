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

let get_mittelpunkt  p1 p2 p3 q1 q2 q3 r1 r2 r3 =
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


let schub = 50.0;;
let grenze = 100.0;;

let sign t =
  if (t<0.0) then
    -.1.0
  else
    1.0;;

let distanz siex siey =
  sqrt(siex *. siex  +. siey *. siey);;

let faktor_x siex siey =
  let sum = siex +. siey in
    siex /. sum;;

let faktor_y siex siey =
  let sum = siex +. siey in
    siey /. sum;;

let time t =
  t /. schub;;


let pi = 4.0 *. atan 1.0;;

let radius sx sy = 
  sqrt ((sx *. sx) +. (sy *. sy));;

let winkel sx sy =
  atan2 sy sx;;

let polar sx sy =
  ((radius sx sy), (winkel sx sy));;

let polar2 sx1 sy1 sx2 sy2 = 
  let sx = sx2 -. sx1 in
  let sy = sy2 -. sy1 in
  polar sx sy;;

let vektor2 sx1 sy1 sx2 sy2 = 
  let sx = sx2 -. sx1 in
  let sy = sy2 -. sy1 in
  (sx, sy);;

let vektor r alpha =
  ((r *. (cos alpha)) , (r *. (sin alpha)) );;

let vektordiff (x1,y1) (x2,y2) =
  ((x2 -. x1),(y2 -. y1));;
 

let distance  sx1 sy1 sx2 sy2 =
  sqrt ((sx1 -. sx2) *. (sx1 -. sx2) +. (sy1 -. sy2) *. (sy1 -. sy2));;

let to_our x = (0.0 -. x);;

let approximate_position ichx ichy sie_x sie_y =
  let siex = ((to_our ichx) +. sie_x) in
  let siey = ((to_our ichy) +. sie_y) in
  let d = distanz  siex siey in
  if (d < grenze) then
    (0.0,0.0,0.0)
  else 
    let t = time d in
    ((schub *. (sign siex) *. (faktor_x siex siey)) , 
     (schub *. (sign siey) *. (faktor_y siex siey)), 
     t);; 

let approximate_orbit ichx1 ichy1 ichx2 ichy2 siex1 siey1 siex2 siey2 =
  let sie_orbit = vektor2 ((to_our ichx1) +. siex1)  ((to_our ichx2) +. siex2)  ((to_our ichy1) +. siey1)  ((to_our ichy2) +. siey2) in
  let ich_orbit = vektor2 (to_our ichx1) (to_our ichy1) (to_our ichx2) (to_our ichy2) in
  let orbit_diff = vektordiff sie_orbit ich_orbit in
  orbit_diff;;

approximate_position 0.0 0.0 7000.0 100.0;;
approximate_position  0.0 0.0 7000.0 (-100.0);;
approximate_position  0.0 0.0 (-7000.0) 100.0;;
approximate_position  0.0 0.0 (-7000.0) (-100.0);;

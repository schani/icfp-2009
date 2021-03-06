
let pi = 4.0 *. atan 1.0;;

(*
  kg 
*)
let mass_of_earth = 6.0e24;;

(*
  m
*)
let radius_of_earth = 6.357e6;;

(*
  universal gravitational constant 
  m^3 / (kg * s^2)
*)
let ge = 6.67428e-11;;

(*
  standard gravitational paramater of the primary body, i.e., the
  EARTH
  m^3 / s^2
*)

let mu = mass_of_earth *. ge;;


let radius sx sy = 
  sqrt ((sx *. sx) +. (sy *. sy));;

let get_th r1 r2 = 
  pi *. (sqrt ( ( (r1 +. r2) *. (r1 +. r2) *. (r1 +. r2)) /. (8.0 *. mu)));;


(*
  beta is der other
  alpha is der meine
*)

let w w = 
  if w > 2. *. pi then 
    w -. 2. *. pi 
  else if w < 0. then
    w +. 2. *. pi
  else
    w

let get_winkel x y =
  w (atan2 y x)

let get_speed a1 a2 =
  let v = a2 -. a1 in
  if v > pi then
    v -. 2.*.pi
  else
    v

let absolute_time_to_start ichx1 ichy1 ichx2 ichy2 siex1 siey1 siex2 siey2
    =
  let beta_t = get_winkel siex2 siey2 in
  let alpha_t = get_winkel ichx2 ichy2 in
  let v_a = 
    get_speed (get_winkel ichx1 ichy1) (get_winkel ichx2 ichy2) in
  let v_b = 
    get_speed (get_winkel siex1 siey1) (get_winkel siex2 siey2) in
  let t_h = get_th (radius  ichx1 ichy1) (radius siex1 siey1) in
  (* let res = (beta_t -. alpha_t -. pi +. (v_b *. t_h)) /. (v_a -. v_b) *)
  let beta_th = w(beta_t +. (v_b *. t_h)) in
  let alpha_th = w(alpha_t +. pi) in
  let delta_th = (beta_th -. alpha_th) in
  let res = delta_th /. (v_a -. v_b) in
  let res = 
    if v_a > v_b then
      if res >= 0. then 
	res 
      else 
	(delta_th-.2.*.pi) /. (v_a -. v_b)
    else
      if res < 0. then 
	res 
      else 
	(delta_th-.2.*.pi) /. (v_a -. v_b)
  in
  (* Printf.printf "abs_time_to_start: %f %f at %f bt %f va %f vb %f ath %f bth %f dth %f\n" 
     res t_h alpha_t beta_t v_a v_b alpha_th beta_th delta_th; *)
  abs_float res


let inve f = 0.0 -. f;;

let abso ich sie =
  (inve ich) +. sie;;

let new_time_to_start  ichx1 ichy1 ichx2 ichy2 siex1 siey1 siex2 siey2
    =
  absolute_time_to_start 
    (inve ichx1)
    (inve ichy1)
    (inve ichx2)
    (inve ichy2)
    (abso ichx1 siex1)   
    (abso ichy1 siey1)   
    (abso ichx2 siex2)   
    (abso ichy2 siey2);;


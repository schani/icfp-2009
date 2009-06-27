(*
  Vetchinkin transfer
  CCCP space program rules im hirn
*)


let pi = 4.0 *. atan 1.0;;

let radius sx sy = 
  sqrt ((sx *. sx) +. (sy *. sy));;

let winkel sx sy =
  atan2 sy sx;;

let polar sx sy =
  ((radius sx sy), (winkel sx sy));;

let vektor r alpha =
  ((r *. (cos alpha)) , (r *. (sin alpha)) );;

let distance  sx1 sy1 sx2 sy2 =
  sqrt ((sx1 -. sx2) *. (sx1 -. sx2) +. (sy1 -. sy2) *. (sy1 -. sy2));;

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

(*
  formeln laut http://en.wikipedia.org/wiki/Hohmann_transfer_orbit
  fuer den benötigten geschwindigkeitsunterschied.  
  m/s
*)
let delta_v r1 r2 =
  (sqrt (mu /. r1)) *.  ( (sqrt ((2.0 *. r2) /. (r1 +. r2))) -. 1.0);; 

let delta_v_prime r1 r2 =
  (sqrt (mu /. r2)) *.  ( 1.0 -. (sqrt ((2.0 *. r1) /. (r1 +. r2))) );; 


(*
  time to change to other radius
  s
*)

let th r1 r2 = 
  pi *. (sqrt ( ( (r1 +. r2) *. (r1 +. r2) *. (r1 +. r2)) /. (8.0 *. mu)));;
  




(*
  clockwise orange
  gguhrzeiger = 1 wenn der hosche gegen die uhr kurdelt und -1 wenn er das
  ned tut
*)

let erster_schub sx sy zielradius gguhrzeiger =
  let startradius = radius sx sy in
  let alpha = ((winkel sx sy) +. (gguhrzeiger *. (pi /. 2.0)))
  in
  vektor (delta_v startradius zielradius) alpha;;

let zweiter_schub sx sy zielradius gguhrzeiger =
  let startradius = radius sx sy in
  let alpha = ((winkel sx sy) +. ((0.0 -. gguhrzeiger) *. (pi /. 2.0)))
  in
  vektor (delta_v_prime startradius zielradius) alpha;;

(*
  in absoluten koordinaten
*)
let zweiterpunkt  sx sy zielradius =
    vektor zielradius ((winkel sx sy) +. pi);;

let zeitbedarf sx sy zielradius =
  let startradius = radius sx sy in
  th startradius zielradius;;

 
let pos_winkel a = 
  if (a < 0.0) then
    (a +. 2.0 *. pi)
  else
    a;;

 
let richtung sx1 sy1 sx2 sy2 =
  let gguhr = function
      true -> 1.0
    | false -> -1.0 in
  let a = pos_winkel (winkel sx1 sy1) in
  let b = pos_winkel (winkel sx2 sy2) in
  gguhr (a < b);; 


let to_our x = (0.0 -. x);;

(*
  (schub 1, schub 2, timetosayhello)

*)

let hohmann sx1 sy1 sx2 sy2 zielradius =
  let rich = richtung  (to_our sx1) (to_our sy1) (to_our sx2) (to_our
    sy2) in
  (
    (erster_schub  (to_our sx2) (to_our sy2) zielradius rich),
    (zweiter_schub  (to_our sx2) (to_our sy2) zielradius rich),
    (zeitbedarf (to_our sx2) (to_our sy2) zielradius),
(zweiterpunkt (to_our sx2) (to_our sy2) zielradius)
);;


(* hohmann (-6556995.342903) 7814.930000 (-6556981.371618) 15629.848899 *)
(*   8000000q.000000;; *)

(* hohmann (-6556995.342903) 7814.930000 (-6556981.371618) 15629.848899 *)
(*   80000000.000000;; *)


let to_absolute we other = other -. we;;


(*
  pi / s
*)

let speed sx1 sy1 sx2 sy2 =
  let a = pos_winkel (winkel sx1 sy1) in
  let b = pos_winkel (winkel sx2 sy2) in
  b -. a;;


(* let winkel_if_shot wex1 wey1 wex2 wey2 oex1 oey1 oex2 oey2 = *)
(*   let zielr = radius (to_absolute wex1 oex1) (to_absolute wey1 oey1) in *)
(*   let zielspeed = speed  *)
(*     (to_absolute wex1 oex1)  *)
(*     (to_absolute wey1 oey1) *)
(*     (to_absolute wex2 oex2)  *)
(*     (to_absolute wey2 oey2) in *)
(*   let arrival_date = zeitbedarf  wex2 wey2 zielr in *)
(*   let arrival_point = zweiterpunkt wex2 wey2 zielr in *)
(*   let alpha = winkel  (to_absolute  wex2 oex2 ) (to_absolute wey2 oey2) in *)
(*   let his_pos = vektor zielr (alpha +. (zielspeed *. arrival_date)) in *)
(*   match (arrival_point, his_pos) with *)
(*       ((x,y), (hx,hy)) -> distance x y hx hy;; *)


let winkel_if_shot wex1 wey1 wex2 wey2 oex1 oey1 oex2 oey2 =
  let zielr = radius (to_absolute wex1 oex1) (to_absolute wey1 oey1) in
  let zielspeed = speed 
    (to_absolute wex1 oex1) 
    (to_absolute wey1 oey1)
    (to_absolute wex2 oex2) 
    (to_absolute wey2 oey2) in
  let arrival_date = zeitbedarf  (to_our wex2) (to_our wey2) zielr in
  let arrival_point = zweiterpunkt (to_our wex2) (to_our wey2) zielr in
  let his_alpha = winkel  (to_absolute  wex2 oex2 ) (to_absolute wey2 oey2) in
  let our_alpha = match arrival_point with
      (x,y) -> winkel x y in 
  (his_alpha +. (arrival_date *.zielspeed)) -. our_alpha;;


let time_to_start wex1 wey1 wex2 wey2 oex1 oey1 oex2 oey2 =
  let winkel_to_goal = 
    (winkel_if_shot wex1 wey1 wex2 wey2 oex1 oey1 oex2 oey2) in
  let winkel_to_wait =
      winkel_to_goal in
  let zielspeed = speed 
    (to_absolute wex1 oex1) 
    (to_absolute wey1 oey1)
    (to_absolute wex2 oex2) 
    (to_absolute wey2 oey2) in
  let ourspeed = speed
    (to_our wex1) 
    (to_our wey1) 
    (to_our wex2) 
    (to_our wey2) in
  winkel_to_wait /. (ourspeed -. zielspeed);;


 

(* time_to_start  *)
(*   (-6556995.342903) (7814.932739)  *)
(*   (-6556981.371618) (15629.854376) *)
(*   (1800001.790116) (892.59737999) *)
(*   (1800007.160459) (1785.18840850);; *)

(*  winkel_if_shot *)
(*   (-6556995.342903) (7814.932739)  *)
(*   (-6556981.371618) (15629.854376) *)
(*   (1800001.790116) (892.59737999) *)
(*   (1800007.160459) (1785.18840850);; *)

 

(* to_absolute (-6556995.342903) (1800001.790116)  ;; *)
(*  to_absolute (7814.932739) (892.59737999)  ;; *)

(*  to_absolute (-6556981.371618) (1800007.160459)  ;; *)
(*  to_absolute (15629.854376) (1785.18840850)  ;; *)


(*   hohmann  (-6556995.342903) (7814.932739)  (-6556981.371618) *)
(*   (15629.854376) *)
(*     (radius  *)
(*       (to_absolute (-6556995.342903) (1800001.790116) )  *)
(*       (to_absolute  (7814.932739) (892.59737999)) *)
(*     );; *)

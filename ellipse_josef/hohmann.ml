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


hohmann (-6556995.342903) 7814.930000 (-6556981.371618) 15629.848899
  42164000.000000;;



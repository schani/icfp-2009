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

let length (sx, sy) =
  sqrt( sx *. sx +. sy *. sy );;



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
  (schub 1, schub 2, time schub 2, expected meeting point, totalschub)

*)

let hohmann sx1 sy1 sx2 sy2 zielradius =
  let rich = richtung  (to_our sx1) (to_our sy1) (to_our sx2) (to_our
    sy2) in
  (
    (erster_schub  (to_our sx2) (to_our sy2) zielradius rich),
    (zweiter_schub  (to_our sx2) (to_our sy2) zielradius rich),
    (zeitbedarf (to_our sx2) (to_our sy2) zielradius),
    (zweiterpunkt (to_our sx2) (to_our sy2) zielradius),
    (
      length (erster_schub  (to_our sx2) (to_our sy2) zielradius rich) +.
      length (zweiter_schub  (to_our sx2) (to_our sy2) zielradius rich)
    ) 
);;


hohmann (-6556995.342903) 7814.930000 (-6556981.371618) 15629.848899
  42164000.000000;;

(*
  BI-Elliptic Transfer 
  http://en.wikipedia.org/wiki/Bi-elliptic_transfer
*)

let bi_delta_v1 r0 a1 =
  sqrt( 2.0 *. mu /. r0 -. mu /. a1 ) -. sqrt( mu /. r0 );;

let bi_delta_v2 rb a1 a2 =
   sqrt( 2.0 *. mu /. rb -. mu /. a2 ) -. sqrt( 2.0 *. mu /. rb -. mu /. a1 );; 

let bi_delta_v3 rf a2 = 
   sqrt( mu /. rf ) -. sqrt( 2.0 *. mu /. rf -. mu /. a2 );;

let bi_semimajor_a1 r0 rb =
  r0 +. rb /. 2.0;;

let bi_semimajor_a2 rf rb =
  rf +. rb /. 2.0;;

let bi_time_t1 a1 =
  pi *. sqrt( a1 *. a1 *. a1 /. mu );;

let bi_time_t2 a2 =
  pi *. sqrt( a2 *. a2 *. a2 /. mu );;

let bi_erster_schub sx sy intermediateradius gguhrzeiger =
  let startradius = radius sx sy in
  let alpha = ((winkel sx sy) +. (gguhrzeiger *. (pi /. 2.0))) in
  let a1 = bi_semimajor_a1 startradius intermediateradius
  in
  vektor (bi_delta_v1 startradius a1) alpha;;

let bi_zweiter_schub sx sy intermediateradius zielradius gguhrzeiger =
  let alpha = ((winkel sx sy) +. (gguhrzeiger *. (pi /. 2.0))) in
  let startradius = radius sx sy in
  let a1 = bi_semimajor_a1 startradius intermediateradius in
  let a2 = bi_semimajor_a2 zielradius intermediateradius in
  vektor (bi_delta_v2 intermediateradius a1 a2) alpha;;

let bi_dritter_schub sx sy intermediateradius zielradius gguhrzeiger =
  let alpha = ((winkel sx sy) +. (gguhrzeiger *. (pi /. 2.0))) in
  let a2 = bi_semimajor_a2 zielradius intermediateradius in
  vektor (bi_delta_v3 zielradius a2) alpha;;

let bi_zeitpunkt_mittelschub sx sy intermediateradius =
  let startradius = radius sx sy in
  let a1 = bi_semimajor_a1 startradius intermediateradius in
  bi_time_t1 a1;;  

let bi_zeitpunkt_endschub intermediateradius zielradius =
  let a2 = bi_semimajor_a2 zielradius intermediateradius in
  bi_time_t2 a2;;

let bi_zweiterpunkt sx sy zielradius =
  let alpha = winkel sx sy in
  vektor zielradius alpha;;

(*
  (schub 1, schub 2, schub 3, time schub 2, time schub 3, expected meeting point, totalschub)

*)

let bielliptic sx1 sy1 sx2 sy2 zielradius =
  let rich = richtung  (to_our sx1) (to_our sy1) (to_our sx2) (to_our
    sy2) in
  let inter = (radius (to_our sx2) (to_our sy2)) *. 0.1 in
  (
    
    (bi_erster_schub  (to_our sx2) (to_our sy2) inter rich),
    (bi_zweiter_schub  (to_our sx2) (to_our sy2) inter zielradius rich),
    (bi_dritter_schub  (to_our sx2) (to_our sy2) inter zielradius rich),
    (bi_zeitpunkt_mittelschub (to_our sx2) (to_our sy2) inter),
    (bi_zeitpunkt_endschub inter zielradius),
    (bi_zweiterpunkt (to_our sx2) (to_our sy2) zielradius),
    ( (length (bi_erster_schub  (to_our sx2) (to_our sy2) inter rich) ) +.
      (length (bi_zweiter_schub  (to_our sx2) (to_our sy2) inter zielradius rich)) +.
      (length (bi_dritter_schub  (to_our sx2) (to_our sy2) inter zielradius rich))
    )
);;

bielliptic (-6556995.342903) 7814.930000 (-6556981.371618) 15629.848899
  42164000.000000;;

hohmann (-6556995.342903) 7814.930000 (-6556981.371618) 15629.848899
  42164000.000000;;

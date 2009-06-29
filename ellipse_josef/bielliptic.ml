(*
  Vetchinkin transfer
  CCCP space program rules im hirn
*)
open Hohmann

let length (sx, sy) =
  sqrt( sx *. sx +. sy *. sy );;

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
  ( r0 +. rb ) /. 2.0;;

let bi_semimajor_a2 rf rb =
  ( rf +. rb ) /. 2.0;;

let bi_time_t a =
  pi *. sqrt( a *. a *. a /. mu );;


let bi_erster_schub sx sy intermediateradius gguhrzeiger =
  let startradius = radius sx sy in
  let alpha = ((winkel sx sy) +. (gguhrzeiger *. (pi /. 2.0))) in
  let a1 = bi_semimajor_a1 startradius intermediateradius
  in
  vektor (bi_delta_v1 startradius a1) alpha;;

let bi_zweiter_schub sx sy intermediateradius zielradius gguhrzeiger =
  let alpha = ((winkel sx sy) +. ((-. gguhrzeiger) *. (pi /. 2.0))) in
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
  bi_time_t a1;;  

let bi_zeitpunkt_endschub intermediateradius zielradius =
  let a2 = bi_semimajor_a2 zielradius intermediateradius in
  bi_time_t a2;;

let bi_zweiterpunkt sx sy zielradius =
  let alpha = winkel sx sy in
  vektor zielradius alpha;;

let calculate_costs start inter ziel = 
  let a1 = bi_semimajor_a1 start inter in
  let a2 = bi_semimajor_a2 ziel inter in
  (abs_float (bi_delta_v1 start a1)) +. 
    (abs_float (bi_delta_v2 inter a1 a2)) +.
    (abs_float (bi_delta_v3 ziel a2))
    
(* let inter = ((length (sx1,sy1)) +. zielradius) /. 2.0 in *)



(*
  (schub 1, schub 2, schub 3, time schub 2, time schub 3, expected meeting point, totalschub)

*)
let bielliptic sx1 sy1 sx2 sy2 zielradius inter =
  let rich = richtung  (to_our sx1) (to_our sy1) (to_our sx2) (to_our
    sy2) in
  (* let inter = 268000000.0 in *)
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

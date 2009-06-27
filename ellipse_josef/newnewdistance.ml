let ensure_les_than_pi a =
  if a > pi then
    a -. (2.0 *. pi)
  else
    a;;

let ensure_greater_than_minus_pi a =
  if a < (inve pi) then
    a +. (2.0 *. pi)
  else
    a;;


(*
  distance angle alpha to target hosche
  -2pi < alpha < 2pi
  if alpha < 0 dann gehts im uhrzeigersinn
*)

let differenz ankunft sie uhrzeiger =
  let normalisierte_winkel =
    ensure_greater_than_minus_pi (ensure_les_than_pi (pi -. sie +. ankunft))
  in
  if uhrzeiger then
    (inve pi) -. normalisierte_winkel  
  else
    pi -. normalisierte_winkel    
;;


(* let differenz ankunft sie uhrzeiger = *)
(*   let normalisierte_winkel = *)
(*     if sie > 0.0 then *)
(*       (ensure_les_than_pi (pi -. sie +. ankunft)) *)
(*     else *)
(*       (ensure_greater_than_minus_pi (ankunft -. (pi +. sie))) *)
(*   in *)
(*   if uhrzeiger then *)
(*     (inve pi) -. normalisierte_winkel   *)
(*   else *)
(*     pi -. normalisierte_winkel     *)
(* ;; *)

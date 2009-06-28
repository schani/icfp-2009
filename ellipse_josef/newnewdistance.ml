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

let ensure a =
  ensure_greater_than_minus_pi (ensure_les_than_pi a);;

let get_winkel x y =
  atan2 y x;;

(*
  distance angle alpha to target hosche
  -2pi < alpha < 2pi
  if alpha < 0 dann gehts im uhrzeigersinn
*)

let inve f = 0.0 -. f;;

let abso ich sie =
  (inve ich) +. sie;;


let differenz ankunft sie uhrzeiger =
  let normalisierte_winkel =
    ensure_greater_than_minus_pi (ensure_les_than_pi (pi -. sie +. ankunft))
  in
  if uhrzeiger then
    (inve pi) -. normalisierte_winkel  
  else
    pi -. normalisierte_winkel    
;;


let getuhr x1 y1 x2 y2   =
  let alpha =  get_winkel x1 y1 in
  let beta = get_winkel x2 y2 in
  (ensure (pi -. alpha +. beta)) > 0.0;; 


let getspeed  x1 y1 x2 y2  =
  let alpha =  get_winkel x1 y1 in
  let beta = get_winkel x2 y2 in
  differenz alpha beta (getuhr x1 y1 x2 y2);;

let get_th r1 r2 = 
  pi *. (sqrt ( ( (r1 +. r2) *. (r1 +. r2) *. (r1 +. r2)) /. (8.0 *. mu)));;






let absolute_time_to_start ichx1 ichy1 ichx2 ichy2 siex1 siey1 siex2 siey2 =
  let zielr = radius (abso ichx1 siex1) (abso ichy1 siey1) in
  let startr = radius ichx1 ichy1 in
  let ankunft = ensure (get_winkel ichx2 ichy2 +. pi) in
  let th = get_th startr zielr in
  let herspeed = getspeed siex1 siey1 siex2 siey2 in
  let meispeed = getspeed  ichx1 ichy1 ichx2 ichy2 in
  let uhr = getuhr  siex1 siey1 siex2 siey2 in 
  let sie = 
    (get_winkel siex2 siey2) +. th *. herspeed
  in
  let weg = differenz ankunft sie uhr 
  in
  abs (int_of_float (weg /. (meispeed -. herspeed)))
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

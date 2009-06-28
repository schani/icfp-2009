open Hohmann

(* 
   let vec_minus (x1,y1) (x2,y2) = 
   (x2-x1,y2-y1)
   
   let length (x,y) =
*)

let calc_correction wex1 wey1 wex2 wey2 oex1 oey1 oex2 oey2 =
  let dist = distance 0. 0. oex2 oey2 in 
  if dist > 1000. then
    let winkel_forward = winkel (wex2 -. wex1) (wey2 -. wey1) in
    let winkel_to_sat = winkel oex2 oey2 in
    let correction = 
      if (winkel_forward *. winkel_to_sat) < 0. then 
	(oex2/.20000.),(oey2/.20000.)
      else
      (-.(oex2/.20000.)),(-.(oey2/.20000.))
    in
    correction
  else
    (0.,0.)
  
    

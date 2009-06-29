let max_speed = 500.0;;
let homing_maxdist = 1000.;;
let homing_innerthreshold = 100.;;

let vec_minus (x1,y1) (x2,y2) =
  ((x1 -. x2),(y1 -. y2));;

let vec_add (x1,y1) (x2,y2) =
  ((x2 +. x1),(y2 +. y1));;


let vec_scalar_mult s (x,y) = 
  (s*.x,s*.y)

let vec_length (x,y) = 
  sqrt (x*.x+.y*.y)

let vec_square_length (x,y) = 
  (x*.x+.y*.y)
  
let vec_scale_to len vec = 
  vec_scalar_mult (len/.(vec_length vec)) vec 

let vec_neg (x,y) = 
  ((-.x),(-.y))

let vecapproximation speed_diff =
  vec_neg speed_diff;;

let speed_factor distance speed =
  if distance > homing_maxdist then
    speed /. distance
  else if distance > ( ( homing_maxdist -. homing_innerthreshold ) /. 2. +. homing_innerthreshold) then
    1.0 
  else
    2.0;;

let posapproximation he_now speed_diff distance =
  let move_vector = vec_add speed_diff  he_now in
  let vec_length_move = vec_length move_vector in
  let move_vector = vec_scalar_mult vec_length_move move_vector in
    move_vector;;

let follow m weprev heprev posapprox vecapprox =
  let abs_weprev = (vec_neg weprev) in
  let abs_heprev =  (vec_minus heprev weprev) in
  let wenow = Vm.vm_read_ourpos m in
  let henow = Vm.vm_read_sat_pos m 0 in
  let abs_we = (vec_neg wenow) in
  let abs_he = (vec_minus henow wenow) in
  let speed_we = (vec_minus abs_we (vec_neg abs_weprev)) in
  let speed_he = (vec_minus abs_he (vec_minus abs_heprev abs_weprev)) in
  let distance = vec_length(henow) in
    (*  let pos_diff = (vec_diff  abs_we abs_he) in *)
  let speed_diff = vec_minus speed_he speed_we in
    if posapprox then
      posapproximation  henow speed_diff distance
    else if vecapprox then
      vecapproximation speed_diff 
    else 
      (0.0, 0.0);;
   

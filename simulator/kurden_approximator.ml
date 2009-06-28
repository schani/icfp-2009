

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

let gt to_earth = 
  let rsq = (vec_square_length to_earth) in
  let force = Hohmann.mu /. rsq in
  vec_scale_to force to_earth
    
    
(* abs coords *)
let calc_next_pos pos prev = 
  let gtvec = gt (vec_neg pos) in
  vec_add (vec_add pos (vec_minus pos prev)) (vec_scalar_mult 0.5 gtvec)

let magic_schani_constants = 30,75.,500.

let do_steps m time = 
  Speculate.lookahead m time Emp_dumper.emp_dump_writer

let zerothrust_steps m time = 
  Speculate.lookahead m time (fun m -> 
    ignore(Emp_dumper.emp_dump_writer m);
    (Vm.vm_write_thrust m (0.,0.)))

let min x y = 
  if x < y then x else y

(* follows for "one" step *)
    let follower_step m weprev heprev = 
  match Speculate.do_we_win m with 
    | Some m -> 
	m 
    | None -> 
	let wenow = Vm.vm_read_ourpos m in
	let henow = Vm.vm_read_sat_pos m 0 in
	let abs_we = (vec_neg wenow) in
	let abs_he = (vec_minus henow wenow) in
	let speed_we = (vec_minus abs_we (vec_neg weprev)) in
	let speed_he = (vec_minus abs_he (vec_minus heprev weprev)) in
	(*  let pos_diff = (vec_diff  abs_we abs_he) in *)
	let speed_diff = vec_minus speed_he speed_we in
	let skip_size,fueldivisor,mindist  = magic_schani_constants in
	let thrust = vec_add speed_diff (vec_scale_to (min (vec_length henow)
	  ((Vm.vm_read_fuel m) /. fueldivisor)) henow) in
	
	if (vec_length henow) > mindist then 
	  let m = do_steps (Vm.vm_write_thrust m thrust) 1 in
	  let m = zerothrust_steps m skip_size in
	  m
	else
	  zerothrust_steps m 1
	    
let follow m = 
let rec loop m = 
    let pos = Vm.vm_read_ourpos m in
    let his_pos = Vm.vm_read_sat_pos m 0 in
    if Vm.vm_is_done m then
      m 
    else
      loop (follower_step (zerothrust_steps m 1) pos his_pos)
  in
  loop m 

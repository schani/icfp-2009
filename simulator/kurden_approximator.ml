open Vec 

let gt to_earth = 
  let rsq = (vec_square_length to_earth) in
  let force = Hohmann.mu /. rsq in
  vec_scale_to force to_earth
    
    
(* abs coords *)
let calc_next_pos pos prev = 
  let gtvec = gt (vec_neg pos) in
  vec_add (vec_add pos (vec_minus pos prev)) (vec_scalar_mult 0.5 gtvec)


let do_steps_f m time func = 
  let emp_dumper = Emp_dumper.get_emp_dump_writer stdout in
  let rec loop i m = 
    if i = 0 then
      m
    else
      let m = (Vm.vm_execute_one_step_and_record (emp_dumper (func m))) in
      if Vm.vm_is_done m then 
	emp_dumper m
      else
	loop (i-1) m
  in
  loop time m

let step_machine m = 
  do_steps_f m 1 (fun m -> m)

let zerothrust_steps m time = 
  Printf.printf "zeroing %d\n" time;
  do_steps_f m time (fun m -> 
    Printf.printf "zero\n";
    (Vm.vm_write_thrust m (0.,0.)))

let min x y = 
  if x < y then x else y

let vec_normal (x,y) = 
  ((-.y),x)

let vec_align_to_direction v1 v2 = 
  if (vec_length (vec_add v1 v2)) > (vec_length (vec_add v1 (vec_neg v2))) then 
    v2
  else
    (vec_neg v2)


let magic_constants = 20,95.,700.,500.

let calculate_orbit_velocity_vector m speed_we = 
  let r = vec_length (Vm.vm_read_ourpos m) in
  let circ_speed = sqrt (Hohmann.mu /. r) in 
  let normalvec = vec_scale_to circ_speed (Vm.vm_read_ourpos m) in
  let correct_direction_vector_for_orbital_velocity = vec_align_to_direction speed_we (vec_normal normalvec) in
  let sx,sy = speed_we in
  let px,py = Vm.vm_read_ourpos m in
  let cx,cy = correct_direction_vector_for_orbital_velocity in
  let nx,ny = normalvec in
  Printf.fprintf stderr "THRUST1 r=%08.8f circ=%08.8f speed=%08.8f, %08.8f pos=%08.8f, %08.8f\n" 
                         r circ_speed                   sx sy             px py;
  Printf.fprintf stderr "THRUST2 corrvec=%08.8f, %08.8f normalvec=%08.8f, %08.8f  \n"
                                           cx cy                 nx ny;
  correct_direction_vector_for_orbital_velocity  
    

(* follows for "one" step *)
let follower_step m weprev heprev = 
  match Speculate.do_we_win m with 
    | Some m -> 
	let m = zerothrust_steps m 900 in 
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
	let skip_size,fueldivisor,mindist,stay_here_relatively  = magic_constants in
	let thrust = vec_add speed_diff (vec_scale_to 
	  ((min ((vec_length henow)) (Vm.vm_read_fuel m)) /. fueldivisor) henow) in
	let tx,ty = thrust in
	
	if (vec_length henow) < stay_here_relatively then 
	  (* let correction_thrust = vec_minus (calculate_orbit_velocity_vector m speed_we) speed_we in
	     let thx,thy = correction_thrust in
	     Printf.fprintf stderr "need_ thrust: %d %08.8f %08.8f\n" m.Vm.timestep thx thy;
	  *)
	  let correction_thrust = vec_minus speed_he speed_we in
	  let m = step_machine (Vm.vm_write_thrust m correction_thrust) in
	  Printf.printf "ZEROTHURSTING now for1000000 %d \n" m.Vm.timestep;
	  let m = zerothrust_steps m 1000000 in 
	  m
	else if (vec_length henow) > mindist then 
	  let _ = 
	    Printf.printf "will thrust\n";
	    Printf.printf "thrust: %f %f %f %f %f %b\n" 
	      tx ty (vec_length thrust) (vec_length henow) (Vm.vm_read_fuel m) ((vec_length henow) > (Vm.vm_read_fuel m)) ; 
	    flush stdout;
	  in
	  let m = step_machine (Vm.vm_write_thrust m thrust) in
	  let m = zerothrust_steps m skip_size in
	  m
	else
	  zerothrust_steps m 1
	    
let follow m = 
(* let writer = m.writer in*)
  let rec loop m = 
    (*  let m = writer m in *)
    let pos = Vm.vm_read_ourpos m in
    let his_pos = Vm.vm_read_sat_pos m 0 in
      if Vm.vm_is_done m then
	m 
      else
	loop (follower_step (zerothrust_steps m 1) pos his_pos)
  in
    loop m 

open Vm
open Vec



let do_nothing m =    
  vm_write_thrust m (0.,0.)
    

let copy_m m = 
  let m' = {m with 
    datamem = (Array.copy m.datamem); 
    output_ports = (Array.copy m.output_ports);
    input_ports = (Array.copy m.input_ports);
  } in
  m'
  
let lookahead m time func = 
  let rec loop i m = 
    if i = 0 then
      m
    else
      let m =  (vm_execute_one_step (func m)) in
      if vm_is_done m then 
	m 
      else
	loop (i-1) m
  in
  loop time (copy_m m)
    
let do_we_win m = 
  let m = lookahead m 900 do_nothing in
  if vm_is_done m then
    Some(m)
  else
    None

let skip_time m time = 
  lookahead m time do_nothing 

    
let measure_min_distance_perige m sat = 
  let sat_pos m = (vec_minus (vm_read_sat_pos m sat) (vm_read_ourpos m)) in
  let best_pos = ref (sat_pos m) in
  let best_time = ref (m.timestep) in
  let best_dist = ref (vec_length !best_pos) in
  let improved = ref false in
   
  let rec loop m = 
    let m = (vm_execute_one_step m) in
    let pos = sat_pos m in 
    let dist = vec_length pos in 
    if dist < !best_dist then (
      best_dist := dist;
      best_pos := pos;
      best_time := m.timestep;
      improved := true;
      loop m
    ) else if !improved then (
      (*Printf.printf "PERIGE: time: %d %f%f\n" m.timestep !best_time !best_dist;flush stdout; *)
      (!best_time,!best_pos,!best_dist))
    else
      loop m
  in
  loop (copy_m m)

let get_closest_info m to_pos = 
  let our_pos m = (vec_neg (vm_read_ourpos m)) in
  let best_pos = ref (vec_minus (our_pos m) to_pos) in
  let best_time = ref (m.timestep) in
  let best_dist = ref (vec_length !best_pos) in
  let improved = ref false in
  
  let rec loop m = 
    Printf.printf "x %d\n" m.timestep;
    let m = (vm_execute_one_step m) in
    let pos = (our_pos m) in 
    let dist = vec_length (vec_minus pos to_pos) in 
    if dist < !best_dist then (
      best_dist := dist;
      best_pos := pos;
      best_time := m.timestep;
      improved := true;
      loop m
    ) else if !improved then
      (!best_time,!best_pos,!best_dist)
    else
      loop m
  in
  loop (copy_m m)

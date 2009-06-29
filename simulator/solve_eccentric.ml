open Hohmann
open Vec
open Vm

type strat = {
  sat_at_perige: int;
  perige: float * float;
  sat_circ_orbit: float;
}
    


let calc_strat m sat lastpos = 
  (* let my_orbit = (vec_length (vm_read_ourpos m)) in *)
  let sat_at_perige,perige,sat_circ_orbit = Speculate.measure_min_distance_perige m sat in
  {
    sat_at_perige = sat_at_perige;
    perige = perige;
    sat_circ_orbit = sat_circ_orbit
  }


let calc_inter start ziel time = 
  let time = float_of_int time  in
  let rec loop lower upper = 
    let inter = ((lower+.upper)/.2.) in
    let time' = Bielliptic.calculate_time start inter ziel in
    
    if time' -. time < 1. then
      inter
    else if time' > time then
      loop lower inter
    else
      loop inter upper
  in
  if ziel > start then 
    loop start (100.*.start)
  else
    loop ziel (100.*.ziel)
  
let zerothrust = (0.,0.)


let rec ecc_jaeger m sat = 
 if vm_is_done m then
   (ignore(m.quitter m); m)
 else
   let emp_dumper = Emp_dumper.get_emp_dump_writer stdout in
   let step m = 
     if vec_length (vm_read_sat_pos m sat) > (3000.*.1000.) then
       vm_execute_one_step_and_record (emp_dumper m) 
     else
       ecc_jaeger (Kurden_approximator.follow m) sat
   in
   let lastpos = (vm_read_ourpos m) in
   let m = step m in
   
   
   let speed = (vec_length (vec_minus lastpos (vm_read_ourpos m))) in
   let strat = (calc_strat m sat lastpos) in
   let time1,pos1,_ = Speculate.get_closest_info m strat.perige in
    
   
   let myperige = vec_scale_to (vec_length lastpos) strat.perige in
   
   let do_bielliptic m inter = 
     Printf.printf "entering bielliptic\n";flush stdout;
     let lastpos = vm_read_ourpos m in
     let m = step m in
     let pos = vm_read_ourpos m in
      let thrust1,thrust2,thrust3,time2,time3,_,_ = 
	Bielliptic.bielliptic_rel lastpos pos strat.sat_circ_orbit inter
      in
      let time2,time3 = (int_of_float time2),(int_of_float time3) in
      Printf.printf "TIMES %d %d \n" time2 time3; flush stdout;
      
      let rec loop m = 
	if m.timestep = time2 then
	  loop (step (vm_write_thrust m thrust2))
	else if m.timestep = time3 then 
	  (step (vm_write_thrust m thrust3))
	else
	  loop (step (vm_write_thrust m zerothrust))
      in
      
      let m = step (vm_write_thrust m thrust1) in 
      loop m 
    in
    
    let to_target_circ m = 
      let rec loop m = 
	if m.timestep = time1 then (
	  Printf.printf "at \"perigy\" %d!\n" m.timestep; flush stdout;
	  let inter = calc_inter (vec_length (vm_read_ourpos m)) strat.sat_circ_orbit strat.sat_at_perige in 
	  do_bielliptic m inter)
	else
	  let m = vm_write_thrust m (0.,0.) in
	  let m = step m in
	  loop m 
      in
      loop m
    in
  
  (* 
  let nothing m = 
    Printf.printf "NOTHING!\n" ;
    let rec loop m = 
      loop (step (vm_write_thrust m zerothrust))
    in
    loop m
  in
  *)
  
  Kurden_approximator.follow (to_target_circ m)
  

let aktuator m = 
  ecc_jaeger m 0
    
(* 
   let rec loop m lastpos strat = 
    let thrust1 = hohmann_thrust1 lastpos (vm_read_ourpos m) strat.sat_circ_orbit in
    let thrust2 = hohmann_thrust2 lastpos (vm_read_ourpos m) strat.sat_circ_orbit in
    let lastpos = (vm_read_ourpos m) in
    (* Printf.printf "excentric aktuator: %d %d \n" strat.homanstart
       m.timestep; flush stdout;*)
    if m.timestep = strat.homanstart then
      (let x,y = thrust1 in
      Printf.printf "excentric aktuator: thrust %f %f\n" x y;flush stdout;
      let m = vm_write_thrust m thrust1 in
      let m = step m in
      loop m lastpos {strat with thrust2 = thrust2})
    else if m.timestep = strat.homanend then
      (let x,y = strat.thrust2 in
      Printf.printf "excentric aktuator: thrust %f %f\n" x y;flush stdout;
      let m = vm_write_thrust m strat.thrust2 in
      let m = step m in
      loop m lastpos strat)
    else
      let m = vm_write_thrust m (0.,0.) in
      let m = step m in
      loop m lastpos strat
  in
  loop m lastpos strat
*)

let get_closest_alive_sat m = 
  let sat = ref None in 
  let dist = ref 1E50 in
  
  for i = 0 to 9 do 
    if not (vm_read_sat_killed m i) then 
      let dist' = (vec_length (vm_read_sat_pos m i)) in
      if !dist < dist' then 
	(sat := Some i; dist := dist')
  done;
  !sat
    

let sky_net m = 
  let emp_dumper = Emp_dumper.get_emp_dump_writer stdout in 
  let q = m.quitter in 
  
  let jump_to_next m = 
    match get_closest_alive_sat m with 
      | Some sat -> 
	  ecc_jaeger m sat
      | None -> 
	  let rec loop m = 
	    let m = {m with quitter = q} in
	    vm_execute_one_step_and_record (emp_dumper (vm_write_thrust m zerothrust))
	  in
	  loop m 
  in
  
  let m = {m with quitter = jump_to_next} in
  jump_to_next m



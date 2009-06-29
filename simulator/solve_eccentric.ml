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
  loop start (100.*.start)
  
let zerothrust = (0.,0.)

let aktuator m = 
  let emp_dumper = Emp_dumper.get_emp_dump_writer stdout in
  let step m = vm_execute_one_step_and_record (emp_dumper m) in
  let lastpos = (vm_read_ourpos m) in
  let m = step m in

  let speed = (vec_length (vec_minus lastpos (vm_read_ourpos m))) in
  let strat = (calc_strat m 1 lastpos) in
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
	Printf.printf "at \"perigy\"!\n"; flush stdout;
	let inter = calc_inter (vec_length (vm_read_ourpos m)) strat.sat_circ_orbit strat.sat_at_perige in 
	do_bielliptic m inter)
      else
	let m = vm_write_thrust m (0.,0.) in
	let m = step m in
	loop m 
    in
    loop m
  in
  
  let nothing m = 
    Printf.printf "NOTHING!\n" ;
    let rec loop m = 
      loop (step (vm_write_thrust m zerothrust))
    in
    loop m
  in
  
  Kurden_approximator.follow (to_target_circ m)
  
  
    
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

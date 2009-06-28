open Vm;;
open Osf_reader;;
open Instructions;;

let config_to_problem config_id = 
  match config_id with
    1001 | 1002 | 1003 | 1004 -> Vm.Hohmann
  | 2001 | 2002 | 2003 | 2004 -> Vm.MeetAndGreet
  | 3001 | 3002 | 3003 | 3004 -> Vm.Eccentric
  | 4001 | 4002 | 4003 | 4004 -> Vm.ClearSky
  | _ -> failwith "unkown configuration id";;


let apply_addr_value m (addr, value) =
  match addr with 
      16000 -> {m with config = value}
    | _ -> write_actuator m addr value;;


let rec apply_frame m frame =
  match frame with
    first::tail->
      begin
	let m = apply_addr_value m first in
	  begin
	    apply_frame m tail
	  end
      end
    | _ -> m

let step m frames =
  match frames with 
      (timestamp,frame)::tail->
	if m.timestep = timestamp then
	  begin
	    let m = apply_frame m frame in
	       (m, tail)
	  end
	else
	  (m, frames)
    | _ -> (m, frames);;


let run_simulation osf_filename =
  let (config_id, framelist) = read_osf_file osf_filename in
  let problem_type = config_to_problem config_id  in
  let m = vm_init_machine problem_type in
  let rec loop m frames = 
    if not (vm_is_done m) then
      begin
	(*      Printf.printf "%d \n " t; *)
	let (m, frames) = step m frames in
	  begin
            let m = vm_execute_one_step m in
	      loop m frames 
	  end
      end
    else
      m
  in 
  let m = loop m framelist in
  let score = vm_read_sensor m 0 in
    Printf.printf "muhkuh scored: %f in move %d\n" score m.timestep;;

    
run_simulation "3003.osf"

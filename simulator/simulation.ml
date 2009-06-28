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



let x = 1;;

let step m

let run_simulation osf_filename =
  let (config_id, frames_t) = read_osf_file osf_filename in
  let frames = ref frames_t in
  let problem_type = config_to_problem config_id  in
  let m = vm_init_machine problem_type in
  let t = 0 in 
  let rec loop m t =
    step t m frames;
    if !vm_is_done then
      loop (step m frames) (t+1)
    else 
      ()
  in 
    loop m t;
    ;;



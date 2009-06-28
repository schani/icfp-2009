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
(*
let apply_frame frame =
  ();;

let step t m frames =
  match frames with 
      (timestamp,frame)::tail->if t=timestamp then
    begin
      apply_frame frame;
      tail
    end
  else
    frames
    | _ -> frames;;


let run_simulation osf_filename =
  let (config_id, frames_t) = read_osf_file osf_filename in
  let frames = ref frames_t in
  let problem_type = config_to_problem config_id  in
  let m = vm_init_machine problem_type in
  let t = ref 0 in 
  let output = ref [] in
  while not (vm_is_done m) do
    let o = step t m frames in
      output := o :: !output;
    t := !t + 1;
  done;
   ();;

*)

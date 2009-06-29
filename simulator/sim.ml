open Vm

let hohmannmode = `Bielliptic
  (* let hohmannmode = `Bielliptic*)

let get_aktuator = function 
  | Vm.Hohmann -> (match hohmannmode with 
      | `Bielliptic -> Solve_bielyptic.aktuator
      | `Hohmann ->  Solve_hohman.aktuator)
  | Vm.MeetAndGreet -> Solve_meetandgreet.aktuator
  | Vm.Eccentric -> Solve_eccentric.aktuator
  | _ -> (fun x -> 
      
      ignore(Printf.printf "final points %f\n" (-2.); flush stdout);
      exit 0;
      x
    )

let get_configs = function
  | Vm.Hohmann -> [1002] (* ;1002;1003;1004] *)
  | Vm.MeetAndGreet -> [2003] (*  [2001;2002;2003;2004]*)
  | Vm.Eccentric -> [3001]
  | Vm.ClearSky -> [4001]

let problem_by_config = function
  | 1001 | 1002 | 1003 | 1004 -> Vm.Hohmann
  | 2001 | 2002 | 2003 | 2004 -> Vm.MeetAndGreet
  | 3001 | 3002 | 3003 | 3004 -> Vm.Eccentric
  | 4001 | 4002 | 4003 | 4004 -> Vm.ClearSky
  | _ -> failwith "invalid config"
      
let doit problem config = 
  let aktuator = get_aktuator problem in 
  
  let machine = Vm.vm_init_machine problem in
  let machine = Vm.vm_configure machine config in
  let machine = Vm.vm_set_output_filename machine ((string_of_int config)^".osf") in
  
  let emp_dumper = Emp_dumper.get_emp_dump_writer stdout in

  let m = Vm.vm_execute machine (fun m -> 
    ignore(emp_dumper m);
    aktuator m) 
  in
  m

(* -s XXXX => XXXX.osf XXX.emp *)

let _ = 
  if (Array.length Sys.argv) = 2 then
    (ignore(Simulation.run_simulation Sys.argv.(1));0)
  else if (Array.length Sys.argv) = 3 then
    if Sys.argv.(1) = "-s" then
      let config = int_of_string (Sys.argv.(2)) in
      let problem = problem_by_config config in
      let _ = Emp_dumper.get_emp_dump_writer (open_out ((string_of_int config)^".emp")) in
      let m = doit problem config in
      ignore(Printf.printf "final points %f\n" (vm_read_score m); flush stdout);0
    else
      failwith "invalid commandline args"
  else
    let problem = Vm.Eccentric in
    let configs = get_configs problem in
    let rec loop = function 
      | [] -> 0
      | x::xs -> 
	  Printf.fprintf stderr "running %d: \n" x;
	  flush stdout;
	  ignore(doit problem x); loop xs
    in
    let m = loop configs in
    ignore m;0

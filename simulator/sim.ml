open Vm

let hohmannmode = `Bielliptic
  (* let hohmannmode = `Bielliptic*)

let get_aktuator = function 
  | Vm.Hohmann -> (match hohmannmode with 
      | `Bielliptic -> Solve_bielyptic.aktuator
      | `Hohmann ->  Solve_hohman.aktuator)
  | Vm.MeetAndGreet -> Solve_meetandgreet.aktuator
  | _ -> (fun x -> x)

let get_configs = function
  | Vm.Hohmann -> [1002] (* ;1002;1003;1004] *)
  | Vm.MeetAndGreet -> [2003] (*  [2001;2002;2003;2004]*)
  | Vm.Eccentric -> [3001]
  | Vm.ClearSky -> [4001]

let doit problem config = 
  let aktuator = get_aktuator problem in 
  
  let machine = Vm.vm_init_machine problem in
  let machine = Vm.vm_configure machine config in
  let machine = Vm.vm_set_output_filename machine ((string_of_int config)^".osf") in
  
  let emp_dumper = Emp_dumper.get_emp_dump_writer stdout in

  let _ = Vm.vm_execute machine (fun m -> 
    ignore(emp_dumper m);
    aktuator m) 
  in
  ()

let _ = 
  if (Array.length Sys.argv) > 1 then
    (ignore(Simulation.run_simulation Sys.argv.(1));0)
  else
    let problem = Vm.Hohmann in
    let configs = get_configs problem in
    let rec loop = function 
      | [] -> 0
      | x::xs -> 
	  Printf.fprintf stderr "running %d: \n" x;
	  flush stdout;
	  doit problem x; loop xs
    in
    loop configs

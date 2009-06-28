(* huhu Verwender dieses Programms 
 * 
 * Dies ist die kurzintro:
 * 
 * will einer einen Visualisator schreiben braucht er 7 sachen
 * 
 * 1.) eine Simulations-VM
 *)

open Vm

let hohmannmode = `Hohmann
  (* let hohmannmode = `Bielliptic*)

let get_aktuator = function 
  | Vm.Hohmann -> (match hohmannmode with 
      | `Bielliptic -> Solve_bielyptic.aktuator
      | `Hohmann ->  Solve_hohman.aktuator)
  | Vm.MeetAndGreet -> Solve_meetandgreet.aktuator
  | _ -> (fun x -> x)

let get_configs = function
  | Vm.Hohmann -> [1001] (* ;1002;1003;1004] *)
  | Vm.MeetAndGreet -> [2001] (*  [2001;2002;2003;2004]*)
  | _ -> failwith "not implementated"


(*
 * 2.) eine funktion im kopf der mensch was das ding visualisiert 
 *)
(* 4. und einen Unterstrich *)
let _ = 
  let problem = Vm.MeetAndGreet in
  let configs = get_configs problem in

  let doit problem config = 
    let aktuator = get_aktuator problem in 
    
    let visualisiere_meetandgreet = (fun m -> 
      let fuel =   vm_read_sensor m 0x1 in
      let posx =   vm_read_sensor m 0x2 in
      let posy =   vm_read_sensor m 0x3 in
      let targetx = vm_read_sensor m 0x4 in
      let targety = vm_read_sensor m 0x5 in
      (* Printf.printf "%08d %08.8f %08.8f %08.8f %08.8f\n" 
	m.timestep (-.posx) (-.posy) (targetx-.posx) (targety-.posy); *)
      (* Printf.printf "fuel %0f\n" fuel; *)
      m
    ) in
    let visualisierer = (fun m -> m) in
    let visualisierer = visualisiere_meetandgreet in 
      
    
    let machine = Vm.vm_init_machine problem in
    let machine = Vm.vm_configure machine config in
    let machine = Vm.vm_set_output_filename machine ((string_of_int config)^".osf") in
    
    let _ = Vm.vm_execute machine (fun m -> aktuator (visualisierer m)) in
    ()
  in
  let rec loop = function 
    | [] -> 0
    | x::xs -> 
	Printf.fprintf stderr "running %d: \n" x;
	flush stdout;
	doit problem x; loop xs
  in
  loop configs
(* FAQ: 
 * 
 * no questions yet.
 * 
 *)

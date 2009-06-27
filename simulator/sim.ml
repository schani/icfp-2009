(* huhu Verwender dieses Programms 
 * 
 * Dies ist die kurzintro:
 * 
 * will einer einen Visualisator schreiben braucht er 7 sachen
 * 
 * 1.) eine Simulations-VM
 *)
type strat = {first_thrust:float*float; second_thrust:float*float}

let strategy = 
  {
    first_thrust = (-5.87933562337077564, -2466.47900495061549);
    second_thrust =  (3.53485723689119924, 1482.93135803171094);
  }

let x (x,y) = x
let y (x,y) = y

  (* 
     (3.53485723689119924, 1482.93135803171094), 18875.6246799160326,
     (-42163880.2124678791, 100505.863806331559))
  *)

open Vm
(*
 * 2.) eine funktion im kopf der mensch was das ding visualisiert 
 *)
let visualisierer = 
  fun m -> 
    let fuel =   vm_read_sensor m 0x1 in
    let x =      vm_read_sensor m 0x2 in
    let y =      vm_read_sensor m 0x3 in
    let target = vm_read_sensor m 0x4 in
    Printf.printf "%d %08.8f %08.8f %08.8f %08.8f\n" m.timestep fuel x
      y fuel;
    (* do something with the above also halt leiwand zeichnen *) 
    ignore(fuel,x,y,target);
    (* aber am schluss will ich mein m zurueck.*)
    m
(*
 * 3. optional eine coole aktuator function 
 *)
let aktuator = 
  fun m -> 
    (* 
       let m = vm_write_actuator m DeltaX 0. in
       let m = vm_write_actuator m DeltaY 0. in
    *)
    match m.timestep with
      | 2 -> 
	  let m = vm_write_actuator m DeltaX (x strategy.first_thrust)
	  in
	  let m = vm_write_actuator m DeltaY (y strategy.first_thrust)
	  in
	  m
      |  18875 ->
	  let m = vm_write_actuator m DeltaX (x strategy.second_thrust)
	  in
	  let m = vm_write_actuator m DeltaY (y strategy.second_thrust)
	  in
	  m
      | _ -> 
	  let m = vm_write_actuator m DeltaX 0. in
	  let m = vm_write_actuator m DeltaY 0. in
	  m
(* 4. und einen Unterstrich *)
let _ = 
  (* 5.) eine "instanz von der vm fuer ein problem" *)
  let machine = Vm.vm_init_machine Vm.Hohmann in
  (* 6.) die will die konfiguriert sein *)
  let machine = Vm.vm_configure machine 1001 in
  let machine = Vm.vm_set_output_filename machine ((string_of_int 1001)^".sub") in
  (* 7.) Go! go! go! ... *)
  let _ = Vm.vm_execute machine (fun m -> aktuator (visualisierer m)) in
  0
    
(* FAQ: 
 * 
 * no questions yet.
 * 
 *)

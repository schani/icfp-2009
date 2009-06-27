(* huhu Verwender dieses Programms 
 * 
 * Dies ist die kurzintro:
 * 
 * will einer einen Visualisator schreiben braucht er 7 sachen
 * 
 * 1.) eine Simulations-VM
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

open Vm

type strat = 
    {
      first_thrust:float*float; 
      second_thrust:float*float;
      second_thrust_time: int;
      timetotransfer: int;
    }

(* 
   let strategy = 
   {
   first_thrust = (-5.87933562337077564, -2466.47900495061549);
   second_thrust =  (3.53485723689119924, 1482.93135803171094);
  }
*)

let x (x,y) = x
let y (x,y) = y

let pos0 = ref (0.,0.);;
let pos1 = ref (0.,0.);;
let tpos0 = ref (0.,0.);;
let tpos1 = ref (0.,0.);;
let strategy = ref {
  first_thrust = (0.,0.); 
  second_thrust = (0.,0.); 
  second_thrust_time = -100;
  timetotransfer = -100;
};;

let calc_transfer_strat () = 
  {
    first_thrust = (0.,0.); 
    second_thrust_time = -1;
    second_thrust = (0.,0.);
    timetotransfer = (int_of_float 
      (Hohmann.time_to_start 
	(x !pos0) (y !pos0) 
	(x !pos1) (y !pos1)
	(x !tpos0) (y !tpos0) 
	(x !tpos1) (y !tpos1)));
  }
         

let run_hohmann dst = 
  let thrust1,thrust2,timetosayhello,predictedpos = 
    (* Printf.printf "starting hohmann with %f %f %f %f\n"
      (x !pos0) (y !pos0) 
       (x !pos1) (y !pos1); *)
    Hohmann.hohmann 
      (x !pos0) (y !pos0) 
      (x !pos1) (y !pos1)
      dst in
  ignore(timetosayhello,predictedpos);
  {
    first_thrust = thrust1;
    second_thrust = thrust2;
    second_thrust_time = (int_of_float timetosayhello);
    timetotransfer = !strategy.timetotransfer;
  }

let square x = x*.x

let aktuator = 
  fun m -> 
    let fuel =   vm_read_sensor m 0x1 in
    let posx =   vm_read_sensor m 0x2 in
    let posy =   vm_read_sensor m 0x3 in
    let targetx = vm_read_sensor m 0x4 in
    let targety = vm_read_sensor m 0x5 in
    let targetorbit = 
      sqrt ((square (targetx-.posx))+.(square (targety-.posy))) -. 950. in
    begin 
      flush stdout;
      match m.timestep with
	| 1 -> 
	    pos0 := (posx,posy);
	    tpos0 := (targetx,targety);
	| 2 -> 
	    pos1 := (posx,posy);
	    tpos1 := (targetx,targety);
	    strategy := calc_transfer_strat ();
	    Printf.printf "strategy: do nothing until %d\n"
	      !strategy.timetotransfer;
	    flush stdout
	| t when t = !strategy.timetotransfer -> 
	    pos0 := (posx,posy)
	| t when t = !strategy.timetotransfer + 1 -> 
	    pos1 := (posx,posy)
	| _ -> 
	    (* Printf.printf "dist %d %08.8f\n" m.timestep (sqrt
	       ((targetx*.targetx) +. (targety*.targety)));*)
	    ()
    end;
	  (* Printf.printf "%d %08.8f %08.8f %08.8f %08.8f\n" m.timestep fuel x
	     y fuel; *)
    (* do something with the above also halt leiwand zeichnen *) 
    ignore(fuel,x,y);
    (* 
       let m = vm_write_actuator m DeltaX 0. in
       let m = vm_write_actuator m DeltaY 0. in
    *)
    match m.timestep with
      | t when t = !strategy.timetotransfer + 2 -> 
	  Printf.printf "starting transfer \n";
	  strategy := run_hohmann targetorbit;
	  strategy := {!strategy with second_thrust_time =
	      !strategy.second_thrust_time + t};
	  Printf.printf "hohman: %d %f %f\n" m.timestep (x !strategy.first_thrust)
	    (y !strategy.first_thrust);
	  let m = vm_write_actuator m DeltaX (x !strategy.first_thrust)
	  in
	  let m = vm_write_actuator m DeltaY (y !strategy.first_thrust)
	  in
	  m
      | step when step = !strategy.second_thrust_time ->
	  Printf.fprintf stderr "second thrust time reached\n"; flush stderr;
	  let m = vm_write_actuator m DeltaX (x !strategy.second_thrust)
	  in
	  let m = vm_write_actuator m DeltaY (y !strategy.second_thrust)
	  in
	  m
      | _ ->
	    let m = vm_write_actuator m DeltaX 0. in
	    let m = vm_write_actuator m DeltaY 0. in
	    m

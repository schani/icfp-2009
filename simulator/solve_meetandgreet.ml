open Vm

type strat = 
    {
      first_thrust:float*float; 
      second_thrust:float*float;
      second_thrust_time: int;
      timetotransfer: int;
      check: int;
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
  timetotransfer = 100000;
  check = -100;
};;

let calc_transfer_strat m = 
  (* Printf.printf "starting timecalc with %f %f %f %f"
    (x !pos0) (y !pos0) 
    (x !pos1) (y !pos1); 
  Printf.printf " %f %f %f %f\n"
    (x !tpos0) (y !tpos0) 
     (x !tpos1) (y !tpos1);  *)
  let timetotransfer = (
    (Hohmann.time_to_start 
      (x !pos0) (y !pos0) 
      (x !pos1) (y !pos1)
      (x !tpos0) (y !tpos0) 
      (x !tpos1) (y !tpos1))) 
  in
  Printf.printf "josef sagt %d\n" timetotransfer; 
  {
    first_thrust = (0.,0.); 
    second_thrust_time = -1;
    second_thrust = (0.,0.);
    timetotransfer = m.timestep + timetotransfer; 
    check = 0;
  }
         

let run_hohmann dst m = 
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
    second_thrust_time = (int_of_float timetosayhello) + m.timestep;
    timetotransfer = !strategy.timetotransfer;
    check = !strategy.check;
  }

let square x = x*.x

let aktuator = 
  fun m -> 
    let score = vm_read_sensor m 0x0 in
    let fuel =   vm_read_sensor m 0x1 in
    let posx =   vm_read_sensor m 0x2 in
    let posy =   vm_read_sensor m 0x3 in
    let targetx = vm_read_sensor m 0x4 in
    let targety = vm_read_sensor m 0x5 in
    let targetorbit = 
      sqrt ((square (targetx-.posx))+.(square (targety-.posy))) -. 950. in
    begin 
      (* flush stdout;*)
      if  m.timestep = 1 then
	(
	  pos1 := (posx,posy);
	  tpos1 := (targetx,targety)
	)
      else if m.timestep > 1 then
	( 
	  pos0 := !pos1;
	  tpos0 := !tpos1;
	  pos1 := (posx,posy);
	  tpos1 := (targetx,targety)
	);
      ()
    end;

    (* time score fuel  x y #o #sat #moon fo fo .. sx sy
       ... remifiziert *)
    Printf.printf "%i %f %f %f %f %i %i %i %f %f %s\n"
      m.timestep score fuel posx posy 0 1 0 
      (Hohmann.to_absolute posx targetx) 
      (Hohmann.to_absolute posy targety) 
      "emptag";


	  (* Printf.printf "%d %08.8f %08.8f %08.8f %08.8f\n" m.timestep fuel x
	     y fuel; *)
    (* do something with the above also halt leiwand zeichnen *) 
    ignore(fuel,x,y);
    (* 
       let m = vm_write_actuator m DeltaX 0. in
       let m = vm_write_actuator m DeltaY 0. in
    *)
    if (m.timestep < !strategy.timetotransfer) && (m.timestep >=
  2)then (
      (* Printf.printf "will_start timecalc %d\n" m.timestep; *)
      strategy := calc_transfer_strat m;
      (* Printf.printf "strategy %d do nothing until %d\n" m.timestep
	 !strategy.timetotransfer *)
    );
	
    match m.timestep with
      | t when t = !strategy.timetotransfer -> 
	  Printf.fprintf stderr "starting transfer \n"; 
	  strategy := run_hohmann targetorbit m;
	  Printf.fprintf stderr "hohman: %d %f %f\n" m.timestep (x !strategy.first_thrust)
	    (y !strategy.first_thrust);
	  let m = vm_write_actuator m DeltaX (x !strategy.first_thrust)
	  in
	  let m = vm_write_actuator m DeltaY (y !strategy.first_thrust)
	  in
	  m
      | step when step = !strategy.second_thrust_time ->
	  Printf.fprintf stderr "second thrust time reached\n"; 
	  let m = vm_write_actuator m DeltaX (x !strategy.second_thrust)
	  in
	  let m = vm_write_actuator m DeltaY (y !strategy.second_thrust)
	  in
	  m
      | _ ->
	    let m = vm_write_actuator m DeltaX 0. in
	    let m = vm_write_actuator m DeltaY 0. in
	    m

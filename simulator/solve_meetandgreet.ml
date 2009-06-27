open Vm

type strat = 
    {
      first_thrust:float*float; 
      second_thrust:float*float;
      second_thrust_time: int;
      timetotransfer: int;
      check: int;
      transfer_active: bool;
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
  transfer_active = false;
};;

let calc_transfer_strat m = 
  (* Printf.printf "starting timecalc with %f %f %f %f"
    (x !pos0) (y !pos0) 
    (x !pos1) (y !pos1); 
  Printf.printf " %f %f %f %f\n"
    (x !tpos0) (y !tpos0) 
     (x !tpos1) (y !tpos1);  *)
  let timetotransfer = (int_of_float
    (* Hohmann.time_to_start *)
    (Newtime.new_time_to_start 
      (x !pos0) (y !pos0) 
      (x !pos1) (y !pos1)
      (x !tpos0) (y !tpos0) 
      (x !tpos1) (y !tpos1))) 
  in
  {
    first_thrust = (0.,0.); 
    second_thrust_time = -1;
    second_thrust = (0.,0.);
    timetotransfer = m.timestep + timetotransfer; 
    check = 0;
    transfer_active = false;
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
    transfer_active = !strategy.transfer_active;
  }

let square x = x*.x

let length (x,y) = 
  let xx = square x in
  let yy = square y in
  sqrt (xx +. yy)
  
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
      Printf.fprintf stderr "distance to dst: %f %d\n" (length !tpos1) m.timestep;
      ()
    end;

    (* time score fuel  x y #o #sat #moon fo fo .. sx sy ... remifiziert *)
    Printf.printf "%i %f %f %f %f %i %i %i %f %f %s\n"
      m.timestep score fuel (-.posx) (-.posy) 0 1 0 
      (Hohmann.to_absolute posx targetx) 
      (Hohmann.to_absolute posy targety) 
      "emptag";
    
    if (m.timestep < !strategy.timetotransfer) && (m.timestep >= 2)then (
      strategy := calc_transfer_strat m;
      Printf.printf "strategy %d do nothing until %d\n" m.timestep !strategy.timetotransfer;
      let m = vm_write_actuator m DeltaX 0. in
      let m = vm_write_actuator m DeltaY 0. in
      m
    ) else if (m.timestep >=2 ) && (not !strategy.transfer_active) then (
	Printf.printf "comencing transfer \n"; 
	strategy := run_hohmann targetorbit m;
	strategy := {!strategy with transfer_active = true};
	Printf.printf "hohman: %d %f %f\n" m.timestep (x !strategy.first_thrust) (y !strategy.first_thrust);
	let m = vm_write_actuator m DeltaX (x !strategy.first_thrust)
	in
	let m = vm_write_actuator m DeltaY (y !strategy.first_thrust)
	in
	m
      )
      else
	match m.timestep with
	  | step when step = !strategy.second_thrust_time ->
	      Printf.printf "second thrust time reached\n"; 
	      let m = vm_write_actuator m DeltaX (x !strategy.second_thrust)
	      in
	      let m = vm_write_actuator m DeltaY (y !strategy.second_thrust)
	      in
	      m
	  | _ ->
	      let m = vm_write_actuator m DeltaX 0. in
	      let m = vm_write_actuator m DeltaY 0. in
	      m








let button_press () = 
  print_string "Dont't press this button again"

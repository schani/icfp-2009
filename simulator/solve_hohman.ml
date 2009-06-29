open Vm

type strat = 
    {
      first_thrust:float*float; 
      second_thrust:float*float;
      second_thrust_time: int;
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

let original_r = ref 0.;;
let pos0 = ref (0.,0.);;
let pos1 = ref (0.,0.);;
let strategy = ref {
  first_thrust = (0.,0.); 
  second_thrust = (0.,0.); 
  second_thrust_time = 0;
};;

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
  }

let aktuator = 
  fun m -> 
    let fuel =   vm_read_sensor m 0x1 in
    let posx =   vm_read_sensor m 0x2 in
    let posy =   vm_read_sensor m 0x3 in
    let target = (vm_read_sensor m 0x4) -. 950. in
    begin 
      match m.timestep with
	| 1 -> 
	    pos0 := (posx,posy)
	| 2 -> 
	    pos1 := (posx,posy)
	| _ -> 
	    ()
    end;
    ignore(fuel,x,y,target);
    match m.timestep with
      | 2 -> 
	  original_r := (Kurden_approximator.vec_length (posx,posy));
	  strategy := run_hohmann (vm_read_sensor m 0x4);
	  let m = vm_write_actuator m DeltaX (x !strategy.first_thrust)
	  in
	  let m = vm_write_actuator m DeltaY (y !strategy.first_thrust)
	  in
	  m
      | step when step = !strategy.second_thrust_time ->
	  let m = vm_write_actuator m DeltaX (x !strategy.second_thrust)
	  in
	  let m = vm_write_actuator m DeltaY (y !strategy.second_thrust)
	  in
	  let fuel = vm_read_fuel m in 
	  let costs = 
	    (abs_float (Hohmann.calculate_costs !original_r (vm_read_sensor m 4))) +.
	      (abs_float (Hohmann.calculate_costs (vm_read_sensor m 4) !original_r))
	  in
	  Printf.printf "costs for one more %f = %f + %f \n" costs
	    (Hohmann.calculate_costs !original_r (vm_read_sensor m 4))
	    (Hohmann.calculate_costs (vm_read_sensor m 4) !original_r)
	  ; flush;
	  if (fuel *. 0.95) > costs then
	    (
	      if (!original_r -. 
		(Kurden_approximator.vec_length (posx,posy))) < 10.
	      then
		strategy := run_hohmann (vm_read_sensor m 0x4)
	      else
		strategy := run_hohmann !original_r;
	      m
	    )
	  else
	    m
      | _ ->
	  let m = vm_write_actuator m DeltaX 0. in
	  let m = vm_write_actuator m DeltaY 0. in
	  m
	    

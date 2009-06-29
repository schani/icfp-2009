open Vm

type strat = 
    {
      first_thrust:float*float; 
      second_thrust:float*float;
      third_thrust:float*float;
      second_thrust_time: int;
      third_thrust_time:int;
    }

(* 
   let strategy = 
   {
   first_thrust = (-5.87933562337077564, -2466.47900495061549);
   second_thrust =  (3.53485723689119924, 1482.93135803171094);
  }
*)

let min_inter = 6.6E6

let x (x,y) = x
let y (x,y) = y

let pos0 = ref (0.,0.);;
let pos1 = ref (0.,0.);;
let strategy = ref {
  first_thrust = (0.,0.); 
  second_thrust = (0.,0.); 
  second_thrust_time = 0;
  third_thrust =  (0.,0.); 
  third_thrust_time = 0;
};;

let state = ref "before second"

let calc_inter fuel src dst = 
  let rec loop inter = 
    let inter' = inter /. 1.01 in
    let costs = Bielliptic.calculate_costs src inter' dst in 
    if (inter' < min_inter) || (costs > fuel) then
      inter
    else
      loop inter' 
  in
  loop (dst /. 1.01)

let run_bielleptic m dst = 
  let inter = calc_inter (vm_read_fuel m) (Kurden_approximator.vec_length !pos0) dst in
  let thrust1,thrust2,thrust3,time2,time3,predictedpos,heuslristic = 
    (* Printf.printf "starting hohmann with %f %f %f %f\n"
      (x !pos0) (y !pos0) 
       (x !pos1) (y !pos1); *)
    Bielliptic.bielliptic 
      (x !pos0) (y !pos0) 
      (x !pos1) (y !pos1)
      dst 
      inter
  in
  ignore(predictedpos,heuslristic);
  {
    first_thrust = thrust1;
    second_thrust = thrust2;
    third_thrust = thrust3;
    second_thrust_time = (int_of_float time2);
    third_thrust_time = (int_of_float time2) + (int_of_float time3);
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
    (* Printf.printf "%d %08.8f %08.8f %08.8f %08.8f\n" m.timestep fuel x
       y fuel; *)
    (* do something with the above also halt leiwand zeichnen *) 
    ignore(fuel,x,y,target);
    (* 
       let m = vm_write_actuator m DeltaX 0. in
       let m = vm_write_actuator m DeltaY 0. in
    *)
    flush stdout;
    match m.timestep with
      | 2 -> 
	  strategy := run_bielleptic m (vm_read_sensor m 0x4);
	  let m = vm_write_actuator m DeltaX (x !strategy.first_thrust)
	  in
	  let m = vm_write_actuator m DeltaY (y !strategy.first_thrust)
	  in
	  m
      | step when step = !strategy.second_thrust_time ->
	  state := "before third";
	  let m = vm_write_actuator m DeltaX (x !strategy.second_thrust)
	  in
	  let m = vm_write_actuator m DeltaY (y !strategy.second_thrust)
	  in
	  m
      | step when step = !strategy.third_thrust_time ->
	  state := "after third";
	  let m = vm_write_actuator m DeltaX (x !strategy.third_thrust)
	  in
	  let m = vm_write_actuator m DeltaY (y !strategy.third_thrust)
	  in
	  m
      | _ ->
	    let m = vm_write_actuator m DeltaX 0. in
	    let m = vm_write_actuator m DeltaY 0. in
	    m

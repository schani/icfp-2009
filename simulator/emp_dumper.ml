open Vm


let vec_len (x,y) = 
  sqrt (x*.x+.y*.y)

let vec_minus (x1,y1) (x2,y2) =
  ((x1 -. x2),(y1 -. y2))

let rel_to_abs rel me = 
  vec_minus rel me 
  
let abs_pos_to_string (x,y) = 
  Printf.sprintf "%08.8f %08.8f" x y

let our_pos_string m = 
  let x,y = vm_read_ourpos m in
  abs_pos_to_string ((-.x),(-.y))

let sat_pos_to_string m num  = 
  let me = vm_read_ourpos m in
  let sat = vm_read_sat_pos m num in
  let ax,ay = (rel_to_abs sat me) in
  abs_pos_to_string (rel_to_abs sat me)
  
let sats_string m = 
  match m.problem with 
    | Hohmann -> 0,"" 
    | Eccentric | MeetAndGreet -> 
	1,(sat_pos_to_string m 0)
    | ClearSky -> 
	let rec loop acc = function
	  | 12 -> acc
	  | num -> 
	      loop (acc^" "^(sat_pos_to_string m num)) (num+1)
	in
	12,(loop "" 0)

let moons_string m = 
  match m.problem with 
    | ClearSky -> 
	1,( abs_pos_to_string (rel_to_abs (vm_read_moon m)
	  (vm_read_ourpos m)))
    | _ -> 
	0,""
	  

let fuelstations_to_string m = 
  match m.problem with 
    | ClearSky -> 
	1,(abs_pos_to_string (rel_to_abs (vm_read_fueling m)
	  (vm_read_ourpos m)))
    | _ -> 
	0,""

let orbits_to_string m = 
  match m.problem with 
    | Hohmann -> 
	1,(Printf.sprintf "%08.8f" (vm_read_sensor m 0x4))
    | _ -> 
	0,""
  
let compose_comment m = 
  match m.problem with
    | ClearSky -> 
	let rec loop acc = function
	  | 12 -> acc
	  | num -> 
	      loop (acc^(if vm_read_sat_killed m num then "1" else "0")) (num+1)
	in
	(loop "emptrace killed" 0)
    | MeetAndGreet | Eccentric -> 
	let dist = vec_len (vm_read_sat_pos m 0) in
	("emptrace disttodst "^(string_of_float dist))
    | _ -> 
	"emptrace"

let empdumper = ref None

let get_emp_dump_writer oc = 	
  match !empdumper with 
    | None -> 
	let f = (fun m ->
	  let time = m.timestep in
	  let score = vm_read_score m in
	  let fuel = vm_read_fuel m in
	  let mypos = our_pos_string m in
	  let orbitcount,orbitstring = orbits_to_string m in
	  let satcount,satstring = sats_string m in
	  let mooncount,moonstring = moons_string m in
	  let fuelingcount,fuelingstr = fuelstations_to_string m in
	  let debugcount,debugstr = 0,"" in
	  (* time score fuel  x y #o #sat #moon #fuel #debug fo fo .. sx sy
	     ... mx my ... dx dy ... remifiziert *)
	  Printf.fprintf oc "%i %f %f %s %d %d %d %d %d %s %s %s %s %s %s\n"
	    time score fuel mypos 
	    orbitcount satcount mooncount fuelingcount debugcount
	    orbitstring satstring moonstring fuelingstr debugstr
	    (compose_comment m);
	  m
	) 
	in
	empdumper := Some f;
	f
    | Some f -> 
	f
	    
  

  

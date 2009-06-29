open Instructions

type input_port = DeltaX | DeltaY 

let input_port_number = function
  | DeltaX -> 0x2
  | DeltaY -> 0x3

type problem = 
  | Hohmann 
  | MeetAndGreet
  | Eccentric
  | ClearSky 

let get_file_for_problem = function 
  | Hohmann ->  "../angabe/bin1.obf"
  | MeetAndGreet -> "../angabe/bin2.obf"
  | Eccentric -> "../angabe/bin3.obf"
  | ClearSky -> "../angabe/bin4.obf"


type machine_state = 
    {
      problem: problem;
      teamnumber: int;
      outputfilename: string;
      datamem: float array;
      insnmem: Instructions.instruction array;
      statusreg: bool;
      instruction_pointer:int;
      input_ports: float array;
      old_input_ports: float array;
      output_ports: float array;
      timestep: int;
      config: float;
(*       writer: machine_state; *)
    }

let check_config problem = function
  | 1001 | 1002 | 1003 | 1004 ->
      problem = Hohmann
  | 2001 | 2002 | 2003 | 2004 ->
      problem = MeetAndGreet
  | 3001 | 3002 | 3003 | 3004 -> 
      problem = Eccentric
  | 4001 | 4002 | 4003 | 4004 ->
      problem = ClearSky
  | _ -> false 
  
let write_data m idx data = 
  m.datamem.(idx) <- data

let read_data m idx  = 
  m.datamem.(idx)

let write_insn m idx data = 
  m.insnmem.(idx) <- data

let read_insn m idx = 
  m.insnmem.(idx)



let alloc_machine problem memsize = 
  let data = Array.make memsize 0. in
  let insn = Array.make memsize Instructions.zero in
  {
    problem = problem;
    teamnumber = 19; 
    outputfilename = "";
    datamem = data; 
    insnmem = insn; 
    statusreg = false;
    instruction_pointer = -1;
    input_ports =  Array.make 0x4 0.;
    old_input_ports = Array.make 0x4 0.;
    output_ports = Array.make 0x66 0.;
    timestep = 0;
    config = 0.;
(*    writer = 0; *)
  }

let exchange_inputs m = 
  {m with old_input_ports = Array.copy m.input_ports}

let inputs_diff m = 
  let len = Array.length m.input_ports in
  let rec loop i acc = 
    if i = len then
      acc 
    else
      loop (i+1) (
	if m.input_ports.(i) <> m.old_input_ports.(i) then
	  (i,m.input_ports.(i))::acc
	else
	  acc
      )
  in
  loop 0 []

let fetch_insn m = 
  let m = {m with instruction_pointer = m.instruction_pointer + 1} in
  let insn = read_insn m m.instruction_pointer in
  m,insn

let is_instruction m = 
  (read_insn m m.instruction_pointer) <> Instructions.No_Instruction 
    
let save_result m res = 
  write_data m m.instruction_pointer res;
  m

let read_status m = 
  m.statusreg

let read_port m = function
  | 0x3E80 -> 
      m.config
  | port ->
    m.input_ports.(port)

let write_port m port data = 
  (* Printf.printf "writing [%d] port %d := %08.8f\n" m.timestep port data;*)
  m.output_ports.(port) <- data;
  m

let get_comparator = function
  | LTZ -> (<)
  | LEZ -> (<=)
  | EQZ -> (=)
  | GEZ -> (>=)
  | GTZ -> (>)


let insn_to_string m = function 
  | S_Instruction (Noop,_,_) -> 
      Printf.sprintf "S-->%s"  (s_code_to_string Noop)
  | S_Instruction (s,ccode,a) -> 
      Printf.sprintf "S-->%s %s %d\n(%3.10F)"  (s_code_to_string s)
	(cmpcode_to_string s ccode) a 
	(read_data m a)
  | D_Instruction (Phi as d,a1,a2) -> 
      Printf.sprintf "D-->%s %d %d status: %b" 
	(d_code_to_string d) a1 a2 (read_status m)
  | D_Instruction (d,a1,a2) -> 
      Printf.sprintf "D-->%s %d %d\n(%3.10F) (%3.10F)" 
	(d_code_to_string d) a1 a2 
	(read_data m a1) (read_data m a2)
  | No_Instruction -> "Moo-nop"



let execute_one_instruction m = 
  let m,insn = fetch_insn m in
  match insn with
    | No_Instruction -> 
	m
    | S_Instruction (Noop,_,_) ->
	m
    | S_Instruction (Cmpz,comp,addr) ->
	{m with statusreg = ((get_comparator comp) (read_data m addr) 0.)}
    | S_Instruction (Sqrt,_,addr) -> 
	let result = sqrt (read_data m addr) in
	save_result m result
    | S_Instruction (Copy,_,addr) ->
	save_result m (read_data m addr)
    | S_Instruction (Input,_,addr) -> 
	save_result m (read_port m addr)

    | D_Instruction (Output,port,addr) -> 
	write_port m port (read_data m addr)
    | D_Instruction (Phi,r1,r2) ->
	save_result m (read_data m (if read_status m then r1 else r2))
    | D_Instruction (Div,r1,r2) -> 
	let op1 = read_data m r1 in
	let op2 = read_data m r2 in
	save_result m
	  (if op2 = 0. then
	    0.
	  else
	    ( /. ) op1 op2)
    | D_Instruction (Add,r1,r2) -> 
	let op1 = read_data m r1 in
	let op2 = read_data m r2 in
	save_result m (( +. ) op1 op2)
    | D_Instruction (Mult,r1,r2) -> 
	let op1 = read_data m r1 in
	let op2 = read_data m r2 in
	save_result m (( *. ) op1 op2)
    | D_Instruction (Sub,r1,r2) -> 
	let op1 = read_data m r1 in
	let op2 = read_data m r2 in
	save_result m (( -. ) op1 op2)

    
let write_actuator m portno data = 
  m.input_ports.(portno) <- data;
  m

let vm_write_actuator m portid data = 
  write_actuator m (input_port_number portid) data

let vm_write_thrust m (x,y)  = 
  let m = vm_write_actuator m DeltaX x in
  let m = vm_write_actuator m DeltaY y in
  m

let vm_read_sensor m port =
    m.output_ports.(port)  

let vm_read_ourpos m = 
  (m.output_ports.(2), m.output_ports.(3))

let vm_read_fuel m = 
  m.output_ports.(1)

let vm_read_moon m = 
  (m.output_ports.(0x64),m.output_ports.(0x65))

let vm_read_fueling m = 
  (m.output_ports.(0x4),m.output_ports.(0x5))

let vm_read_sat_pos m num = 
  match m.problem with 
    | MeetAndGreet | Eccentric -> 
	(m.output_ports.(4), m.output_ports.(5))
    | ClearSky -> 
	(m.output_ports.(3*num+0x7), m.output_ports.(3*num+0x8))
    | _ -> failwith "there is not sat"
    
let vm_read_sat_killed m num = 
  match m.problem with 
    | ClearSky -> 
	m.output_ports.(3*num+0x9) = 1.
    | _ -> 
	false
	
let vm_read_score m = 
  m.output_ports.(0)
    

let vm_init_machine problem = 
  let ic = Basic_reader.open_obf (get_file_for_problem problem) in 
  let m = alloc_machine problem (Basic_reader.get_size ic) in
  let rec loop ic = 
    let (data,insn),(ic) = Basic_reader.basic_read_memory_line ic in
    let (_,i) = ic in 
    write_data m i data;
    write_insn m i (Instructions.decode_insn insn);
    loop ic
  in
  try 
    ignore(loop ic);
    m
  with
    | End_of_file -> 
	m

let vm_configure m config = 
  let m = vm_write_actuator m DeltaX 0E0 in
  let m = vm_write_actuator m DeltaY 0E0 in
  if check_config m.problem config then 
    
    {m with config = (float_of_int config)}
  else
    failwith "bad config"

	
(* this executes "one simulation step, i.e., 1 sec "*)
let vm_execute_one_step m = 
  let rec loop m = 
    let m = execute_one_instruction m in
    if is_instruction m then
      loop m
    else
      m	
  in
  let m = loop m in
  {m with timestep = m.timestep + 1; instruction_pointer = -1; }

let vm_execute_n_steps n m = 
  let rec loop i m = 
    if i > 0 then
      loop (i-1) (vm_execute_one_step m)
    else
      m
  in
  loop n m

let vm_is_done m = 
  let ret =   (((vm_read_sensor m 0) <> 0.) || (m.timestep = 3000000)) in
  (* score written || 3M timesteps  -> eog *)
  if ret then
    Printf.fprintf stderr "we have reached the end of the world @ %d %f\n" m.timestep (vm_read_score m);
  ret
  

let open_writer filname m = 
  let oc = Basic_writer.open_submission filname m.teamnumber
    (int_of_float m.config) in

  let write_frame m = 
    let diff = inputs_diff m in
    let diff = 
      if m.timestep <> 0 then
	diff
      else
	(0x3E80,m.config)::diff
    in
    if diff <> [] then
      Basic_writer.output_frame oc (m.timestep,diff);
    exchange_inputs m;
  in
  let close_write m = 
    Basic_writer.output_end_of_submission oc m.timestep;
    Basic_writer.close_submission oc;
    ()
  in
  write_frame,close_write


let vm_set_output_filename m name = 
  {m with outputfilename = name}


let vm_execute m controller = 
  let writer,closer = 
    if m.outputfilename <> "" then
      open_writer m.outputfilename m 
    else
      (fun m -> m),(fun m -> ())
  in
  let rec loop m = 
    let m = writer m in
    let m = vm_execute_one_step m in
    if vm_is_done m then
      (closer m; m)
    else
      loop (controller m)
  in
  let m = loop m in 
  m 
    

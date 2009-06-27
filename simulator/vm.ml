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
  | ClearSky -> failwith "streber!" (* "../angabe/bin4.obf" *)

module OrderedInt = 
  struct
    type t = int
    let compare = Pervasives.compare
  end
module IntMap = Map.Make(OrderedInt)


type machine_state = 
    {
      problem: problem;
      datamem: float array;
      insnmem: Instructions.instruction array;
      statusreg: bool;
      instruction_pointer:int;
      (* todo make these faster! *) 
      input_ports: float IntMap.t;
      output_ports: float IntMap.t;
      timestep: int;
    }

let memsize = 0x4000

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


let alloc_machine problem = 
  let data = Array.make memsize 0. in
  let insn = Array.make memsize Instructions.zero in
  {
    problem = problem;
    datamem = data; 
    insnmem = insn; 
    statusreg = false;
    instruction_pointer = -1;
    input_ports = IntMap.empty;
    output_ports = IntMap.empty;
    timestep = 0;
  }

let fetch_insn m = 
  let m = {m with instruction_pointer = m.instruction_pointer + 1} in
  let insn = read_insn m m.instruction_pointer in
  m,insn

let is_instruction m = 
  (read_insn m m.instruction_pointer) <> Instructions.No_Instruction 
    
let save_result m res = 
  (* Printf.printf "schani: v%d:=%f\n" m.instruction_pointer res; *)
  write_data m m.instruction_pointer res;
  m

let read_status m = 
  m.statusreg

let read_port m port = 
  try 
    let result = IntMap.find port m.input_ports in
    (* Printf.printf "reading from port %d -> %f\n" port result; *)
    result
  with 
      Not_found -> failwith ("read_port "^(string_of_int port))

let write_port m port data = 
 (* Printf.printf "writing %f to port %d\n" data port;  *)
  {m with output_ports = IntMap.add port data m.output_ports}

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
  (* Printf.printf "%d %s\n" m.instruction_pointer 
     (insn_to_string m insn);  *)
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
  {m with input_ports = IntMap.add portno data m.input_ports}

let vm_write_actuator m portid data = 
  write_actuator m (input_port_number portid) data

let vm_read_sensor m address = 
  try 
    IntMap.find  address m.output_ports
  with 
      Not_found -> 
	0.

let vm_init_machine problem = 
  let m = alloc_machine problem in
  let ic = Basic_reader.open_obf (get_file_for_problem problem) in 
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
    write_actuator m 0x3E80 (float_of_int config)
  else
    failwith "bad config"

	
(* this executes "one simulation step, i.e., 1 sec "*)
let vm_execute_one_step m = 
  (* let m = {m with datamem = Array.copy m.datamem} in *)
  let rec loop m = 
    let m = execute_one_instruction m in
    if is_instruction m then
      loop m
    else
      m	
  in
  let m = loop m in
  {m with timestep = m.timestep + 1; instruction_pointer = -1; }

let vm_execute m controller = 
  let rec loop m = 
    let m = vm_execute_one_step m in
(*    Array.iteri (fun i f -> Printf.printf "DUMP %d %f\n" i f) m.datamem;*)
    Printf.printf "%c%07d" (Char.chr 0x0d) m.timestep; 
    if ((vm_read_sensor m 0) <> 0.) || (m.timestep = 3000000) then
      (* score written -> eog || 3M timesteps*)
      m
    else
      loop (controller m)
  in
  let m = loop m in 
  Printf.printf "muhkuh scored: %f\n" (vm_read_sensor m 0);
  m 
    

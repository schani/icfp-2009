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
      input_ports: float IntMap.t;
      output_ports: float IntMap.t;
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
  }

let fetch_insn m = 
  let ip =  !m.instruction_pointer in
  m := {!m with instruction_pointer = ip + 1};
  read_insn !m ip

let save_result m res = 
  write_data !m !m.instruction_pointer res

let read_status m = 
  !m.statusreg

let read_port m port = 
  try 
    IntMap.find port m.input_ports
  with 
      Not_found -> failwith ("read_port "^(string_of_int port))

let write_port m port data = 
  m := {!m with output_ports = IntMap.add port data !m.output_ports}

let get_comparator = function
  | LTZ -> (<)
  | LEZ -> (<=)
  | EQZ -> (=)
  | GEZ -> (>=)
  | GTZ -> (>)

let get_operation = function
  | Add ->  ( +. )
  | Sub ->  ( -. )
  | Mult -> ( *. )
  | _ -> failwith "get_operation called with an op (Div,Phi,Output) that is handled directly."


let execute_one_instruction m = 
  match fetch_insn m with
    | No_Instruction -> 
	`Done
    | S_Instruction (Noop,_,_) ->
	`More
    | S_Instruction (Cmpz,comp,addr) ->
	m := {!m with statusreg = ((get_comparator comp) (read_data !m addr) 0.)};
	`More
    | S_Instruction (Sqrt,_,addr) -> 
	let result = sqrt (read_data !m addr) in
	save_result m result;
	`More
    | S_Instruction (Copy,_,addr) ->
	save_result m (read_data !m addr);
	`More
    | S_Instruction (Input,_,addr) -> 
	save_result m (read_port !m addr);
	`More

    | D_Instruction (Output,port,addr) -> 
	write_port m port (read_data !m addr);
	`More
    | D_Instruction (Phi,r1,r2) ->
	save_result m (read_data !m (if read_status m then r1 else r2));
	`More
    | D_Instruction (Div,r1,r2) -> 
	let op1 = read_data !m r1 in
	let op2 = read_data !m r2 in
	save_result m
	  (if op2 = 0. then
	    0.
	  else
	    ( /. ) op1 op2);
	`More
    | D_Instruction (op,r1,r2) -> 
	let op1 = read_data !m r1 in
	let op2 = read_data !m r2 in
	save_result m ((get_operation op) op1 op2);
	`More
	  	
	
	

(* this executes "one simulation step, i.e., 1 sec "*)
let vm_execute_one_step m = 
  let m = {m with datamem = Array.copy m.datamem} in
  let m = ref m in
  while execute_one_instruction m <> `Done do
    ()
  done;
  !m
    
let write_actuator m portno data = 
  {m with input_ports = IntMap.add portno data m.input_ports}

let vm_write_actuator m portid data = 
  write_actuator m (input_port_number portid) data

let vm_read_sensor m address = 
  IntMap.find m address


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
    loop ic;
    m
  with
    | End_of_file -> 
	m

let vm_configure m config = 
  let m = vm_write_actuator m DeltaX 0. in
  let m = vm_write_actuator m DeltaY 0. in
  if check_config m.problem config then 
    write_actuator m 0x3E80 (float_of_int config)
  else
    failwith "bad config"

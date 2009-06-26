open Instructions

type machine_state = 
    {
      datamem: float array;
      insnmem: Instructions.instruction array;
      statusreg: bool;
    }

let alloc_machine () = 
  let data = Array.make 0x4000 0. in
  let insn = Array.make 0x4000 Instructions.zero in
  {datamem = data; insnmem = insn; statusreg = false}

let write_data m idx data = 
  m.datamem.(idx) <- data

let read_data m idx  = 
  m.datamem.(idx)

let write_insn m idx data = 
  m.insnmem.(idx) <- data

let read_insn m idx = 
  m.insnmem.(idx)

let read_memory_from_file filename = 
  let m = alloc_machine () in
  let ic = Basic_reader.open_obf filename in 
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


let one_instruction 



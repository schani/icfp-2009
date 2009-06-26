
let alloc_memories () = 
  let data = Array.make 0x4000 0. in
  let insn = Array.make 0x4000 Instructions.zero in
  data,insn

let write_data (mem,_) idx data = 
  Printf.printf "<%f>\n" (data);
  mem.(idx) <- data

let read_data (mem,_) idx  = 
  mem.(idx)

let write_insn (_,mem) idx data = 
  mem.(idx) <- data

let read_insn (_,mem) idx = 
    mem.(idx)

let read_memory_from_file filename = 
  let mem = alloc_memories () in
  let ic = Basic_reader.open_obf filename in 
  let rec loop ic = 
    let (data,insn),(ic) = Basic_reader.basic_read_memory_line ic in
    let (_,i) = ic in 
    write_data mem i data;
    write_insn mem i (Instructions.decode_insn insn);
    loop ic
  in
  try 
    loop ic;
    mem
  with
    | End_of_file -> 
	mem

	  

let _ = 
  let mem = read_memory_from_file "../angabe/bin1.obf" in
  for i = 0 to 0x4000-1 do
    Printf.printf "%d %f\n" i (read_data mem i)
  done 

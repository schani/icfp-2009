
type memfile = in_channel * int

let input_data ic = 
  let i64 = ref Int64.zero in
  for i = 0 to 7 do
    i64 := Int64.logor !i64
      (Int64.shift_left (Int64.of_int (input_byte ic)) (i*8));
  done;
  Int64.float_of_bits !i64 
    
    
let input_insn ic = 
  let i32 = ref Int32.zero in
  for i = 0 to 3 do
    i32 := Int32.logor !i32
      (Int32.shift_left (Int32.of_int (input_byte ic)) ((i)*8));
  done;
  !i32 
    
let open_obf filename = 
  let ic = open_in_bin filename in
  ic,-1

let basic_read_memory_line (ic,i) = 
  let i = i + 1 in
  let data,insn = 
    if i mod 2 = 0 then (* even: *)
      let data = input_data ic in
      let insn = input_insn ic in
      (data,insn)
    else (* odd *) 
      let insn = input_insn ic in
      let data = input_data ic in
      (data,insn)
  in
  (data,insn),(ic,i)

let get_size (ic,_)     = 
  let ret = ((in_channel_length ic) / (8+4)) in
  (ret + 1)

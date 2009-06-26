
type memfile = in_channel * int

let downfrom7 = [7;6;5;4;3;2;1;0];;
let downfrom3 = [3;2;1;0];;

let input_data ic = 
  let zero = Int64.zero in
  let setbyte x shift = 
    Int64.logor (Int64.shift_left (Int64.of_int (input_byte ic))
      shift) x
  in
  let i64 = List.fold_left setbyte zero downfrom7 in
  Printf.printf ">%s\n" (Int64.to_string i64);
  Int64.float_of_bits i64 
    
    
let input_insn ic = 
  let zero = Int32.zero in
  let setbyte x shift = 
    Int32.logor (Int32.shift_left (Int32.of_int (input_byte ic))
      shift) x 
  in
  let i32 = List.fold_left setbyte zero downfrom3 in
  i32
    
let open_obf filename = 
  let ic = open_in_bin filename in
  ic,0

let basic_read_memory_line (ic,i) = 
  let ret = 
    if i mod 2 = 0 then (* even: *)
      let data = input_data ic in
      let insn = input_insn ic in
      (data,insn),(ic,i+1)
    else (* odd *) 
      let insn = input_insn ic in
      let data = input_data ic in
      (data,insn),(ic,i+1)
  in
  print_int i; print_newline ();
  ret
      
      
    


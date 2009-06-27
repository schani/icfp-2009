(* timestep * updates list *)
type output_frame = int * (int*float) list

let lsr64 = Int64.shift_right
let lsr32 = Int32.shift_right


let outputi64 oc i64 = 
  for i = 0 to 7 do
    output_byte oc ((Int64.to_int (lsr64 i64 (i*8))) land 0xff);
  done;
  ()

let outputi32 oc i32 = 
  for i = 0 to 3 do
    output_byte oc ((Int32.to_int (lsr32 i32 (i*8))) land 0xff);
  done;
  ()

let outputi oc i = 
  outputi32 oc (Int32.of_int i)

let outputf oc f = 
  outputi64 oc (Int64.bits_of_float f)

let output_header oc teamnumber configuration = 
  let magic = 0xCAFEBABEl in
    outputi32 oc magic;
    outputi32 oc (Int32.of_int teamnumber);
    outputi32 oc (Int32.of_int configuration);
    ()

let output_frame oc (timestep,liste) = 
  let count = List.length liste in
    outputi oc timestep;
    outputi oc count;
    let rec loop = function
      | [] -> ()
      | (addr,value)::xs -> 
	  outputi oc addr;
	  outputf oc value;
	  loop xs
    in loop liste
  

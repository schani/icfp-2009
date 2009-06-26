type addr = int

type d_instruction = Add | Sub | Mult | Div | Output | Phi 

type comparison = LTZ | LEZ | EQZ | GEZ | GTZ 
type s_instruction = | Noop 
		     | Cmpz 
		     | Sqrt
		     | Copy 
		     | Input
 

type instruction = | D_Instruction of d_instruction * addr * addr
		   | S_Instruction of s_instruction * comparison * addr 


let zero = S_Instruction (Noop,LTZ,0)

let badcode str code = 
  failwith (Printf.sprintf "Bad Instruction: %x" code)


let shift_right_to_int i32 shift mask = 
  (Int32.to_int (Int32.shift_right i32 shift)) land mask

let s_code_of_int = function 
  | 0x0 -> Noop 
  | 0x1 -> Cmpz
  | 0x2 -> Sqrt 
  | 0x3 -> Copy 
  | 0x4 -> Input
  | x -> badcode "s_code_of_int" x
      
let cmpcode_of_int = function
  | 0x0 -> LTZ 
  | 0x1 -> LEZ 
  | 0x2 -> EQZ 
  | 0x3 -> GEZ
  | 0x4 -> GTZ
  | x -> badcode "cmpcode_of_int" x

let d_code_of_int = function
  | 0x1 -> Add 
  | 0x2 -> Sub
  | 0x3 -> Mult
  | 0x4 -> Div
  | 0x5 -> Output 
  | 0x6 -> Phi 
  | x -> badcode "d_code_of_int" x 

let decode_insn insn =
  match shift_right_to_int insn 28 0xf with 
    | 0x0 -> (* S_Instruction *)
	let scode = s_code_of_int  (shift_right_to_int insn 24   0xf) in
	let ccode = cmpcode_of_int (shift_right_to_int insn 14 0x1ff) in
	let r1    = shift_right_to_int insn 0 0x3fff in
	S_Instruction (scode,ccode,r1)
    | d_code -> 
	let r1 = shift_right_to_int insn 14 0x3fff in
	let r2 = shift_right_to_int insn  0 0x3fff in
	D_Instruction ((d_code_of_int d_code),r1,r2)
	


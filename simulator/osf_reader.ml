open Basic_reader;;

(* 

reads on osf file into a list 
   
result = [(timestamp, [(addr, value), (addr, value), ...]), ...] 

*)

(* read 64bit float from buffer and shift buffer *)
let input_float =
  Basic_reader.input_data;;

(* read 32bit int from buffer and shift buffer *)
let input_int32 =
  Basic_reader.input_insn;;

(* open biniary osf file for input parsing *)
let open_osf filename =
  open_in_bin filename;;

(* read header - returns scenario id
   returns int
*)
let read_header ic =
  let cafebabe =Int32.to_int (input_int32 ic) in
  let team_id = Int32.to_int (input_int32 ic) in
  let scenario_id = Int32.to_int (input_int32 ic) in
    Printf.printf "Header: %x %d %d\n" cafebabe team_id scenario_id;
    scenario_id;;

(* read an int32, float64 tupl - returns (int, float) *)
let read_value ic =
  let addr = Int32.to_int (input_int32 ic) in
  let value = input_float ic in
  Printf.printf "  [%d, %f]" addr value;
  (addr, value);;

(* read a time frame
   constists of c (addr, value) tuples and one timestamp
   (int, [(int, float), (int, float), ...])
*)
let read_frame ic =
  let t = Int32.to_int (input_int32 ic) in
  let c = Int32.to_int (input_int32 ic) in
  let data = ref [] in
  Printf.printf "%d (c:%d): " t c;
  for i = 1 to c do
    let v = read_value (ic) in
      data := v :: !data; 
  done;
  Printf.printf "\n";
  (t, !data);;
  
(* read and parse an osf file
   returns n frames. the last frame contains no (addr, value) tuples.
   [(int, [(int, float), (int, float), ...]), ...]
*)
let read_osf_file filename =
  let ic = open_osf filename in
  let frames = ref [] in
  Printf.printf "Read file %s\n" filename; 
    let scenario_id = read_header ic in
  try
    while true do
      let frame = read_frame ic in
	frames := frame :: !frames;
    done;
    (0, []) (* not reached *)
  with End_of_file ->
    close_in ic;
    scenario_id, (List.rev !frames);;

(* test *)
(*
read_osf_file "3003.osf";;
*)

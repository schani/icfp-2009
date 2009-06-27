(* vmbridge: interfaces different VMs
 *
 *  stamp score fuel x y orbit# [..] sat# [.. ..] rem
 *)

let whitepages_regexp = Str.regexp " +"
type q = {
  mutable step :(int -> int * float * float * float * float
		   * float list * (float * float) list * string);
}

exception Parse_error of string

let q = {
  step = function _ -> 0, 0.0, 0.0, 0.0, 0.0, [], [], "rem";
}

let setup_file filename =
  let ifi = open_in filename
  in let rec step = function
    | howmuch when howmuch > 1 -> (* skip some *)
	ignore (input_line ifi);
	step (howmuch - 1)
    | _ ->
	let rec parse_zeug ?(orbits=[]) ?(sats=[])
	    orbitcount satcount = function
	      | orb :: r when orbitcount > 0 ->
		  Printf.fprintf stderr "1111111111111111111 orb=%s\n" orb;
		  parse_zeug ~orbits:((float_of_string orb) :: orbits)
		    ~sats (orbitcount - 1) satcount r
	      | satx :: saty :: r when satcount > 0 ->
		  Printf.fprintf stderr "2222222222222222222\n";
		  parse_zeug ~orbits ~sats:(((float_of_string satx),
					     (float_of_string saty)) :: sats)
		    orbitcount (satcount - 1) r
	      | _ ->
		  Printf.fprintf stderr "3333333333333\n";
		  (List.rev orbits), (List.rev sats), "REM"
	      | [] ->
		  Printf.fprintf stderr "DER DERCK IS LEER*\n"; flush stderr;
		  raise (Parse_error "parse_zeug failed")
	      | _ ->
		  Printf.fprintf stderr "SCHASS IM VOID *\n"; flush stderr;
		  raise (Parse_error "parse_zeug failed")
	in
	let line = input_line ifi
	in
	  try
	    Scanf.sscanf line "%i %f %f %f %f %i %i %[^]]"
	      (fun stamp score fuel x y orbitcount satcount str ->
		 let zeug = Str.split whitepages_regexp str
		 in
		   Printf.fprintf stderr "rest=%s orbitcount=%i satcount=%i\n"
		     str orbitcount satcount;
		   let orbits, sats, rem =
		     parse_zeug orbitcount satcount zeug
		   in
		     Printf.fprintf stderr "parsed a line!\n"; flush stderr;
		     stamp, score, fuel, x, y, orbits, sats, rem
	      )
	  with
	      x ->
		Printf.fprintf stderr "SCHASS IM VOID\n";
		Printf.fprintf stderr "line=%s\n" line;
		Printf.fprintf stderr "boese exception: %s\n"
		  (Printexc.to_string x);
		flush stderr;
		raise (Parse_error "parse failed")
  in
    q.step <- step;
    q

let setup_signof () =
  let step howmuch =
    0, 0.0, 0.0, 0.0, 0.0, [], [], "rem";
  in
    q.step <- step;
    q

(*
      in let machine = Vm.vm_init_machine Vm.Hohmann
    in let machine = Vm.vm_configure machine 1001

*)

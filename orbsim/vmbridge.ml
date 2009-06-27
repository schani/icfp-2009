(* vmbridge: interfaces different VMs
 *
 *  stamp score fuel x y orbit# [..] sat# [.. ..] rem
 *)

let whitepages_regexp = Str.regexp " +"
type q = {
  mutable step :(int -> int * float * float * float * float * float list *
		   (float * float) list * (float * float) list *string);
}

exception Parse_error of string

let q = {
  step = function _ -> 0, 0.0, 0.0, 0.0, 0.0, [], [], [], "rem";
}

let setup_file filename =
  let ifi = open_in filename
  in let rec step = function
    | howmuch when howmuch > 1 -> (* skip some *)
	ignore (input_line ifi);
	step (howmuch - 1)
    | _ ->
	let rec parse_zeug ?(orbits=[]) ?(sats=[]) ?(moons=[])
	    orbitcount satcount mooncount = function
	      | orb :: r when orbitcount > 0 ->
		  parse_zeug ~orbits:((float_of_string orb) :: orbits)
		    ~sats (orbitcount - 1) satcount mooncount r
	      | satx :: saty :: r when satcount > 0 ->
		  parse_zeug ~orbits ~sats:(((float_of_string satx),
					     (float_of_string saty)) :: sats)
		    orbitcount (satcount - 1) mooncount r
	      | moonx :: moony :: r when mooncount > 0 ->
		  parse_zeug ~orbits ~sats
		    ~moons:(((float_of_string moonx),
			     (float_of_string moony)) :: moons)
		    orbitcount satcount (mooncount - 1) r
	      | _ ->
		  (List.rev orbits), (List.rev sats), (List.rev moons), "REM"
		    (*
	      | [] ->
		  Printf.fprintf stderr "DER ..IS LEER*\n"; flush stderr;
		  raise (Parse_error "parse_zeug failed")
	      | _ ->
		  Printf.fprintf stderr "MUELL IM VOID *\n"; flush stderr;
		  raise (Parse_error "parse_zeug failed")
		    *)
	in
	let line = input_line ifi
	in
	  try
	    Scanf.sscanf line "%i %f %f %f %f %i %i %i %[^]]"
	      (fun stamp score fuel x y orbitcount satcount mooncount str ->
		 let zeug = Str.split whitepages_regexp str
		 in
		   let orbits, sats, moons, rem =
		     parse_zeug orbitcount satcount mooncount zeug
		   in
		     stamp, score, fuel, x, y, orbits, sats, moons, rem
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
    0, 0.0, 0.0, 0.0, 0.0, [], [], [], "rem";
  in
    q.step <- step;
    q

(*
      in let machine = Vm.vm_init_machine Vm.Hohmann
    in let machine = Vm.vm_configure machine 1001

*)

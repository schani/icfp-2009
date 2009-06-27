(* vmbridge: interfaces different VMs
 *)

type q = {
  mutable step :(int -> int * float * float * float * float * float);
}

exception End_of_trace

let q = {
  step = function _ -> 0, 0.0, 0.0, 0.0, 0.0, 0.0;
}

let setup_file filename =
  let ifi = open_in filename
  in let rec step = function
    | howmuch when howmuch > 1 -> (* skip some *)
	ignore (input_line ifi);
	step (howmuch - 1)
    | _ ->
	try
	  Scanf.sscanf (input_line ifi) "%i %f %f %f %f %f"
            (fun stamp score fuel x y muh -> stamp, score, fuel, x, y, muh)
	with
	    _ -> raise End_of_trace
  in
    q.step <- step;
    q

let setup_signof () =
  let step howmuch =
    0, 0.0, 0.0, 0.0, 0.0, 0.0
  in
    q.step <- step;
    q

(*
      in let machine = Vm.vm_init_machine Vm.Hohmann
    in let machine = Vm.vm_configure machine 1001

*)

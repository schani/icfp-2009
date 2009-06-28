open Vm

let do_nothing m =    
  vm_write_thrust m (0.,0.)
    
  
let lookahead m time func = 
  let m' = {m with datamem = (Array.copy m.datamem)} in
  let rec loop i m = 
    if i = 0 then
      m
    else
      let m =  (vm_execute_one_step (func m)) in
      if vm_is_done m then 
	m 
      else
	loop (i-1) m
  in
  loop time m' 
    
let do_we_win m = 
  let m = lookahead m 900 do_nothing in
  if vm_is_done m then
    Some(m)
  else
    None

let skip_time m time = 
  lookahead m time do_nothing 

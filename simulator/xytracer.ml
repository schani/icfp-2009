let make_tracer filename = 
  let oc = open_out filename in
  fun m -> 
    let x = Vm.vm_read_sensor m 0x2 in
    let y = Vm.vm_read_sensor m 0x3 in
    Printf.fprintf oc "%f,%f\n" x y;
    m

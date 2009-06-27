

let _ = 
  let machine = Vm.vm_init_machine Vm.Hohmann in
  let machine = Vm.vm_configure machine 1001 in
  let tracer = Xytracer.make_tracer "/tmp/output.dump" in
  let _ = Vm.vm_execute machine tracer in
  1
    

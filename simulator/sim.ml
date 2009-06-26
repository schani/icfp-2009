

  



let _ = 
  let machine = Vm.vm_init_machine Vm.Hohmann in
  let machine = Vm.vm_configure machine 1001 in
  let _ = machine in
  1

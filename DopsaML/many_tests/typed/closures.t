  $ ../../bin/DopsaML.exe manytests/typed/010faccps_ll.ml
  $ clang-18 --target=riscv64-linux-gnu -Wno-override-module -c out.ll -o prog.o
  $ clang-18 --target=riscv64-linux-gnu -c ../../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc -static prog.o runtime.o -o prog.exe
  $ qemu-riscv64 prog.exe
  24

  $ ../../bin/DopsaML.exe manytests/typed/010fibcps_ll.ml
  $ clang-18 --target=riscv64-linux-gnu -Wno-override-module -c out.ll -o prog.o
  $ riscv64-linux-gnu-gcc -static prog.o runtime.o -o prog.exe
  $ qemu-riscv64 prog.exe
  8

  $ ../../bin/DopsaML.exe manytests/typed/012faccps.ml
  $ clang-18 --target=riscv64-linux-gnu -Wno-override-module -c out.ll -o prog.o
  $ riscv64-linux-gnu-gcc -static prog.o runtime.o -o prog.exe
  $ qemu-riscv64 prog.exe
  720

  $ ../../bin/DopsaML.exe manytests/typed/012fibcps.ml
  $ clang-18 --target=riscv64-linux-gnu -Wno-override-module -c out.ll -o prog.o
  $ riscv64-linux-gnu-gcc -static prog.o runtime.o -o prog.exe
  $ qemu-riscv64 prog.exe
  8

  $ ../../bin/DopsaML.exe manytests/typed/004manyargs.ml
  $ clang-18 --target=riscv64-linux-gnu -Wno-override-module -c out.ll -o prog.o
  $ riscv64-linux-gnu-gcc -static prog.o runtime.o -o prog.exe
  $ qemu-riscv64 prog.exe
  1111111111
  1
  10
  100

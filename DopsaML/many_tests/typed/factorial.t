  $ ../../bin/DopsaML.exe manytests/typed/001fac.ml
  $ clang-18 --target=riscv64-linux-gnu -Wno-override-module -c out.ll -o prog.o
  $ clang-18 --target=riscv64-linux-gnu -c ../../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc -static prog.o runtime.o -o prog.exe
  $ qemu-riscv64 prog.exe
  24

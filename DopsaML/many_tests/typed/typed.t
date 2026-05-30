  $ ../../bin/DopsaML.exe 010fac_anf.ml
  $ clang-18 --target=riscv64-linux-gnu -Wno-override-module -c out.ll -o fac.o
  $ clang-18 --target=riscv64-linux-gnu -c ../../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc -static fac.o runtime.o -o fac.exe
  $ qemu-riscv64 fac.exe
  24

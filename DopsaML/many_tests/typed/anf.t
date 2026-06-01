  $ ../../bin/DopsaML.exe manytests/typed/010fac_anf.ml
  $ clang-18 --target=riscv64-linux-gnu -Wno-override-module -c out.ll -o prog.o
  $ clang-18 --target=riscv64-linux-gnu -c ../../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc -static prog.o runtime.o -o prog.exe
  $ qemu-riscv64 prog.exe; echo $?
  24

  $ ../../bin/DopsaML.exe manytests/typed/003fib.ml
  $ clang-18 --target=riscv64-linux-gnu -Wno-override-module -c out.ll -o prog.o
  $ riscv64-linux-gnu-gcc -static prog.o runtime.o -o prog.exe
  $ qemu-riscv64 prog.exe
  3
  3

  $ cat > test_print.ml << 'EOM'
  > let large x = if x <> 0 then print_int 0 else print_int 1
  > let main =
  >   let x = if 1 < 2 then 7 else 0 in
  >   let () = print_int 42 in
  >   large x
  > EOM
  $ ../../bin/DopsaML.exe test_print.ml
  $ clang-18 --target=riscv64-linux-gnu -Wno-override-module -c out.ll -o prog.o
  $ riscv64-linux-gnu-gcc -static prog.o runtime.o -o prog.exe
  $ qemu-riscv64 prog.exe
  42
  0

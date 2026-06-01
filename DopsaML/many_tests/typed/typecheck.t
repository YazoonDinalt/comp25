  $ ../../bin/DopsaML.exe manytests/typed/009let_poly.ml -o /dev/null
  $ ../../bin/DopsaML.exe manytests/typed/008ascription.ml -o /dev/null

  $ ../../bin/DopsaML.exe manytests/do_not_type/001.ml -o /dev/null
  Type error: Typechecker error: undefined variable 'fac'
  [1]
  $ ../../bin/DopsaML.exe manytests/do_not_type/002if.ml -o /dev/null
  Type error: Typechecker error: unification failed on int and bool
  [1]
  $ ../../bin/DopsaML.exe manytests/do_not_type/003occurs.ml -o /dev/null
  Type error: Occurs check failed. Type variable '2 occurs inside 'a -> 'b.
  [1]
  $ ../../bin/DopsaML.exe manytests/do_not_type/004let_poly.ml -o /dev/null
  Type error: Typechecker error: unification failed on bool and int
  [1]
  $ ../../bin/DopsaML.exe manytests/do_not_type/015tuples.ml -o /dev/null
  Type error: Typechecker error: empty pattern
  [1]

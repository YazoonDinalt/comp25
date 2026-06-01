let main =
  let p = (1, (2, 3)) in
  let (a, (b, c)) = p in
  print_int (a + b + c)

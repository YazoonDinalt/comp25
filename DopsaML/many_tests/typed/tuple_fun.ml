let main =
  let g = (1, (2, (fun x -> x + 1))) in
  let (a, (b, f)) = g in
  print_int (f (a + b))

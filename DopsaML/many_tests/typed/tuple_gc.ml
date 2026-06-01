let main =
  let p = (1, 2) in
  let () = collect () in
  let (a, b) = p in
  print_int (a + b)

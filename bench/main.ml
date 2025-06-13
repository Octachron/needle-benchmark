
let exp scale () = int_of_float (-. scale *. log (Random.float 1.))

let substring len text =
  let len = len () in
  let offset = Random.int (String.length text - len) in
  String.sub text offset len

let[@inline never] sample len search text =
  search (substring len text)

let multisample len search text niter =
  let found = ref 0 in
  let search = sample len search text in
  for _i = 1 to niter do
    match search text with
    | None -> ()
    | Some _ -> incr found
  done;
    !found

let implems = ["naive"; "kmp"]

let parse_implem = function
  | "kmp" -> Search.Kmp.search
  | _ -> Search.Naive.search

let args r = ["-search", Arg.Symbol (implems, (fun s -> r := s)), "implem" ]

let () =
  let r = ref "" in
  Arg.parse (args r) ignore "main -search {kmp,naive}";
  let search = parse_implem !r in
  let niter = 10 in
  let check = multisample (exp 1000.) search Text.data niter in
  assert (check=niter)

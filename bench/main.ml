
let exp scale () = int_of_float (-. scale *. log (Random.float 1.))

let substring len text =
  let len = len () in
  let tlen = String.length text in
  let off0 = tlen/2 in
  let offset = off0 + Random.int (off0 - len) in
  String.sub text offset len

let multisample_shared search ~needle text niter =
  let found = ref 0 in
  let search = search needle in
  for _i = 1 to niter do
    match search text with
    | None -> ()
    | Some _ -> incr found
  done;
    !found

let multisample search needles text niter =
  let found = ref 0 in
  for i = 0 to niter-1 do
    let needle = needles.(i) in
    match  search needle text with
    | None -> ()
    | Some _ -> incr found
  done;
    !found

let implems = ["naive"; "kmp"; "kmp_online"]

let parse_implem = function
  | "kmp" -> Search.Kmp.search
  | "kmp_online" -> Search.Kmp_online.search
  | _ -> Search.Naive.search

let args ~shared ~niter ~size ~implem = [
  "-search", Arg.Symbol (implems, (:=) implem), "implem";
  "-needle-size", Arg.Float ((:=) size), "needle size";
  "-n-iteration", Arg.Int ((:=) niter), "number of iteration";
  "-shared", Arg.Bool ((:=) shared), "share needle compilation"
]

let human_text size =
  substring (exp size) Text.data


let () =
  let implem = ref ""
  and shared = ref false
  and niter = ref 100
  and size = ref 1000. in
  Arg.parse (args ~implem ~shared ~niter ~size)
    ignore "main -search {kmp,naive}";
  let search = parse_implem !implem in
  let niter = !niter in
  let check =
   if !shared then
      let needle = human_text !size in
      multisample_shared search ~needle Text.data niter
   else
      let needles = Array.init niter (fun _ -> human_text !size) in
      multisample search needles Text.data niter
  in
  assert (check=niter)

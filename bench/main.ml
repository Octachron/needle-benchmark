
let _exp scale () = int_of_float (-. scale *. log (Random.float 1.))

let substring len text =
  let tlen = String.length text in
  let free_len = tlen - len in
  if free_len < 0 then failwith "Too large human text size" else
  let off0 = free_len/2 in
  let offset = off0 + Random.int off0 in
  String.sub text offset len

let multisample_shared f ~haystack:text niter =
  for _i = 1 to niter do f text done

let multisample f ~needle ~haystack niter =
  for _i = 0 to niter-1 do f needle haystack done


type mode =
  | Replace_first
  | Replace_all
  | Search_first

type bench_kind =
  | Worst
  | Random_large
  | Random_small
  | Human


let implems: (string * (module Search.Api.Bench)) list = [
  "naive", (module Search.Naive2);
  "kmp", (module Search.Kmp);
  "kmp_online", (module Search.Kmp_online);
   "two-way", (module Search.Two_way)
]

let modes = [
  "search", Search_first;
  "replace-first", Replace_first;
  "replace-all", Replace_all
]

let data_kinds = [
  "worst", Worst;
  "human", Human;
  "random", Random_large;
  "random-small", Random_small
]


let symbol r l =
   Arg.Symbol (List.map fst l, fun s -> r := List.assoc s l)

let implem = ref (module Search.Naive2: Search.Api.Bench)
and shared = ref false
and niter = ref 1000
and size = ref 10
and mode = ref Search_first
and data = ref Human
and haystack_size = ref (4 * 1024)
and seed = ref 45

let args = [
  "-search", symbol implem implems, "implem";
  "-data", symbol data data_kinds, "benchmark data kind";
  "-mode", symbol mode modes, "function to benchmark";
  "-needle-size", Arg.Int ((:=) size), "needle size";
  "-haystack-size", Arg.Int ((:=) haystack_size), "haystack size";
  "-n-iteration", Arg.Int ((:=) niter), "number of iteration";
  "-shared", Arg.Bool ((:=) shared), "share needle compilation";
  "-seed", Arg.Int ((:=) seed), "random seed"
]

let _human_text size =
  substring size Text.data

(* Benchmarking *)




let random_string n = String.init n (fun _ -> Char.chr (0x20 + Random.int (0x7E-0x20)))
let random_small_string alphasize n =
  String.init n (fun _ -> Char.chr (0x61 + Random.int alphasize))


(* This code is Copyright (c) 2025 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC *)
let worst_ab_string n = (* a^(n-1)b *)
  String.init n (fun i -> if i = n - 1 then 'b' else 'a')

let bench_needle needle_size data_kind () = match data_kind with
  | Worst -> worst_ab_string needle_size
  | Random_large -> random_string needle_size
  | Random_small -> random_small_string 2 needle_size
  | Human -> substring needle_size Text.data

let bench_haystack haystack_size data_kind () = match data_kind with
  | Worst ->  Stdlib.String.make haystack_size 'a'
  | Random_large -> random_string haystack_size
  | Random_small -> random_small_string 2 haystack_size
  | Human -> substring haystack_size Text.data


let compile (module String:Search.Api.Bench) mode needle =
  let ignored f h = Sys.opaque_identity (ignore (f h)) in
  let f =
  match mode with
  | Replace_first -> ignored @@ String.replace_first ~sub:needle ~by:""
  | Search_first -> ignored @@ String.find_sub ?start:None ~sub:needle
  | Replace_all -> ignored @@ String.replace_all ~sub:needle ~by:""
  in
  fun h -> ignore (f h)



let () =
  Arg.parse args ignore "main -search {kmp,naive}";
  Random.init !seed;
  let niter = !niter in
  let needle = bench_needle !size !data () in
  Format.eprintf "needle=%S@." needle;
  let haystack = bench_haystack !haystack_size !data () in
  if !shared then
    let f = compile !implem !mode needle in
    multisample_shared f ~haystack niter
  else
    multisample (compile !implem !mode)
      ~needle
      ~haystack
      niter

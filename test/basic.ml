let implementations =  [
  "naive", Search.Naive.search;
  "kmp", Search.Kmp.search;
  "kmp_online", Search.Kmp_online.search;
(*  "kmp_compressed", Search.Kmp_compressed.search; *)
]

let pp ppf = function
  | None -> Format.fprintf ppf "Not found"
  | Some x -> Format.fprintf ppf "%d" x

let test (name,i) number (needle, s, expected) =
  match let r = i needle s in r = expected, r with
  | true, _ -> Format.printf "%d. %s:[ok]@." number name
  | false, r ->
    Format.printf "%d. %s:[ERROR] %a/%a@."
      number name
      pp r
      pp expected
  | exception e -> Format.printf "%d. %s:[CRASH]@." number name; raise e

let tests = [
  "a", "aa", Some 0;
  "b", "aaa", None;
  "a", "bbba", Some 3;
  "abc", "aaabc", Some 2;
  "aa", "abaa", Some 2;
  "aaba", "aaaaba", Some 2;
  "abaa", "ababaae", Some 2;
  "abcabcd", "abcabcabcdee", Some 3;
  "aababcabcd", "aadabceabcdeabcaabababcfaababcabcdabcdeabcdef", Some 24;
]


let () =
  Format.printf "@.";
  List.iter (fun i ->
      List.iteri (test i) tests
    ) implementations;

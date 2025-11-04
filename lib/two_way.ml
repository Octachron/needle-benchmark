(*---------------------------------------------------------------------------
   Copyright (c) 2025 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {1:design_notes Design notes}

    {ul
    {- We implement string search with the
       {{:https://doi.org/10.1145/116825.116845}two way algorithm}.  It
       has a good worse case complexity and the result of
       pre-preprocessing is only two integers (versus allocating tables
       for Knuth-Morris-Pratt or Boyer-Moore).}
    {- We raise on invalid position, we could also return [None]
       or make the functions behave like the identity (if applicable). But
       this is more in the spirit of the current [String] module and it's
       also unclear which one is best.}
    {- In `rfind_sub`, the [start] argument has a different semantics from
       the argument [r*_from] functions. The advantage of the definition
       used below is that it coincides with the mental model of indices
       which is the one you want to work with.}}
    {- The [start] argument remains however a position (vs index)
       so that definitions on empty arguments are consistent and well
       defined without fuss.}} *)

(** String search and replace functions. *)

(** Implement API with two way string search *)
  (* See https://doi.org/10.1145/116825.116845 or
     http://www-igm.univ-mlv.fr/~lecroq/string/node26.html#SECTION00260 *)

open String


let invalid_start ~start len =
  let i = Int.to_string in
  invalid_arg @@ concat "" ["start: "; i start; "not in range [0;"; i len; "]"]

let find_maximal_suffix_and_period ~sub =
  let sublen = length sub in
  let i = ref (-1) and j = ref 0 and k = ref 1 and p = ref 1 in
  let[@inline] maximal_suffix ~order =
    while (!j + !k < sublen) do
      let c = order * Char.compare (get sub (!j + !k)) (get sub (!i + !k)) in
      if c < 0 then (j := !j + !k; k := 1; p := !j - !i) else
      if c > 0 then (i := !j; j := !i + 1; k := 1; p := 1) else (* c = 0 *)
      if !k = !p then (j := !j + !p; k := 1) else incr k
    done;
  in
  (maximal_suffix[@inlined]) ~order:1;
  let l0 = !i and p0 = !p in
  i := -1; j := 0; k := 1; p := 1;
  (maximal_suffix[@inlined]) ~order:(-1);
  let l1 = !i and p1 = !p in
  if l0 > l1 then (l0, p0) else (l1, p1)

let periodic_sub ~sub ~sub_lp:(l, p) =
  let i = ref 0 in
  while !i <= l && Char.equal (get sub !i) (get sub (!i + p))
  do incr i done;
  !i > l

let primitive_find_sub ~start ~sub ~sub_lp:(l, p as sub_lp) s =
  let slen = length s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
    let sublen = length sub in
    let smax = slen - sublen in
    let j = ref start in
    try
      if periodic_sub ~sub ~sub_lp then begin
        let memory = ref (-1) in
        while (!j <= smax) do
          let i = ref (1 + Int.max l !memory) in
          while (!i < sublen && Char.equal (get sub !i) (get s (!i + !j)))
          do incr i done;
          if !i < sublen then (j := !j + (!i - l); memory := -1) else
            begin
              i := l;
              while (!i > !memory && Char.equal (get sub !i) (get s (!i + !j)))
              do decr i done;
              if !i <= !memory then raise_notrace Exit else
                (j := !j + p; memory := sublen - p - 1)
            end
        done;
        -1
      end else begin
        let p = 1 + Int.max (l + 1) (sublen - l - 1) in
        while (!j <= smax) do
          let i = ref (l + 1) in
          while (!i < sublen && Char.equal (get sub !i) (get s (!i + !j)))
          do incr i done;
          if !i < sublen then (j := !j + (!i - l)) else
            begin
              i := l;
              while (!i >= 0 && Char.equal (get sub !i) (get s (!i + !j)))
              do decr i done;
              if !i < 0 then raise_notrace Exit else (j := !j + p)
            end
        done;
        -1
      end
    with Exit -> !j

let primitive_rfind_sub ~start ~sub ~sub_lp:(l, p as sub_lp) s =
  (* Note this is the same as above except for the assignement
     and test logic on [j] where we move from right to left. *)
  let slen = length s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
    let sublen = length sub in
    let smax = slen - sublen in
    let j = ref (if start > smax then smax else start) in
    try
      if periodic_sub ~sub ~sub_lp then begin
        let memory = ref (-1) in
        while (!j >= 0) do
          let i = ref (1 + Int.max l !memory) in
          while (!i < sublen && Char.equal (get sub !i) (get s (!i + !j)))
          do incr i done;
          if !i < sublen then (j := !j - (!i - l); memory := -1) else
            begin
              i := l;
              while (!i > !memory && Char.equal (get sub !i) (get s (!i + !j)))
              do decr i done;
              if !i <= !memory then raise_notrace Exit else
                (j := !j - p; memory := sublen - p - 1)
            end
        done;
        -1
      end else begin
        let p = 1 + Int.max (l + 1) (sublen - l - 1) in
        while (!j >= 0) do
          let i = ref (l + 1) in
          while (!i < sublen && Char.equal (get sub !i) (get s (!i + !j)))
          do incr i done;
          if !i < sublen then (j := !j - (!i - l)) else
            begin
              i := l;
              while (!i >= 0 && Char.equal (get sub !i) (get s (!i + !j)))
              do decr i done;
              if !i < 0 then raise_notrace Exit else (j := !j - p)
            end
        done;
        -1
      end
    with Exit -> !j

let includes ~affix:sub s =
  let sub_lp = find_maximal_suffix_and_period ~sub in
  primitive_find_sub ~start:0 ~sub ~sub_lp s <> -1

let find_sub ?(start = 0) ~sub s =
  let sub_lp = find_maximal_suffix_and_period ~sub in
  match primitive_find_sub ~start ~sub_lp ~sub s with
  | -1 -> None | i -> Some i

let rfind_sub ?start ~sub s =
  let start = match start with None -> length s | Some s -> s in
  let sub_lp = find_maximal_suffix_and_period ~sub in
  match primitive_rfind_sub ~start ~sub_lp ~sub s with
  | -1 -> None | i -> Some i

let find_all_sub ?(start = 0) f ~sub s acc =
  let rec loop f acc sub sub_lp s ~start ~slen =
    if start > slen then acc else
      match primitive_find_sub ~start ~sub ~sub_lp s with
      | -1 -> acc
      | i ->
        let acc = f i acc in
        let start = i + length sub in
        let start = if start = i then start + 1 else start in
        loop f acc sub sub_lp s ~start ~slen
  in
  let slen = length s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
    let sub_lp = find_maximal_suffix_and_period ~sub in
    loop f acc sub sub_lp s ~start ~slen

let rfind_all_sub ?start f ~sub s acc =
  let rec loop f acc sub sub_lp s ~start ~slen =
    if start < 0 then acc else
      match primitive_rfind_sub ~start ~sub ~sub_lp s with
      | -1 -> acc
      | i ->
        let start = i - Int.max (length sub) 1 in
        loop f (f i acc) sub sub_lp s ~start ~slen
  in
  let slen = length s in
  let start = match start with None -> length s | Some s -> s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
    let sub_lp = find_maximal_suffix_and_period ~sub in
    loop f acc sub sub_lp s ~start ~slen

let replace_first ?(start = 0) ~sub:needle ~by s =
  let sub_lp = find_maximal_suffix_and_period ~sub:needle in
  match primitive_find_sub ~start ~sub:needle ~sub_lp s with
  | -1 -> s
  | i ->
    let rest_first = i + length needle in
    let rest_len = length s - i - length needle in
    concat by [sub s 0 i; sub s rest_first rest_len]

let replace_all ?start ~sub:needle ~by s =
  let chunk_first = ref 0 in
  let add_chunk i acc =
    let acc = sub s !chunk_first (i - !chunk_first) :: acc in
    chunk_first := i + length needle; acc
  in
  match find_all_sub ?start add_chunk ~sub:needle s [] with
  | [] -> s
  | chunks ->
    let chunks = sub s !chunk_first (length s - !chunk_first) :: chunks in
    concat by (List.rev chunks)

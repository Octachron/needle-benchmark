(*---------------------------------------------------------------------------
   Copyright (c) 2025 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open String


let invalid_start ~start len =
  let i = Int.to_string in
  invalid_arg @@ concat "" ["start: "; i start; "not in range [0;"; i len; "]"]

let is_sub ~sublen ~sub s j =
  let i = ref 0 in
  while !i < sublen && Char.equal (get s (j + !i)) (get sub !i)
  do incr i done;
  !i = sublen

let primitive_find_sub ~start ~sub s =
  let slen = length s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
    let sublen = length sub in
    let smax = slen - sublen in
    let j = ref start in
    while !j <= smax && not (is_sub ~sublen ~sub s !j) do incr j done;
    if !j <= smax then !j else -1

let primitive_rfind_sub ~start ~sub s =
  let slen = length s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
    let sublen = length sub in
    let smax = length s - sublen in
    let j = ref (if start > smax then smax else start) in
    while !j >= 0 && not (is_sub ~sublen ~sub s !j) do decr j done;
    if !j >= 0 then !j else -1

let includes ~affix s = primitive_find_sub ~start:0 ~sub:affix s <> -1

let find_sub ?(start = 0) ~sub s =
  match primitive_find_sub ~start ~sub s with -1 -> None | i -> Some i

let rfind_sub ?start ~sub s =
  let start = match start with None -> length s | Some s -> s in
  match primitive_rfind_sub ~start ~sub s with -1 -> None | i -> Some i

let find_all_sub ?(start = 0) f ~sub s acc =
  let rec loop f acc sub s ~start ~slen =
    if start > slen then acc else
      match primitive_find_sub ~start ~sub s with
      | -1 -> acc
      | i ->
        let acc = f i acc in
        let start = i + Int.max (length sub) 1 in
        loop f acc sub s ~start ~slen
  in
  let slen = length s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
    loop f acc sub s ~start ~slen

let rfind_all_sub ?start f ~sub s acc =
  let rec loop f acc sub s ~start ~slen =
    if start < 0 then acc else
      match primitive_rfind_sub ~start ~sub s with
      | -1 -> acc
      | i ->
        let start = i - Int.max (length sub) 1 in
        loop f (f i acc) sub s ~start ~slen
  in
  let slen = length s in
  let start = match start with None -> length s | Some s -> s in
  if not (0 <= start && start <= slen) then invalid_start ~start slen else
    loop f acc sub s ~start ~slen

let replace_first ?(start = 0) ~sub:needle ~by s =
  match primitive_find_sub ~start ~sub:needle s with
  | -1 -> s
  | i ->
    let rest_first = i + length needle in
    let rest_len = length s - i - length needle in
    concat by [sub s 0 i; sub s rest_first rest_len]

let replace_all ?start ~sub:needle ~by s =
  let chunk_first = ref 0 in
  let add_chunk i acc =
    let last_chunk = sub s !chunk_first (i - !chunk_first) in
    chunk_first := i + length needle; last_chunk :: acc
  in
  match find_all_sub ?start add_chunk ~sub:needle s [] with
  | [] -> s
  | chunks ->
    let last_chunk = sub s !chunk_first (length s - !chunk_first) in
    concat by (List.rev (last_chunk :: chunks))

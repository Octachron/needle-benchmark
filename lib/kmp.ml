
let compute_backedges needle =
  let nlen = String.length needle in
  (* edges.(i) = k means that if we fail to accept a new character at position
     i, we should retry at position k *)
  let edges = Array.make nlen 0 in
  edges.(0) <- -1;
  let rec fill ~back_pos ~new_pos =
    if new_pos >= nlen then ()
    else
      let new_char = needle.[new_pos] in
      if new_char = needle.[back_pos] then begin
        edges.(new_pos) <- edges.(back_pos);
        fill ~back_pos:(back_pos+1) ~new_pos:(new_pos+1)
      end
      else begin
        edges.(new_pos) <- back_pos;
        let back_pos = go_back new_char edges.(back_pos) in
        fill ~back_pos ~new_pos:(new_pos+1)
      end
  and go_back new_char back_pos =
    if back_pos <= 0 then 0
    else if needle.[back_pos] = new_char then
      back_pos
    else
      go_back new_char edges.(back_pos)
  in
  fill ~back_pos:0 ~new_pos:1;
  edges

let execute_search edges ~needle s =
  let nlen = String.length needle in
  let slen = String.length s in
  let rec search edges needle nlen slen ~npos pos s =
    if npos >= nlen then Some (pos-nlen)
    else if pos >= slen then None
    else if needle.[npos] = s.[pos] then
      search edges needle nlen slen ~npos:(1+npos) (1+pos) s
    else if npos = 0 then
      search edges needle nlen slen ~npos:0 (1+pos) s
    else
      let npos = edges.(npos) in
      if npos < 0 then
        search edges needle nlen slen ~npos:0 (1+pos) s
      else
        search edges needle nlen slen ~npos pos s
  in
  search edges needle nlen slen ~npos:0 0 s


let search needle =
  if String.length needle = 0 then fun _ -> Some 0
  else
    let edges = compute_backedges needle in
    fun s -> execute_search ~needle edges s


let find_sub ?start:_ ~sub =
  search sub

let replace_first ?start ~sub:needle ~by s =
  match find_sub ~start ~sub:needle s with
  | None -> s
  | Some i ->
    let rest_first = i + String.length needle in
    let rest_len = String.length s - i - String.length needle in
    String.concat by String.[sub s 0 i; sub s rest_first rest_len]

let replace_all ?start:_ ~sub:_ ~by:_ _ = failwith "Not implemented"

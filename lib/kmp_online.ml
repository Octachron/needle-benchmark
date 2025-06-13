type backedge_state =
  | On_going of {
      last_pos:int;
      back_pos:int;
      edges: int array;
    }
  | Done of int array

let init_backedges needle =
    let edges = Array.make (String.length needle) 0 in
    edges.(0) <- -1;
    On_going { last_pos = 0; back_pos = 0; edges }

let rec go_back needle edges new_char back_pos =
    if back_pos <= 0 then 0
    else if needle.[back_pos] = new_char then
      back_pos
    else
      go_back needle edges new_char edges.(back_pos)

let execute_update_backedges edges back_pos needle new_pos =
  let new_char = needle.[new_pos] in
  if new_char = needle.[back_pos] then begin
    edges.(new_pos) <- edges.(back_pos);
    On_going { edges; back_pos=back_pos+1; last_pos = new_pos }
  end
  else begin
    edges.(new_pos) <- back_pos;
    let back_pos = go_back needle edges new_char edges.(back_pos) in
    On_going { edges; back_pos; last_pos = new_pos}
  end

let update_backedges state needle new_pos = match state with
  | Done _ -> state
  | On_going {last_pos;back_pos;edges} ->
    if new_pos > last_pos then
      execute_update_backedges edges back_pos needle new_pos
    else state

let backedge state pos = match state with
  | Done e -> e.(pos)
  | On_going r -> r.edges.(pos)

let search needle s =
  let nlen = String.length needle in
  let slen = String.length s in
  let rec search edges ~npos pos =
    if npos >= nlen then Some (pos-nlen)
    else if pos >= slen then None
    else
      let edges = update_backedges edges needle npos in
      if needle.[npos] = s.[pos] then
        search edges ~npos:(1+npos) (1+pos)
      else if npos = 0 then
        search edges ~npos:0 (1+pos)
      else
        let npos = backedge edges npos in
        if npos < 0 then
          search edges ~npos:0 (1+pos)
        else
          search edges ~npos pos
  in
  search (init_backedges needle) ~npos:0 0

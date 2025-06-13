let search needle s =
  let nlen = String.length needle in
  let slen = String.length s in
  let rec search ~npos ~needle ~spos ~s =
    if npos >= nlen then Some (spos-nlen)
    else if spos >= slen then None
    else if s.[spos] = needle.[npos] then
      search ~npos:(npos+1) ~needle ~spos:(spos + 1) ~s
    else
      search ~npos:0 ~needle ~spos:(spos - npos + 1 ) ~s
  in
  search ~npos:0 ~needle ~spos:0 ~s

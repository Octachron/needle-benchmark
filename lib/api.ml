module type T = sig
  val includes : affix:string -> string -> bool
  (** [includes ~affix s] is [true] if and only if [s] has [affix] as
      a substring.

      @since X.XX *)

  val find_sub : ?start:int -> sub:string -> string -> int option
  (** [find_sub ~start ~sub s] is the start position (if any) of the
      first occurence of [sub] in [s] after or at position [start]
      (which includes index [start] if it exists, defaults to [0]).

      Note if you need to search for [sub] multiple times in [s] use
      {!find_all_sub} it is more efficient.

      @raise Invalid_argument if [start] is not a valid position of [s].

      @since X.XX *)

  val rfind_sub : ?start:int -> sub:string -> string -> int option
  (** [rfind_sub ~start ~sub s] is the start position (if any) of the
      first occurences of [sub] in [s] before or at position [start]
      (which includes index [start] if it exists, defaults to
      [String.length s]).

      Note if you need to search for [sub] multiple times in [s] use
      {!rfind_all_sub} it is more efficient.

      @raise Invalid_argument if [start] is not a valid position of [s].

      @since X.XX *)

  val find_all_sub :
    ?start:int -> (int -> 'acc -> 'acc) -> sub:string -> string -> 'acc -> 'acc
  (** [find_all_sub ~start f ~sub s acc], starting with [acc], folds [f] over
      all non-overlapping starting positions of [sub] in [s] after or at
      position [start] (which includes index [start] if it exists, defaults
      to [0]). This is [acc] if [sub] could not be found in [s].

      @raise Invalid_argument if [start] is not a valid position of [s].

      @since X.XX *)

  val rfind_all_sub :
    ?start:int -> (int -> 'acc -> 'acc) -> sub:string -> string -> 'acc -> 'acc
  (** [rfind_all_sub ~start f ~sub s acc], starting with [acc], folds
      [f] over all non-overlapping starting positions of [sub] in [s]
      before or at position [start] (which includes index [start] if
      it exists, defaults to [String.length s]). This is [acc] if
      [sub] could not be found in [s].

      @raise Invalid_argument if [start] is not a valid position of [s].

      @since X.XX *)

  val replace_first : ?start:int -> sub:string -> by:string -> string -> string
  (** [replace_first ~start ~sub ~by s] replaces in [s] the first occurence
      of [sub] at or after position [start] (defaults to [0]) by [by].

      @raise Invalid_argument if [start] is not a valid position of [s].

      @since X.XX *)

  val replace_all : ?start:int -> sub:string -> by:string -> string -> string
  (** [replace_all ~start ~sub ~by] replaces in [s] all
      non-overlapping occurences of [sub] at or after position [start]
      (default to [0]) by [by].

      @raise Invalid_argument if [start] is not a valid position of [s].

      @since X.XX *)
end

module type Bench= sig
  val find_sub : ?start:int -> sub:string -> string -> int option
  (** [find_sub ~start ~sub s] is the start position (if any) of the
      first occurence of [sub] in [s] after or at position [start]
      (which includes index [start] if it exists, defaults to [0]).

      Note if you need to search for [sub] multiple times in [s] use
      {!find_all_sub} it is more efficient.

      @raise Invalid_argument if [start] is not a valid position of [s].

      @since X.XX *)

    val replace_first : ?start:int -> sub:string -> by:string -> string -> string
  (** [replace_first ~start ~sub ~by s] replaces in [s] the first occurence
      of [sub] at or after position [start] (defaults to [0]) by [by].

      @raise Invalid_argument if [start] is not a valid position of [s].

      @since X.XX *)

  val replace_all : ?start:int -> sub:string -> by:string -> string -> string
  (** [replace_all ~start ~sub ~by] replaces in [s] all
      non-overlapping occurences of [sub] at or after position [start]
      (default to [0]) by [by].
  *)

end

(* https://en.wikipedia.org/wiki/Consistent_Overhead_Byte_Stuffing *)

let default_delim = '\000'

(** [to_cobs ?delim bytes] is a [bytes] containing a COBS-encoded representation
    of the given [bytes], using the provided [delim]iter (defaults to ['\000']).

    The encoded representation will not end with a delimiter byte; if it is necessary,
    you must append it to the output of [to_cobs].
*)
let to_cobs ?(delim : char option) (b : bytes) =
  let delim = Option.value ~default:default_delim delim in
  let b_len = Bytes.length b in
  (* COBS requires a minimum of 1 byte overhead, and a maximum of ceil(n/254) bytes for n data bytes. *)
  let out = Bytes.create (b_len + ((float_of_int b_len) /. 254.0 |> ceil |> int_of_float)) in
  let rec encode_group_at b_start out_pos =
    (* To encode some bytes, first append a zero byte, then break them into groups of
       either 254 non-zero bytes, or 0–253 non-zero bytes followed by a zero byte.
       Because of the appended zero byte, this is always possible. *)
    let v = Bytes.index_from_opt b b_start delim in (* index of the next delim, if any *)
    (* the amount to skip forward for the next run depends on if there's a delim byte to skip! *)
    let next_delim = (Option.value ~default:b_len v) in (* index of the next delim! (pretending we appended one) *)

    (* write the number of nonzero bytes, then the actual data *)
    (* if next_delim is >254 characters away, we're doing a 254-len non-zero group *)
    (* else we are doing a group followed by a zero byte, so we actually advance one more than the end *)
    if next_delim >= b_start + 254 then (
      Bytes.set out out_pos '\255'; (* number of non-zero bytes plus one *)
      let out_pos = out_pos + 1 in
      Bytes.blit b b_start out out_pos 254; (* actual data *)
      let out_pos = out_pos + 254 in
      let b_start = b_start + 254 in
      if b_start = b_len then
        Bytes.sub out 0 out_pos
      else
        encode_group_at b_start out_pos
    ) else (
      let n_nonzero = next_delim - b_start in
      Bytes.set out out_pos @@ char_of_int (n_nonzero + 1);
      let out_pos = out_pos + 1 in
      Bytes.blit b b_start out out_pos n_nonzero;
      let out_pos = out_pos + n_nonzero in
      let b_start = b_start + n_nonzero + 1 in
      (* we're at the last group (the fake appended delim) if b_start = b_len
         (because there isn't actually any data left for us, and we just did that
         appended delim) *)
      if b_start <= b_len then
        encode_group_at b_start out_pos
      else Bytes.sub out 0 out_pos
    )
  in
  encode_group_at 0 0



(** [from_cobs bytes] is a [bytes] containing the result of COBS-decoding
    the given [bytes], using the provided [delim]iter (defaults to ['\000']).

    [bytes] is expected to not end with the delimiter byte; if present, it
    should be stripped before passing the data to [from_cobs].

    @raise Invalid_argument if [b] does not contain valid COBS-encoded data
*)
let from_cobs ?(delim : char option) (b : bytes) =
  let delim = Option.value ~default:default_delim delim in
  let b_len = Bytes.length b in
  let out = Bytes.create b_len in
  let rec decode_group_at b_start out_pos =
    let header = Bytes.get b b_start |> int_of_char in
    let b_start = b_start + 1 in
    let len = header - 1 in
    Bytes.blit b b_start out out_pos len;
    let b_start = b_start + len in
    let out_pos = out_pos + len in
    if b_start >= b_len then (
      Bytes.sub out 0 out_pos
    ) else if header != 255 then (
      Bytes.set out out_pos delim;
      let out_pos = out_pos + 1 in
      decode_group_at b_start out_pos
    ) else (
      decode_group_at b_start out_pos
    )
  in
  decode_group_at 0 0
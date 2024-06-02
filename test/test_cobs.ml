open OUnit2

let printer_bytes b = b |> String.of_bytes |> String.escaped

let encode_assert_equal ?(delim : char option) (input : string) (expected : string) =
  assert_equal ~printer:String.escaped expected (input |> Bytes.of_string |> Cobs.to_cobs ?delim |> String.of_bytes)

let decode_assert_equal ?(delim : char option) (input : string) (expected : string) =
  assert_equal ~printer:String.escaped expected (input |> Bytes.of_string |> Cobs.from_cobs ?delim |> String.of_bytes)

let bytes_encode_assert_equal (input : bytes) (expected : bytes) =
  assert_equal ~printer:printer_bytes expected (Cobs.to_cobs input)

let bytes_decode_assert_equal (input : bytes) (expected : bytes) =
  assert_equal ~printer:printer_bytes expected (Cobs.from_cobs input)

let make_test_pair ?(delim : char option) (data : string) (encoded : string) (label : string) = (
  label >:: (fun _ -> encode_assert_equal ?delim data encoded),
  label >:: (fun _ -> decode_assert_equal ?delim encoded data)
)

let bytes_make_test_pair (data : bytes) (encoded : bytes) (label : string) = (
  label >:: (fun _ -> bytes_encode_assert_equal data encoded),
  label >:: (fun _ -> bytes_decode_assert_equal encoded data)
)

let tests_list = [
  "00" |> make_test_pair "\000" "\001\001";
  "00 00" |> make_test_pair "\000\000" "\001\001\001";
  "00 11 00" |> make_test_pair "\000\011\000" "\001\002\011\001";
  "11 22 00 33" |> make_test_pair "\011\022\000\033" "\003\011\022\002\033";
  "11 22 33 44" |> make_test_pair "\011\022\033\044" "\005\011\022\033\044";
  "11 22 33 44 (delim 22)" |> make_test_pair ~delim:'\022' "\011\022\033\044" "\002\011\003\033\044";
  "11 00 00 00" |> make_test_pair "\011\000\000\000" "\002\011\001\001\001";
  "01 02 03 ... FD FE" |> (
    let input = Bytes.init 254 (fun i -> char_of_int (i + 1)) in
    let expected = Bytes.extend input 1 0 in
    Bytes.set expected 0 '\255';
    bytes_make_test_pair input expected);
  "00 01 02 ... FC FD FE" |> (
    let input = Bytes.init 255 char_of_int in
    let expected = Bytes.concat (Bytes.create 0) [
      Bytes.of_string "\001\255";
      Bytes.init 254 (fun i -> char_of_int (i + 1))
    ] in
    bytes_make_test_pair input expected);
  "01 02 03 ... FD FE FF" |> (
    let input = Bytes.init 255 (fun i -> char_of_int (i + 1)) in
    let expected = Bytes.concat (Bytes.create 0) [
      Bytes.of_string "\255";
      Bytes.init 254 (fun i -> char_of_int (i + 1));
      Bytes.of_string "\002\255"
    ] in
    bytes_make_test_pair input expected);
  "02 03 04 ... FE FF 00" |> (
    let input = Bytes.init 255 (fun i -> char_of_int ((i + 2) mod 256)) in
    let expected = Bytes.concat (Bytes.create 0) [
      Bytes.of_string "\255";
      Bytes.init 254 (fun i -> char_of_int ((i + 2) mod 256));
      Bytes.of_string "\001\001"
    ] in
    bytes_make_test_pair input expected);
  "03 04 05 ... FF 00 01" |> (
    let input = Bytes.init 255 (fun i -> char_of_int ((i + 3) mod 256)) in
    let expected = Bytes.concat (Bytes.create 0) [
      Bytes.of_string "\254";
      Bytes.init 253 (fun i -> char_of_int ((i + 3) mod 256));
      Bytes.of_string "\002\001"
    ] in
    bytes_make_test_pair input expected);
]

let (encode_tests_list, decode_tests_list) = List.split tests_list

let tests = "test suite for ocaml-cobs" >::: [
  "test suite for cobs encoder" >::: encode_tests_list;
  "test suite for cobs decoder" >::: decode_tests_list;
  "test suite for cobs encoder/decoder (misc.)" >::: [
    "can encode/decode large amounts of random uninitialized data" >:: fun _ -> (
      let input = Bytes.create 1000000 in
      assert_equal ~printer:printer_bytes input (input |> Cobs.to_cobs |> Cobs.from_cobs)
    )
  ];
]

let _ = run_test_tt_main tests;
# `ocaml-cobs`

This OCaml library implements a Consistent Overhead Byte Stuffing (COBS) encoder and decoder. COBS is a useful encoding for framing packets in binary streams; it transforms a sequence of bytes to not contain a given delimiter (typically `0x00`), allowing you to use that delimiter to separate packets in your stream. It has a consistently low overhead which makes it good for real-time applications.

For more information about the COBS algorithm, see [this Wikipedia article](https://en.wikipedia.org/wiki/Consistent_Overhead_Byte_Stuffing).
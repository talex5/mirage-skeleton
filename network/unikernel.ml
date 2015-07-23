open Lwt

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

let (>>!=) x f =
  x >>= function
  | `Ok y -> f y
  | `Eof -> failwith "EOF"
  | `Error _ -> failwith "Error"

module RingBuffer : sig
  val record : string -> unit
end = struct
  let trace_buffer = Queue.create ()

  let record s =
    Queue.push s trace_buffer;
    if Queue.length trace_buffer > 1000 then ignore (Queue.pop trace_buffer)

  let () =
    at_exit (fun () ->
      print_endline "Writing trace.pcap...";
      let ch = open_out "trace.pcap" in
      let header_buf = Cstruct.create Pcap.sizeof_pcap_header in
      Pcap.LE.set_pcap_header_magic_number header_buf Pcap.magic_number;
      Pcap.LE.set_pcap_header_network header_buf Pcap.Network.(to_int32 Ethernet);
      Pcap.LE.set_pcap_header_sigfigs header_buf 0l;
      Pcap.LE.set_pcap_header_snaplen header_buf 0xffffl;
      Pcap.LE.set_pcap_header_thiszone header_buf 0l;
      Pcap.LE.set_pcap_header_version_major header_buf Pcap.major_version;
      Pcap.LE.set_pcap_header_version_minor header_buf Pcap.minor_version;
      output_string ch (Cstruct.to_string header_buf);
      Queue.iter (output_string ch) trace_buffer;
      print_endline "Saved."
    )
end

let pcap_record buffer =
  let pcap_buf = Cstruct.create Pcap.sizeof_pcap_packet in
  let time = Unix.gettimeofday () in
  Pcap.LE.set_pcap_packet_incl_len pcap_buf (Int32.of_int (String.length buffer));
  Pcap.LE.set_pcap_packet_orig_len pcap_buf (Int32.of_int (String.length buffer));
  Pcap.LE.set_pcap_packet_ts_sec pcap_buf (Int32.of_float time);
  let frac = (time -. (float_of_int (truncate time))) *. 1000000.0 in
  Pcap.LE.set_pcap_packet_ts_usec pcap_buf (Int32.of_float frac);
  RingBuffer.record (Cstruct.to_string pcap_buf ^ buffer)

module Main (C: V1_LWT.CONSOLE) (Netif : V1_LWT.NETWORK) = struct
  module Pcap_netif = struct
    include Netif

    let write t buffer =
      pcap_record (Cstruct.to_string buffer);
      write t buffer

    let writev t buffers =
      pcap_record (Cstruct.copyv buffers);
      writev t buffers

    let listen t fn =
      listen t (fun buffer ->
        pcap_record (Cstruct.to_string buffer);
        fn buffer
      )
  end

  module Stackv41_E = Ethif.Make(Pcap_netif)
  module Stackv41_I = Ipv4.Make(Stackv41_E)(Clock)(OS.Time)
  module Stackv41_U = Udp.Make (Stackv41_I)
  module Stackv41_T = Tcp.Flow.Make(Stackv41_I)(OS.Time)(Clock)(Random)
  module S = Tcpip_stack_direct.Make(C)(OS.Time)(Random)(Pcap_netif)(Stackv41_E)(Stackv41_I)(Stackv41_U)(Stackv41_T)

  let stackv41 console interface =
    let config = {
      V1_LWT.name = "stackv41";
      console; interface;
      mode = `IPv4 (Ipaddr.V4.of_string_exn "10.0.0.2", Ipaddr.V4.of_string_exn "255.255.255.0", [Ipaddr.V4.of_string_exn "10.0.0.1"]);
    } in
    Stackv41_E.connect interface >>= function
    | `Error _ -> fail (Failure "Stackv41_E")
    | `Ok ethif ->
    Stackv41_I.connect ethif >>= function
    | `Error _ -> fail (Failure "Stackv41_I")
    | `Ok ipv4 ->
    Stackv41_U.connect ipv4 >>= function
    | `Error _ -> fail (Failure "Stackv41_U")
    | `Ok udpv4 ->
    Stackv41_T.connect ipv4 >>= function
    | `Error _ -> fail (Failure "Stackv41_T")
    | `Ok tcpv4 ->
    S.connect config ethif ipv4 udpv4 tcpv4

  let buffer = Io_page.get 1 |> Io_page.to_cstruct
  let payload = Cstruct.sub buffer 0 1400
  let () =
    Cstruct.blit_from_string (String.make 1400 '!') 0 payload 0 1400

  let start c interface =
    stackv41 c interface >>!= fun s ->
    S.listen_tcpv4 s ~port:8080 (fun flow ->
        let dst, dst_port = S.TCPV4.get_dest flow in
        C.log_s c (green "new tcp connection from %s %d"
                     (Ipaddr.V4.to_string dst) dst_port)
        >>= fun () ->
        let rec aux = function
          | 0 -> return ()
          | n ->
              S.TCPV4.write flow payload >>!= fun () ->
              aux (n - 1) in
        aux 1000 >>= fun () ->
        S.TCPV4.close flow >>= fun () ->
        exit 0
      );

    S.listen s

end

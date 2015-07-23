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

module Main (C: V1_LWT.CONSOLE) (Netif : V1_LWT.NETWORK) = struct
  module Stackv41_E = Ethif.Make(Netif)
  module Stackv41_I = Ipv4.Make(Stackv41_E)(Clock)(OS.Time)
  module Stackv41_U = Udp.Make (Stackv41_I)
  module Stackv41_T = Tcp.Flow.Make(Stackv41_I)(OS.Time)(Clock)(Random)
  module S = Tcpip_stack_direct.Make(C)(OS.Time)(Random)(Netif)(Stackv41_E)(Stackv41_I)(Stackv41_U)(Stackv41_T)

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
        aux 100 >>= fun () ->
        S.TCPV4.close flow
      );

    S.listen s

end

open Mirage_types_lwt
open Lwt.Infix

let src = Logs.Src.create "unikernel" ~doc:"Main unikernel code"
module Log = (val Logs.src_log src : Logs.LOG)

module Main (N: NETWORK) (MClock : Mirage_types.MCLOCK) (Time: TIME) = struct
  module E = Ethif.Make(N)
  module A = Arpv4.Make(E)(MClock)(Time)
  module DC = Dhcp_config

  let of_interest dest net =
    Macaddr.compare dest (N.mac net) = 0 || not (Macaddr.is_unicast dest)

  let input_dhcp clock net config leases buf =
    match Dhcp_wire.pkt_of_buf buf (Cstruct.len buf) with
    | Error e ->
      Log.err (fun f -> f "Can't parse packet: %s" e);
      Lwt.return leases
    | Ok pkt ->
      let open Dhcp_server.Input in
      let now = MClock.elapsed_ns clock |> Duration.to_sec |> float_of_int in
      match input_pkt config leases pkt now with
      | Silence -> Lwt.return leases
      | Update leases ->
        Log.info (fun f -> f "Received packet %s - updated lease database" (Dhcp_wire.pkt_to_string pkt));
        Lwt.return leases
      | Warning w ->
        Log.warn (fun f -> f "%s" w);
        Lwt.return leases
      | Dhcp_server.Input.Error e ->
        Log.err (fun f -> f "%s" e);
        Lwt.return leases
      | Reply (reply, leases) ->
        Log.info (fun f -> f "Received packet %s" (Dhcp_wire.pkt_to_string pkt));
        N.write net (Dhcp_wire.buf_of_pkt reply) >>= fun _ ->
        Log.info (fun f -> f "Sent reply packet %s" (Dhcp_wire.pkt_to_string reply));
        Lwt.return leases

  let start net clock _time =
    (* Get an ARP stack *)
    E.connect net >>= fun e ->
    A.connect e clock >>= fun a ->
    A.add_ip a DC.ip_address >>= fun () ->

    (* Build a dhcp server *)
    let config = Dhcp_server.Config.make
        ~hostname:DC.hostname
        ~default_lease_time:DC.default_lease_time
        ~max_lease_time:DC.max_lease_time
        ~hosts:DC.hosts
        ~addr_tuple:(DC.ip_address, N.mac net)
        ~network:DC.network
        ~range:DC.range
        ~options:DC.options
    in
    let leases = ref (Dhcp_server.Lease.make_db ()) in
    let listener = N.listen net (fun buf ->
        match Ethif_packet.Unmarshal.of_cstruct buf with
        | Result.Error s ->
          Log.err (fun f -> f "Can't parse packet: %s" s);
          Lwt.return_unit
        | Result.Ok (ethif_header, ethif_payload) ->
          if of_interest ethif_header.Ethif_packet.destination net &&
             Dhcp_wire.is_dhcp buf (Cstruct.len buf) then begin
            input_dhcp clock net config !leases buf >>= fun new_leases ->
            leases := new_leases;
            Lwt.return_unit
          end else if ethif_header.Ethif_packet.ethertype = Ethif_wire.ARP then
            A.input a ethif_payload
          else Lwt.return_unit
      ) in
    listener
end

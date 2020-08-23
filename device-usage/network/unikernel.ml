open Lwt.Infix
open Qubes
module Flow = Qubes.RExec.Flow

module Main (S: Mirage_stack.V4) = struct

  let handler ~user:_ cmd flow = Lwt.return 0

  let start s =
    (* Start qrexec agent, GUI agent and QubesDB agent in parallel *)
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port (fun flow ->
        let dst, dst_port = S.TCPV4.dst flow in
        Logs.info (fun f -> f "new tcp connection from IP %s on port %d"
                  (Ipaddr.V4.to_string dst) dst_port);
        S.TCPV4.read flow >>= function
        | Ok `Eof -> Logs.info (fun f -> f "Closing connection!"); Lwt.return_unit
        | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
        | Ok (`Data b) ->
          Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b) (Cstruct.to_string b));
          S.TCPV4.close flow
      );

    let qrexec = RExec.connect ~domid:0 () in
    qrexec >>= fun qrexec ->
    (* OS.Time.sleep_ns (0.0 *. 1e9 |> Int64.of_float) >>= fun () -> *)
    let agent_listener = RExec.listen qrexec handler in
    Logs.info (fun f -> f "ready to listen for new packets");

    S.listen s

end

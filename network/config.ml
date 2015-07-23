open Mirage

let main =
  foreign
    ~libraries:["tcpip"; "mirage-clock-unix"; "tcpip.stack-direct"; "pcap-format"]
    ~packages:["tcpip"]
    "Unikernel.Main" (console @-> network @-> job)

let () =
  register "network" [
    main $ default_console $ tap0
  ]

module Hello (Time : Mirage_time.S) = struct

  let start _time =
    for n = 0 to 256 do
      Logs.info (fun f -> f "hello %d" n);
    done;
    Time.sleep_ns (Duration.of_sec 1)

end

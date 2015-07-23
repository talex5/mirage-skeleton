open Lwt

module Main (C: V1_LWT.CONSOLE) = struct

  let start c =
    for_lwt i = 0 to 4 do
      Printf.printf "Sys.time () = %f\n%!" (Sys.time ());
      C.log c "hello" ;
      if i = 2 then for j = 0 to 100000000 do ignore () done;
      lwt () = OS.Time.sleep 1.0 in
      C.log c "world" ;
      return ()
    done

end

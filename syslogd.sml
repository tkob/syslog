structure Syslogd = struct
  infix >>=
  fun (SOME x) >>= k = k x
    | NONE     >>= k = NONE

  fun stringToAddr hostPort =
        let
          fun split hostPort =
                case String.tokens (fn ch => ch = #":") hostPort of
                     [host, port] => SOME (host, port)
                   | _ => NONE
        in
          split hostPort            >>= (fn (host, port) =>
          NetHostDB.fromString host >>= (fn host' =>
          Int.fromString port       >>= (fn port' =>
          SOME (INetSock.toAddr (host', port')))))
        end

  fun main (name : string, argv : string list) =
        let
          fun boot () = (
            print "starting syslogd\n";
            Syslog.Server.start ("log", valOf (stringToAddr ("0.0.0.0:5140")))
            )
        in
          print "booting\n";
          RunCML.doit (boot, NONE)
        end
end

fun main () =
      OS.Process.exit (Syslogd.main (CommandLine.name (), CommandLine.arguments ()))

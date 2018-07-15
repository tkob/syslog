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
          val inputLine = TextIO.StreamIO.inputLine
          val lines = TextIO.getInstream o TextIO.openString
          val rules = Syslog.Conf.load inputLine (lines "*.* messages")
          fun boot () = (
            print "starting syslogd\n";
            SyslogServer.start ("log", valOf (stringToAddr ("0.0.0.0:5140")), rules))
            handle e => print (exnMessage e ^ "\n")
        in
          print "booting\n";
          RunCML.doit (boot, NONE)
        end
end

fun main () =
      OS.Process.exit (Syslogd.main (CommandLine.name (), CommandLine.arguments ()))

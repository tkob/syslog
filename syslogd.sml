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

  fun outputFileFromPath path =
        let
          open Posix.FileSys
          val mode600 = S.flags [S.irusr, S.iwusr]
        in
          (* open path, create if it does not exist *)
          createf (path, O_WRONLY, O.flags [O.append, O.sync], mode600)
        end

  (* ch : a dispatcher-to-writer channel
   * action : an action
   * return : a writer which takes a message from ch
   *)
  fun writerFactory (ch : Syslog.Message.message Channel.chan, action : SyslogConf.action) : (unit -> unit) =
        case action of
             SyslogConf.File fileName =>
               let
                 val fd = outputFileFromPath fileName
                 fun writer () =
                       Channel.recv ch (fn message =>
                         let
                           val string = Syslog.Message.toString message ^ "\n"
                           val vec = Word8VectorSlice.full (Byte.stringToBytes string)
                           val writtenBytes = Posix.IO.writeVec (fd, vec)
                         in
                           writer ()
                         end)
               in
                 writer
               end
           | SyslogConf.Remote s =>
               (fn () => ()) (* not supported yet *)
           | SyslogConf.RemoteTcp host =>
               (fn () => ()) (* not supported yet *)

  fun main (name : string, argv : string list) =
        let
          val inputLine = TextIO.StreamIO.inputLine
          val lines = TextIO.getInstream o TextIO.openString
          val rules = SyslogConf.load inputLine (lines "*.* messages")
          fun boot () = (
            print "starting syslogd\n";
            SyslogServer.start ("log", valOf (stringToAddr ("0.0.0.0:5140")), rules, writerFactory))
            handle e => print (exnMessage e ^ "\n")
        in
          print "booting\n";
          boot ();
          OS.Process.success
        end
end

fun main () =
      OS.Process.exit (Syslogd.main (CommandLine.name (), CommandLine.arguments ()))

structure Syslog :> sig
  datatype facility = Kern
                    | User
                    | Mail
                    | Daemon
                    | Auth
                    | Syslog
                    | Lpr
                    | New
                    | Uucp
                    | Cron
                    | Authpriv
                    | Ftp
                    | Ntp
                    | LogAudit
                    | LogAlert
                    | Clock
                    | Local0
                    | Local1
                    | Local2
                    | Local3
                    | Local4
                    | Local5
                    | Local6
                    | Local7

  datatype severity = Emerg | Alert | Crit | Err | Warning | Notice | Info | Debug

  type pri = facility * severity

  structure Remote : sig
    val log : INetSock.sock_addr -> pri -> string -> unit
  end

  structure Local : sig
    val log : pri -> string -> unit

    val emerg   : string -> unit
    val alert   : string -> unit
    val crit    : string -> unit
    val err     : string -> unit
    val warning : string -> unit
    val notice  : string -> unit
    val info    : string -> unit
    val debug   : string -> unit
  end

  structure Server : sig
    val start : string * INetSock.sock_addr -> unit
  end

  val stringToAddr : string -> INetSock.sock_addr option
end = struct
  datatype facility = Kern
                    | User
                    | Mail
                    | Daemon
                    | Auth
                    | Syslog
                    | Lpr
                    | New
                    | Uucp
                    | Cron
                    | Authpriv
                    | Ftp
                    | Ntp
                    | LogAudit
                    | LogAlert
                    | Clock
                    | Local0
                    | Local1
                    | Local2
                    | Local3
                    | Local4
                    | Local5
                    | Local6
                    | Local7

  datatype severity = Emerg | Alert | Crit | Err | Warning | Notice | Info | Debug

  type pri = facility * severity

  fun facilityToInt Kern     = 0
    | facilityToInt User     = 1
    | facilityToInt Mail     = 2
    | facilityToInt Daemon   = 3
    | facilityToInt Auth     = 4
    | facilityToInt Syslog   = 5
    | facilityToInt Lpr      = 6
    | facilityToInt New      = 7
    | facilityToInt Uucp     = 8
    | facilityToInt Cron     = 9
    | facilityToInt Authpriv = 10
    | facilityToInt Ftp      = 11
    | facilityToInt Ntp      = 12
    | facilityToInt LogAudit = 13
    | facilityToInt LogAlert = 14
    | facilityToInt Clock    = 15
    | facilityToInt Local0   = 16
    | facilityToInt Local1   = 17
    | facilityToInt Local2   = 18
    | facilityToInt Local3   = 19
    | facilityToInt Local4   = 20
    | facilityToInt Local5   = 21
    | facilityToInt Local6   = 22
    | facilityToInt Local7   = 23

  fun intToFacility 0  = SOME Kern
    | intToFacility 1  = SOME User
    | intToFacility 2  = SOME Mail
    | intToFacility 3  = SOME Daemon
    | intToFacility 4  = SOME Auth
    | intToFacility 5  = SOME Syslog
    | intToFacility 6  = SOME Lpr
    | intToFacility 7  = SOME New
    | intToFacility 8  = SOME Uucp
    | intToFacility 9  = SOME Cron
    | intToFacility 10 = SOME Authpriv
    | intToFacility 11 = SOME Ftp
    | intToFacility 12 = SOME Ntp
    | intToFacility 13 = SOME LogAudit
    | intToFacility 14 = SOME LogAlert
    | intToFacility 15 = SOME Clock
    | intToFacility 16 = SOME Local0
    | intToFacility 17 = SOME Local1
    | intToFacility 18 = SOME Local2
    | intToFacility 19 = SOME Local3
    | intToFacility 20 = SOME Local4
    | intToFacility 21 = SOME Local5
    | intToFacility 22 = SOME Local6
    | intToFacility 23 = SOME Local7
    | intToFacility _  = NONE

  fun severityToInt Emerg   = 0
    | severityToInt Alert   = 1
    | severityToInt Crit    = 2
    | severityToInt Err     = 3
    | severityToInt Warning = 4
    | severityToInt Notice  = 5
    | severityToInt Info    = 6
    | severityToInt Debug   = 7

  fun intToSeverity 0 = SOME Emerg
    | intToSeverity 1 = SOME Alert
    | intToSeverity 2 = SOME Crit
    | intToSeverity 3 = SOME Err
    | intToSeverity 4 = SOME Warning
    | intToSeverity 5 = SOME Notice
    | intToSeverity 6 = SOME Info
    | intToSeverity 7 = SOME Debug
    | intToSeverity _ = NONE

  fun priToString (facility, severity) =
        let
          val added = facilityToInt facility * 8 + severityToInt severity
        in
          "<" ^ Int.toString added ^ ">"
        end

  infix >>=
  fun (SOME x) >>= k = k x
    | NONE     >>= k = NONE

  val toSlice = Word8VectorSlice.full o Byte.stringToBytes

  structure Remote = struct
    fun construct (pri, (timestamp, hostname), msg) =
          let
            val pri' = priToString pri
            val month = Date.fmt "%b" timestamp
            val day =
                let val day = Int.toString (Date.day timestamp) in
                  if String.size day < 2 then " " ^ day
                  else day
                end
            val time = Date.fmt "%H:%M:%S" timestamp
          in
            String.concatWith " " [pri', month,  day, time, hostname, msg] ^ "\n"
          end

    fun log' sockFactory timestampFactory sock_addr pri msg =
          let
            val sock = sockFactory ()
            val timestamp = timestampFactory ()
            val hostname = NetHostDB.getHostName ()
            val message = construct (pri, (timestamp, hostname), msg)
          in
            Socket.sendVecTo (sock, sock_addr, toSlice message)
          end

    val log = log' (fn () => INetSock.UDP.socket ()) (fn () => Date.fromTimeLocal (Time.now ()))

    fun socketFromAddr addr =
          let
            val sock = INetSock.UDP.socket ()
            val _ = Socket.bind (sock, addr)
          in
            sock
          end
  end

  structure Local = struct
    fun log pri msg =
          let
            val sock_addr = UnixSock.toAddr "/dev/log"
            val sock = UnixSock.DGrm.socket ()
            val message = priToString pri ^ msg
          in
            Socket.sendVecTo (sock, sock_addr, toSlice message)
          end

    val emerg   = log (User, Emerg)
    val alert   = log (User, Alert)
    val crit    = log (User, Crit)
    val err     = log (User, Err)
    val warning = log (User, Warning)
    val notice  = log (User, Notice)
    val info    = log (User, Info)
    val debug   = log (User, Debug)

    fun socketFromPath path =
          let
            val addr = UnixSock.toAddr path
            (* Pre-condition: the Unix domain socket is not in use.
               Otherwise, bind will fail. *)
            fun create path =
                  let
                    val sock = UnixSock.DGrm.socket ()
                    val _ = Socket.bind (sock, addr)
                  in
                    sock
                  end
          in
            if OS.FileSys.access (path, [])
            then
              let
                val sock = UnixSock.DGrm.socket ()
              in
                let
                  val sockOpt =
                    (ignore (Socket.connectNB (sock, addr)); NONE)
                    handle OS.SysErr (_, _) => (
                      (* if connect raises an exception,
                         the socket file can be safely removed *)
                      OS.FileSys.remove path;
                      SOME (create path))
                 in
                   case sockOpt of
                        SOME sock => sock
                      | NONE =>
                          (* else, someone else is using the socket *)
                          raise OS.SysErr ("\"" ^  path ^ "\" already in use", NONE)
                end
              end
            else
              create path
          end
  end

  structure Server = struct
    fun receiveLoop (sock, ch) =
          let
            val (vec, _) = Socket.recvVecFrom (sock, 1024)
          in
            CML.send (ch, Byte.bytesToString vec);
            receiveLoop (sock, ch)
          end

    fun start (path, addr) =
          let
            val ch = CML.channel ()
            val localSock = Local.socketFromPath path
            val remoteSock = Remote.socketFromAddr addr
            val receiveLocal = CML.spawn (fn () => receiveLoop (localSock, ch))
            val receiveRemote = CML.spawn (fn () => receiveLoop (remoteSock, ch))
            fun loop () =
                  let
                    val message = CML.recv ch
                  in
                    print message;
                    loop ()
                  end
          in
            loop ()
          end
          handle OS.SysErr (m, _) => print ("OS.SysErr " ^ m ^ "\n")
  end

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
end

structure Syslogd = struct
  fun main (name : string, argv : string list) =
        let
          fun boot () = (
            print "starting syslogd\n";
            Syslog.Server.start ("log", valOf (Syslog.stringToAddr ("0.0.0.0:5140")))
            )
        in
          print "booting\n";
          RunCML.doit (boot, NONE)
        end
end

fun main () =
      OS.Process.exit (Syslogd.main (CommandLine.name (), CommandLine.arguments ()))

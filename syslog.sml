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
    | facilityToInt Local0   = 16
    | facilityToInt Local1   = 17
    | facilityToInt Local2   = 18
    | facilityToInt Local3   = 19
    | facilityToInt Local4   = 20
    | facilityToInt Local5   = 21
    | facilityToInt Local6   = 22
    | facilityToInt Local7   = 23

  fun severityToInt Emerg   = 0
    | severityToInt Alert   = 1
    | severityToInt Crit    = 2
    | severityToInt Err     = 3
    | severityToInt Warning = 4
    | severityToInt Notice  = 5
    | severityToInt Info    = 6
    | severityToInt Debug   = 7

  fun priToString (facility, severity) =
        let
          val added = facilityToInt facility * 8 + severityToInt severity
        in
          "<" ^ Int.toString added ^ ">"
        end

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
  end
end

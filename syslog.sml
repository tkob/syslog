structure Facility = struct
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
                    | Audit
                    | Alert
                    | Clock
                    | Local0
                    | Local1
                    | Local2
                    | Local3
                    | Local4
                    | Local5
                    | Local6
                    | Local7
  fun num Kern     = 0
    | num User     = 1
    | num Mail     = 2
    | num Daemon   = 3
    | num Auth     = 4
    | num Syslog   = 5
    | num Lpr      = 6
    | num New      = 7
    | num Uucp     = 8
    | num Cron     = 9
    | num Authpriv = 10
    | num Ftp      = 11
    | num Ntp      = 12
    | num Audit    = 13
    | num Alert    = 14
    | num Clock    = 15
    | num Local0   = 16
    | num Local1   = 17
    | num Local2   = 18
    | num Local3   = 19
    | num Local4   = 20
    | num Local5   = 21
    | num Local6   = 22
    | num Local7   = 23
end

structure Severity = struct
  datatype severity = Emerg | Alert | Crit | Err | Warning | Notice | Info | Debug
  fun num Emerg   = 0
    | num Alert   = 1
    | num Crit    = 2
    | num Err     = 3
    | num Warning = 4
    | num Notice  = 5
    | num Info    = 6
    | num Debug   = 7
end

structure Pri = struct
  type pri = Facility.facility * Severity.severity

  fun value (facility, severity) =
        Facility.num facility * 8 + Severity.num severity

  fun toString pri = "<" ^ Int.toString (value pri) ^ ">"
end

structure Syslog :> sig
  type hostname = string
  type message
  type port = int

  val pri : message -> Pri.pri
  val timestamp : message -> Date.date
  val hostname : message -> hostname
  val msg : message -> string

  val log' : (unit -> Date.date) -> INetSock.sock_addr -> Pri.pri -> string -> unit
  val log : INetSock.sock_addr -> Pri.pri -> string -> unit
  val logLocal : Pri.pri -> string -> unit

  val emerg : string -> unit
  val alert : string -> unit
  val crit : string -> unit
  val err : string -> unit
  val warning : string -> unit
  val notice : string -> unit
  val info : string -> unit
  val debug : string -> unit
end = struct
  type hostname = string
  type header = Date.date * hostname
  type message = Pri.pri * header * string

  type port = int

  fun pri (pri, _, _) = pri
  fun timestamp (_, (timestamp, _), _) = timestamp
  fun hostname (_, (_, hostname), _) = hostname
  fun msg (_, _, msg) = msg

  fun construct (pri, (timestamp, hostname), msg) =
        let
          val pri' = Pri.toString pri
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

  val toSlice = Word8VectorSlice.full o Byte.stringToBytes
  fun log' timestampFactory sock_addr pri msg =
        let
          val sock = INetSock.UDP.socket ()
          val timestamp = timestampFactory ()
          val hostname = NetHostDB.getHostName ()
          val message = construct (pri, (timestamp, hostname), msg)
        in
          Socket.sendVecTo (sock, sock_addr, toSlice message)
        end

  fun logLocal pri msg =
        let
          val sock_addr = UnixSock.toAddr "/dev/log"
          val sock = UnixSock.DGrm.socket ()
          val message = Pri.toString pri ^ msg
        in
          Socket.sendVecTo (sock, sock_addr, toSlice message)
        end

  val emerg   = logLocal (Facility.User, Severity.Emerg)
  val alert   = logLocal (Facility.User, Severity.Alert)
  val crit    = logLocal (Facility.User, Severity.Crit)
  val err     = logLocal (Facility.User, Severity.Err)
  val warning = logLocal (Facility.User, Severity.Warning)
  val notice  = logLocal (Facility.User, Severity.Notice)
  val info    = logLocal (Facility.User, Severity.Info)
  val debug   = logLocal (Facility.User, Severity.Debug)
end

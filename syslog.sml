structure Syslog :> sig
  structure Facility : sig
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
    val num : facility -> int
  end

  structure Severity : sig
    datatype severity = Emerg | Alert | Crit | Err | Warning | Notice | Info | Debug
    val num : severity -> int
  end

  structure Pri : sig
    type pri = Facility.facility * Severity.severity
  
    val value : pri -> int
    val toString : pri -> string
  end

  structure Remote : sig
    val log : INetSock.sock_addr -> Pri.pri -> string -> unit
  end

  structure Local : sig
    val log : Pri.pri -> string -> unit

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

  val toSlice = Word8VectorSlice.full o Byte.stringToBytes

  structure Remote = struct
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
            val message = Pri.toString pri ^ msg
          in
            Socket.sendVecTo (sock, sock_addr, toSlice message)
          end

    val emerg   = log (Facility.User, Severity.Emerg)
    val alert   = log (Facility.User, Severity.Alert)
    val crit    = log (Facility.User, Severity.Crit)
    val err     = log (Facility.User, Severity.Err)
    val warning = log (Facility.User, Severity.Warning)
    val notice  = log (Facility.User, Severity.Notice)
    val info    = log (Facility.User, Severity.Info)
    val debug   = log (Facility.User, Severity.Debug)
  end
end

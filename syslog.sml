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

  structure Message : sig
      type header = Date.date * string
      type message = pri option * header option * string

      val fromString : string -> message
      val toString : message -> string
  end

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

  structure Conf : sig
    exception Conf of string

    datatype facility_pattern = Facility of facility | AnyFacility
    datatype priority_pattern = GreaterThanOrEqualPriority of severity | AnyPriority | NonePriority
    type selector = facility_pattern list * priority_pattern
    datatype action = File of string
    type rule = selector list * action

    val parseRule : Substring.substring -> rule
    val load : ('strm -> (string * 'strm) option) -> 'strm -> rule list
    val run : rule list -> pri -> action list
    val app : (action -> unit) -> rule list -> pri -> unit
  end

  structure Server : sig
    val start : string * INetSock.sock_addr -> unit
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

  infix >>=
  fun (SOME x) >>= k = k x
    | NONE     >>= k = NONE

  structure Message = struct
    type header = Date.date * string
    type message = pri option * header option * string

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

    fun stringToFacility' "kern"     = SOME Kern
      | stringToFacility' "user"     = SOME User
      | stringToFacility' "mail"     = SOME Mail
      | stringToFacility' "daemon"   = SOME Daemon
      | stringToFacility' "auth"     = SOME Auth
      | stringToFacility' "security" = SOME Auth
      | stringToFacility' "syslog"   = SOME Syslog
      | stringToFacility' "lpr"      = SOME Lpr
      | stringToFacility' "news"     = SOME New
      | stringToFacility' "uucp"     = SOME Uucp
      | stringToFacility' "cron"     = SOME Cron
      | stringToFacility' "authpriv" = SOME Authpriv
      (*
      | stringToFacility' ""         = SOME Ftp
      | stringToFacility' ""         = SOME Ntp
      | stringToFacility' ""         = SOME LogAudit
      | stringToFacility' ""         = SOME LogAlert
      | stringToFacility' ""         = SOME Clock
      *)
      | stringToFacility' "local0"   = SOME Local0
      | stringToFacility' "local1"   = SOME Local1
      | stringToFacility' "local2"   = SOME Local2
      | stringToFacility' "local3"   = SOME Local3
      | stringToFacility' "local4"   = SOME Local4
      | stringToFacility' "local5"   = SOME Local5
      | stringToFacility' "local6"   = SOME Local6
      | stringToFacility' "local7"   = SOME Local7
      | stringToFacility' _  = NONE
    fun stringToFacility s = stringToFacility' (String.map Char.toLower s)

    fun severityToInt Emerg   = 0
      | severityToInt Alert   = 1
      | severityToInt Crit    = 2
      | severityToInt Err     = 3
      | severityToInt Warning = 4
      | severityToInt Notice  = 5
      | severityToInt Info    = 6
      | severityToInt Debug   = 7

    fun gt (s1, s2) = severityToInt s1 <  severityToInt s2
    fun ge (s1, s2) = severityToInt s1 <= severityToInt s2
    fun lt (s1, s2) = severityToInt s1 >  severityToInt s2
    fun le (s1, s2) = severityToInt s1 >= severityToInt s2

    fun intToSeverity 0 = SOME Emerg
      | intToSeverity 1 = SOME Alert
      | intToSeverity 2 = SOME Crit
      | intToSeverity 3 = SOME Err
      | intToSeverity 4 = SOME Warning
      | intToSeverity 5 = SOME Notice
      | intToSeverity 6 = SOME Info
      | intToSeverity 7 = SOME Debug
      | intToSeverity _ = NONE

    fun stringToSeverity' "emerge"  = SOME Emerg
      | stringToSeverity' "panic"   = SOME Emerg
      | stringToSeverity' "alert"   = SOME Alert
      | stringToSeverity' "crit"    = SOME Crit
      | stringToSeverity' "err"     = SOME Err
      | stringToSeverity' "error"   = SOME Err
      | stringToSeverity' "warning" = SOME Warning
      | stringToSeverity' "warn"    = SOME Warning
      | stringToSeverity' "notice"  = SOME Notice
      | stringToSeverity' "info"    = SOME Info
      | stringToSeverity' "debug"   = SOME Debug
      | stringToSeverity' _ = NONE
    fun stringToSeverity s = stringToSeverity' (String.map Char.toLower s)

    fun priToString (facility, severity) =
          let
            val added = facilityToInt facility * 8 + severityToInt severity
          in
            "<" ^ Int.toString added ^ ">"
          end

    fun intToPri i =
          let
            val hi = i div 8
            val lo = i mod 8
          in
            intToFacility hi >>= (fn facility =>
            intToSeverity lo >>= (fn severity =>
            SOME (facility, severity)))
          end

    infix ||
    fun (a || b) input1 strm =
          case a input1 strm of
               SOME x => SOME x
             | NONE => b input1 strm
    infix --
    fun (a -- b) input1 strm =
          a input1 strm >>= (fn (e1, strm) =>
          b input1 strm >>= (fn (e2, strm) =>
          SOME ((e1, e2), strm)))
    fun flattenTriple ((e1, e2), e3) = SOME (e1, e2, e3)
    fun flatten4Tuple (((e1, e2), e3), e4) = SOME (e1, e2, e3, e4)
    fun flatten5Tuple ((((e1, e2), e3), e4), e5) = SOME (e1, e2, e3, e4, e5)
    fun transform a f input1 strm =
          case a input1 strm of
               NONE => NONE
             | SOME (elem, strm') =>
                 case f elem of
                      NONE => NONE
                    | SOME elem' => SOME (elem', strm')
    infix >>
    fun (a >> f) input1 strm = transform a f input1 strm
    fun repeat class input1 strm =
          let
            fun loop strm acc =
                  case class input1 strm of
                       SOME (c, strm') => loop strm' (c::acc)
                     | NONE => SOME (rev acc, strm)
          in
            loop strm []
          end
    fun many0 class input1 strm = (
          (repeat class >> (fn acc => SOME acc)))
          input1 strm
    fun many1 class input1 strm = (
          (class -- repeat class)
          >> (fn (car, cdr) => SOME (car::cdr)))
          input1 strm
    fun opt class input1 strm =
          case class input1 strm of
               NONE => SOME (NONE, strm)
             | SOME (x, strm') => SOME (SOME x, strm')
    fun pred p input1 strm = (* consume an element that satisfies p *)
          case input1 strm of
               NONE => NONE
             | SOME (c', strm') =>
                 if p c' then SOME (c', strm') else NONE
    fun any input1 strm = pred (fn _ => true) input1 strm
    fun char c input1 strm = pred (fn c' => c' = c) input1 strm
    fun string s input1 strm =
          let
            val substring = Substring.full s
            fun loop (substring, strm) =
                  case Substring.getc substring of
                       NONE => SOME (s, strm)
                     | SOME (c, substring') =>
                         case input1 strm of
                              NONE => NONE
                            | SOME (c', strm') =>
                                if c' = c then loop (substring', strm') else NONE
          in
            loop (substring, strm)
          end
    fun space input1 strm = char #" " input1 strm
    fun digit input1 strm = pred Char.isDigit input1 strm
    fun alpha input1 strm = pred Char.isAlpha input1 strm
    fun int input1 strm = (
          (many1 digit)
          >> Option.filter (fn cs => cs = [#"0"] orelse hd cs <> #"0")
          >> (Int.fromString o implode))
          input1 strm
    fun pri input1 strm = (
          (char #"<" -- int -- char #">")
          >> flattenTriple
          >> (fn (_, i, _) => intToPri i))
          input1 strm
    fun stringToMonth "Jan" = SOME Date.Jan
      | stringToMonth "Feb" = SOME Date.Feb
      | stringToMonth "Mar" = SOME Date.Mar
      | stringToMonth "Apr" = SOME Date.Apr
      | stringToMonth "May" = SOME Date.May
      | stringToMonth "Jun" = SOME Date.Jun
      | stringToMonth "Jul" = SOME Date.Jul
      | stringToMonth "Aug" = SOME Date.Aug
      | stringToMonth "Sep" = SOME Date.Sep
      | stringToMonth "Oct" = SOME Date.Oct
      | stringToMonth "Nov" = SOME Date.Nov
      | stringToMonth "Dec" = SOME Date.Dec
      | stringToMonth _ = NONE
    fun month input1 strm = ((
             string "Jan"
          || string "Feb"
          || string "Mar"
          || string "Apr"
          || string "May"
          || string "Jun"
          || string "Jul"
          || string "Aug"
          || string "Sep"
          || string "Oct"
          || string "Nov"
          || string "Dec")
          >> stringToMonth) input1 strm
    fun twoDigitsToInt (c1, c2) = Int.fromString (implode [c1, c2])
    fun day input1 strm =
          ((space -- digit) || (digit -- digit) >> twoDigitsToInt) input1 strm
    fun twoDigits input1 strm =
          ((digit -- digit) >> twoDigitsToInt) input1 strm
    fun time input1 strm = (
          (twoDigits -- char #":" -- twoDigits -- char #":" -- twoDigits)
          >> flatten5Tuple
          >> (fn (hour, _, minute, _, second) => SOME (hour, minute, second)))
          input1 strm
    fun timestamp input1 strm = (
          (month -- space -- day -- space -- time)
          >> flatten5Tuple
          >> (fn (month, _, day, _, (hour, minute, second)) =>
               SOME (Date.date {
                 year = 1970, (* year not present in RFC3164 timestamp *)
                 month = month,
                 day = day,
                 hour = hour,
                 minute = minute,
                 second = second,
                 offset = NONE })))
          input1 strm
    fun hostname input1 strm = (
          (many1 (digit || alpha || char #"." || char #"-" || char #":"))
          >> (fn cs => SOME (implode cs)))
          input1 strm
    fun header input1 strm = (
          (timestamp -- space -- hostname -- space)
          >> flatten4Tuple
          >> (fn (timestamp, _, hostname, _) => SOME (timestamp, hostname)))
          input1 strm
    fun msg input1 strm = (
          (many0 any) >> (fn cs => SOME (implode cs)))
          input1 strm
    fun priAndHeader input1 strm = (
          (pri -- header -- msg)
          >> flattenTriple
          >> (fn (pri, header, msg) =>
                SOME (SOME pri, SOME header, msg)))
          input1 strm
    fun priOnly input1 strm = (
          (pri -- msg)
          >> (fn (pri, msg) =>
                SOME (SOME pri, NONE, msg)))
          input1 strm
    fun noPri input1 strm = (
          msg
          >> (fn msg =>
                SOME (NONE, NONE, msg)))
          input1 strm
    fun message input1 strm = (priAndHeader || priOnly || noPri) input1 strm

    fun fromString s = case message Substring.getc (Substring.full s) of
                            NONE => raise Fail "should never reach here"
                          | SOME (s, _) => s

    fun timestampToString timestamp =
          Date.fmt "%b " timestamp ^
          (if Date.day timestamp < 10 then " " else "") ^
          Int.toString (Date.day timestamp) ^
          Date.fmt " %H:%M:%S" timestamp

    fun toString (SOME pri, SOME (timestamp, host), msg) =
          priToString pri ^ timestampToString timestamp ^ " " ^ host ^ " " ^ msg
      | toString (SOME pri, NONE, msg) =
          priToString pri ^ msg
      | toString (NONE, NONE, msg) =
          msg
      | toString (NONE, SOME (timestamp, host), msg) =
          "<13>" ^ timestampToString timestamp ^ " " ^ host ^ " " ^ msg
  end

  val toSlice = Word8VectorSlice.full o Byte.stringToBytes

  structure Remote = struct
    fun construct (pri, (timestamp, hostname), msg) =
          let
            val pri' = Message.priToString pri
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
            val message = Message.priToString pri ^ msg
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

  structure Conf = struct
    exception Conf of string

    datatype facility_pattern = Facility of facility | AnyFacility
    datatype priority_pattern = GreaterThanOrEqualPriority of severity | AnyPriority | NonePriority
    type selector = facility_pattern list * priority_pattern
    datatype action = File of string
    type rule = selector list * action

    fun parseFacility s =
          if s = "*" then AnyFacility
          else
            case Message.stringToFacility s of
                 NONE => raise Conf s
               | SOME facility => Facility facility

    fun parseFacilities s =
          let
            val facilities = Substring.tokens (fn c => c = #",") s
          in
            map (parseFacility o Substring.string) facilities
          end

    fun parseSelector s =
          let
            val (facilities, priority) =
              case Substring.tokens (fn c => c = #".") s of
                   [facilities, priority] =>
                     (facilities, Substring.string priority)
                 | _ => raise Conf (Substring.string s)
            val facilities = parseFacilities facilities
            val priority =
              if priority = "*" then AnyPriority
              else if priority = "none" then NonePriority
              else
                case Message.stringToSeverity priority of
                     NONE => raise Conf priority
                   | SOME priority => GreaterThanOrEqualPriority priority
          in
            (facilities, priority)
          end

    fun parseSelectors s =
          let
            val selectors = Substring.tokens (fn c => c = #";") s
          in
            map parseSelector selectors
          end

    fun parseAction s = File (Substring.string s)

    fun parseRule s =
          let
            val (selectors, action) =
              case Substring.tokens Char.isSpace s of
                   [selectors, action] => (selectors, action)
                 | _ => raise Conf (Substring.string s)
          in
            (parseSelectors selectors, parseAction action)
          end

    fun load inputLine strm =
          let
            fun processLine strm rules =
                  case inputLine strm of
                       NONE => rev rules
                     | SOME (line, strm') =>
                         let
                           val line = Substring.full line
                           val line = Substring.dropl Char.isSpace line
                         in
                           if Substring.isEmpty line orelse Substring.sub (line, 0) = #"#"
                           then
                             processLine strm' rules
                           else
                             processLine strm' (parseRule line::rules)
                         end
          in
            processLine strm []
          end

    (* match at least one of the comma-delimited facilities? *)
    fun matchFacilities [] _ = false
      | matchFacilities (AnyFacility::facilities) _ = true
      | matchFacilities (Facility facility::facilities) facility' =
          if facility = facility' then true
          else matchFacilities facilities facility'

    fun matchSelectors [] _ matched = matched
      | matchSelectors ((facilities, priority)::selectors) (pri as (facility, severity)) matched =
          if matchFacilities facilities facility then
            (* update(override) matched status *)
            case priority of
                 GreaterThanOrEqualPriority severity' =>
                   matchSelectors selectors pri (Message.ge (severity, severity'))
               | AnyPriority =>
                   matchSelectors selectors pri true
               | NonePriority =>
                   matchSelectors selectors pri false
          else
            matchSelectors selectors pri matched

    fun fold f actions [] pri = actions
      | fold f actions ((selectors, action)::rules) pri =
          if matchSelectors selectors pri false then
            fold f (f (action, actions)) rules pri
          else
            fold f actions rules pri
    fun run rules pri = rev (fold (op ::) [] rules pri)
    fun app f rules pri = fold (fn (x, ()) => (f x)) () rules pri
  end

  structure Server = struct
    fun receiveLoop (sock, ch) =
          let
            val (vec, _) = Socket.recvVecFrom (sock, 1024)
            val s = Byte.bytesToString vec
            val message = Message.fromString s
          in
            CML.send (ch, message);
            receiveLoop (sock, ch)
          end

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

    fun socketFromAddr addr =
          let
            val sock = INetSock.UDP.socket ()
            val _ = Socket.bind (sock, addr)
          in
            sock
          end

    fun fileWriter (fd, ch) =
          let
            val message = CML.recv ch
            val string = Message.toString message ^ "\n"
            val vec = Word8VectorSlice.full (Byte.stringToBytes string)
            val writtenBytes = Posix.IO.writeVec (fd, vec)
          in
            fileWriter (fd, ch)
          end

    fun outputFileFromPath path =
          let
            open Posix.FileSys
            val mode600 = S.flags [S.irusr, S.iwusr]
          in
            (* open path, create if it does not exist *)
            createf (path, O_WRONLY, O.flags [O.append, O.sync], mode600)
          end

    fun start (path, addr) =
          let
            val ch = CML.channel ()
            val fd = outputFileFromPath "messages"
            val fileWriter = CML.spawn (fn () => fileWriter (fd, ch))
            val localSock = socketFromPath path
            val remoteSock = socketFromAddr addr
            val receiveLocal = CML.spawn (fn () => receiveLoop (localSock, ch))
            val receiveRemote = CML.spawn (fn () => receiveLoop (remoteSock, ch))
          in
            ()
          end
  end
end

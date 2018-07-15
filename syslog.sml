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

    val toInt : facility -> int
    val fromInt : int -> facility option
    val fromString : string -> facility option
  end

  datatype severity = Emerg | Alert | Crit | Err | Warning | Notice | Info | Debug

  type pri = Facility.facility * severity

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

    datatype facility_pattern = Facility of Facility.facility | AnyFacility
    datatype priority_pattern = GreaterThanOrEqualPriority of severity | AnyPriority | NonePriority
    type selector = facility_pattern list * priority_pattern
    datatype action = File of string
    type rule = selector list * action

    val parseRule : Substring.substring -> rule
    val load : ('strm -> (string * 'strm) option) -> 'strm -> rule list
    val run : rule list -> pri -> action list
    val app : (action -> unit) -> rule list -> pri -> unit
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

    fun toInt Kern     = 0
      | toInt User     = 1
      | toInt Mail     = 2
      | toInt Daemon   = 3
      | toInt Auth     = 4
      | toInt Syslog   = 5
      | toInt Lpr      = 6
      | toInt New      = 7
      | toInt Uucp     = 8
      | toInt Cron     = 9
      | toInt Authpriv = 10
      | toInt Ftp      = 11
      | toInt Ntp      = 12
      | toInt LogAudit = 13
      | toInt LogAlert = 14
      | toInt Clock    = 15
      | toInt Local0   = 16
      | toInt Local1   = 17
      | toInt Local2   = 18
      | toInt Local3   = 19
      | toInt Local4   = 20
      | toInt Local5   = 21
      | toInt Local6   = 22
      | toInt Local7   = 23

    fun fromInt 0  = SOME Kern
      | fromInt 1  = SOME User
      | fromInt 2  = SOME Mail
      | fromInt 3  = SOME Daemon
      | fromInt 4  = SOME Auth
      | fromInt 5  = SOME Syslog
      | fromInt 6  = SOME Lpr
      | fromInt 7  = SOME New
      | fromInt 8  = SOME Uucp
      | fromInt 9  = SOME Cron
      | fromInt 10 = SOME Authpriv
      | fromInt 11 = SOME Ftp
      | fromInt 12 = SOME Ntp
      | fromInt 13 = SOME LogAudit
      | fromInt 14 = SOME LogAlert
      | fromInt 15 = SOME Clock
      | fromInt 16 = SOME Local0
      | fromInt 17 = SOME Local1
      | fromInt 18 = SOME Local2
      | fromInt 19 = SOME Local3
      | fromInt 20 = SOME Local4
      | fromInt 21 = SOME Local5
      | fromInt 22 = SOME Local6
      | fromInt 23 = SOME Local7
      | fromInt _  = NONE

    fun fromString' "kern"     = SOME Kern
      | fromString' "user"     = SOME User
      | fromString' "mail"     = SOME Mail
      | fromString' "daemon"   = SOME Daemon
      | fromString' "auth"     = SOME Auth
      | fromString' "security" = SOME Auth
      | fromString' "syslog"   = SOME Syslog
      | fromString' "lpr"      = SOME Lpr
      | fromString' "news"     = SOME New
      | fromString' "uucp"     = SOME Uucp
      | fromString' "cron"     = SOME Cron
      | fromString' "authpriv" = SOME Authpriv
      (*
      | fromString' ""         = SOME Ftp
      | fromString' ""         = SOME Ntp
      | fromString' ""         = SOME LogAudit
      | fromString' ""         = SOME LogAlert
      | fromString' ""         = SOME Clock
      *)
      | fromString' "local0"   = SOME Local0
      | fromString' "local1"   = SOME Local1
      | fromString' "local2"   = SOME Local2
      | fromString' "local3"   = SOME Local3
      | fromString' "local4"   = SOME Local4
      | fromString' "local5"   = SOME Local5
      | fromString' "local6"   = SOME Local6
      | fromString' "local7"   = SOME Local7
      | fromString' _  = NONE
    fun fromString s = fromString' (String.map Char.toLower s)
  end

  datatype severity = Emerg | Alert | Crit | Err | Warning | Notice | Info | Debug

  type pri = Facility.facility * severity

  infix >>=
  fun (SOME x) >>= k = k x
    | NONE     >>= k = NONE

  structure Message = struct
    type header = Date.date * string
    type message = pri option * header option * string

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
            val added = Facility.toInt facility * 8 + severityToInt severity
          in
            "<" ^ Int.toString added ^ ">"
          end

    fun intToPri i =
          let
            val hi = i div 8
            val lo = i mod 8
          in
            Facility.fromInt hi >>= (fn facility =>
            intToSeverity    lo >>= (fn severity =>
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

    val emerg   = log (Facility.User, Emerg)
    val alert   = log (Facility.User, Alert)
    val crit    = log (Facility.User, Crit)
    val err     = log (Facility.User, Err)
    val warning = log (Facility.User, Warning)
    val notice  = log (Facility.User, Notice)
    val info    = log (Facility.User, Info)
    val debug   = log (Facility.User, Debug)
  end

  structure Conf = struct
    exception Conf of string

    datatype facility_pattern = Facility of Facility.facility | AnyFacility
    datatype priority_pattern = GreaterThanOrEqualPriority of severity | AnyPriority | NonePriority
    type selector = facility_pattern list * priority_pattern
    datatype action = File of string
    type rule = selector list * action

    fun parseFacility s =
          if s = "*" then AnyFacility
          else
            case Facility.fromString s of
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
end

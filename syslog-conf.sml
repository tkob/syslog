structure SyslogConf :> sig
  exception SyslogConf of string

  datatype facility_pattern = Facility of Syslog.Facility.facility
                            | AnyFacility
  datatype priority_pattern = GreaterThanOrEqualPriority of Syslog.Severity.severity
                            | AnyPriority
                            | NonePriority
  type selector = facility_pattern list * priority_pattern
  datatype action = File of string
                  | Remote of string
                  | RemoteTcp of string
  type rule = selector list * action

  val parseRule : Substring.substring -> rule
  val load : ('strm -> (string * 'strm) option) -> 'strm -> rule list
  val loadFile : string -> rule list
  val run : rule list -> Syslog.Pri.pri -> action list
  val app : (action -> unit) -> rule list -> Syslog.Pri.pri -> unit
end = struct
  open Syslog

  exception SyslogConf of string

  datatype facility_pattern = Facility of Syslog.Facility.facility
                            | AnyFacility
  datatype priority_pattern = GreaterThanOrEqualPriority of Syslog.Severity.severity
                            | AnyPriority
                            | NonePriority
  type selector = facility_pattern list * priority_pattern
  datatype action = File of string
                  | Remote of string
                  | RemoteTcp of string
  type rule = selector list * action

  fun parseFacility s =
        if s = "*" then AnyFacility
        else
          case Syslog.Facility.fromString s of
               NONE => raise SyslogConf s
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
               | _ => raise SyslogConf (Substring.string s)
          val facilities = parseFacilities facilities
          val priority =
            if priority = "*" then AnyPriority
            else if priority = "none" then NonePriority
            else
              case Syslog.Severity.fromString priority of
                   NONE => raise SyslogConf priority
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

  fun parseAction s =
        if Substring.isPrefix "@@" s then
          RemoteTcp (Substring.string (Substring.triml 2 s))
        else if Substring.isPrefix "@" s then
          Remote (Substring.string (Substring.triml 1 s))
        else
          File (Substring.string s)

  fun parseRule s =
        let
          val (selectors, action) =
            case Substring.tokens Char.isSpace s of
                 [selectors, action] => (selectors, action)
               | _ => raise SyslogConf (Substring.string s)
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

  fun loadFile fileName =
        let
          val strm = TextIO.getInstream (TextIO.openIn fileName)
        in
          load TextIO.StreamIO.inputLine strm
          before TextIO.StreamIO.closeIn strm
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
                 matchSelectors selectors pri (Syslog.Severity.ge (severity, severity'))
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


structure SyslogServer :> sig
  val start : string * INetSock.sock_addr * SyslogConf.rule list * (Syslog.Message.message CML.chan * SyslogConf.action -> (unit -> unit)) -> unit
end = struct
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
              exception InUse
             in
               (ignore (Socket.connectNB (UnixSock.DGrm.socket (), addr)); raise InUse)
               handle
                 OS.SysErr (_, _) =>
                   (* if connect raises an exception,
                      the socket is not used and it can be safely removed *)
                   (OS.FileSys.remove path; create path)
               | InUse =>
                   (* else, someone else is using the socket *)
                   raise OS.SysErr ("\"" ^  path ^ "\" already in use", NONE)
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

  local
    fun uniq' [] acc = acc
      | uniq' (x::xs) acc =
          if List.exists (fn x' => x' = x) acc
          then uniq' xs acc
          else uniq' xs (x::acc)
  in
    fun uniq xs = uniq' xs []
  end

  fun start (path, addr, rules, writerFactory) =
        let
          infix |>
          fun (x |> f) = f x
          val actions = uniq (map #2 rules)
          val actionToCh = actions |> map (fn action =>
            let
              val ch = CML.channel ()
              val writer = writerFactory (ch, action)
            in
              CML.spawn writer;
              (action, ch)
            end)
          fun lookupCh [] action =
                raise Fail "action not found: should never reach here"
            | lookupCh ((action, ch)::actions) action' =
                if action = action' then ch
                else lookupCh actions action'
          fun sendToCh message action =
                CML.send (lookupCh actionToCh action, message)
          fun receiveLoop sock =
                let
                  val (vec, _) = Socket.recvVecFrom (sock, 1024)
                  val s = Byte.bytesToString vec
                  val message = Syslog.Message.fromString s
                  val pri = case #1 message of
                                 NONE =>(Syslog.Facility.User, Syslog.Severity.Info)
                               | SOME pri => pri
                in
                  SyslogConf.app (sendToCh message) rules pri;
                  receiveLoop sock
                end
          val localSock = socketFromPath path
          val remoteSock = socketFromAddr addr
          val receiveLocal = CML.spawn (fn () => receiveLoop localSock)
          val receiveRemote = CML.spawn (fn () => receiveLoop remoteSock)
          val syslogInfo = (Syslog.Facility.Syslog, Syslog.Severity.Info)
          val startMessage = (SOME syslogInfo, NONE, "syslogd started")
        in
          SyslogConf.app (sendToCh startMessage) rules syslogInfo
        end
end


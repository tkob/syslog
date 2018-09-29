structure SyslogServer :> sig
  val start : string * INetSock.sock_addr * SyslogConf.rule list * (Syslog.Message.message Channel.chan * SyslogConf.action -> (unit -> unit)) -> unit
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
              val ch = Channel.chan ()
              val writer = writerFactory (ch, action)
            in
              writer ();
              (action, ch)
            end)
          fun lookupCh [] action =
                raise Fail "action not found: should never reach here"
            | lookupCh ((action, ch)::actions) action' =
                if action = action' then ch
                else lookupCh actions action'

          val localSock = socketFromPath path
          val remoteSock = socketFromAddr addr

          fun receiveAndRoute sock k =
                let
                  val (vec, _) = Socket.recvVecFrom (sock, 1024)
                  val s = Byte.bytesToString vec
                  val message = Syslog.Message.fromString s
                  val pri = case #1 message of
                                 NONE => (Syslog.Facility.User, Syslog.Severity.Info)
                               | SOME pri => pri
                  val actions = SyslogConf.run rules pri
                  fun sendToCh [] = k ()
                    | sendToCh (action::actions) =
                        Channel.send (lookupCh actionToCh action) message (fn () => sendToCh actions)
                in
                  sendToCh actions
                end
          type handler = { sockDesc : Socket.sock_desc, handler : (unit -> unit) -> unit }
          val handlers = [
            { sockDesc = Socket.sockDesc localSock, handler = fn k => receiveAndRoute localSock k},
            { sockDesc = Socket.sockDesc remoteSock, handler = fn k => receiveAndRoute remoteSock k}]
          val descs = map #sockDesc handlers
          fun loop () =
                let
                  val {rds, wrs, exs} =
                    Socket.select {rds = descs, wrs = [], exs = [], timeout = NONE}
                  fun callHandler [] = loop ()
                    | callHandler (rd::rds) =
                        let
                          val SOME {handler, sockDesc} =
                            List.find (fn {sockDesc, handler} => Socket.sameDesc (rd, sockDesc)) handlers
                        in
                          handler (fn () => callHandler rds)
                        end
                in
                  callHandler rds
                end
        in
          loop ()
        end
end

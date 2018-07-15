structure SyslogServer :> sig
  val start : string * INetSock.sock_addr * Syslog.Conf.rule list -> unit
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

  fun outputFileFromPath path =
        let
          open Posix.FileSys
          val mode600 = S.flags [S.irusr, S.iwusr]
        in
          (* open path, create if it does not exist *)
          createf (path, O_WRONLY, O.flags [O.append, O.sync], mode600)
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

  fun start (path, addr, rules) =
        let
          infix |>
          fun (x |> f) = f x
          val actions = uniq (map #2 rules)
          val actionToCh = actions |> map (fn action =>
            let
              val ch = CML.channel ()
              val writer =
                case action of
                     Syslog.Conf.File fileName =>
                       let
                         val fd = outputFileFromPath fileName
                         fun writer () =
                               let
                                 val message = CML.recv ch
                                 val string = Syslog.Message.toString message ^ "\n"
                                 val vec = Word8VectorSlice.full (Byte.stringToBytes string)
                                 val writtenBytes = Posix.IO.writeVec (fd, vec)
                               in
                                 writer ()
                               end
                       in
                         writer
                       end
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
                                 NONE =>(Syslog.User, Syslog.Info)
                               | SOME pri => pri
                in
                  Syslog.Conf.app (sendToCh message) rules pri;
                  receiveLoop sock
                end
          val localSock = socketFromPath path
          val remoteSock = socketFromAddr addr
          val receiveLocal = CML.spawn (fn () => receiveLoop localSock)
          val receiveRemote = CML.spawn (fn () => receiveLoop remoteSock)
          val syslogInfo = (Syslog.Syslog, Syslog.Info)
          val startMessage = (SOME syslogInfo, NONE, "syslogd started")
        in
          Syslog.Conf.app (sendToCh startMessage) rules syslogInfo
        end
end

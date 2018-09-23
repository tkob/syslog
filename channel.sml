(* rendezvous channel *)
structure Channel :> sig 

  type 'a chan

  val chan : unit -> 'a chan
  val recv : 'a chan -> ('a -> unit) -> unit
  val send : 'a chan -> 'a -> (unit -> unit) -> unit

end = struct
  datatype 'a state = None | Sender of 'a * (unit -> unit) | Receiver of 'a -> unit
  type 'a chan = 'a state ref

  fun chan () = ref None

  fun recv chan proc =
        case !chan of
             None => chan := Receiver proc
           | Sender (msg, k) => (chan := None; proc msg; k ())
           | Receiver _ => raise Fail "recv: channel already in use"

  fun send chan msg k =
        case !chan of
             None => chan := Sender (msg, k)
           | Sender _ => raise Fail "send: channel already in use"
           | Receiver proc => (chan := None; proc msg; k ())
end

module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  open Type
  open ContactBufferType
  type t = ContactBufferType.t

  let size = 500

  let make stamp splice =
  let rec buffer =
    {
      stamp ;
      next = buffer ;
      num_contacts = 0 ;
      contacts = Array.init size (fun _ -> CpContact.make ())
    }
  in
  (match splice with
    | Some spl -> buffer.next <- spl.next
    | None ->  () ) ;
  buffer

let init buffer stamp splice =
  buffer.stamp <- stamp ;
  buffer.next <- (match splice with 
    | Some spl -> spl.next
    | None -> buffer) ;
  buffer.num_contacts <- 0 ;
  buffer

let push_fresh space =
  let stamp = space.spstamp in
  (match space.spcontact_buffers_head with
    | None -> space.spcontact_buffers_head <- Some (make stamp None)
    | Some head -> begin
      if stamp - head.next.stamp > space.spcollision_persistence
      then let tail = head.next in
           space.spcontact_buffers_head <- Some (init tail stamp (Some tail))
      else begin
        let buffer = make stamp (Some head) in
        head.next <- buffer ; space.spcontact_buffers_head <- Some buffer
      end
    end)

let get_array space =
  let head = CpOption.(!?(space.spcontact_buffers_head)) in
  if (head.num_contacts + max_contacts_per_arbiter) > size
  then push_fresh space ;

  (* spcontact_buffer_head may have changed *)
  let head = CpOption.(!?(space.spcontact_buffers_head)) in 
  CpArray.make head.contacts head.num_contacts

let push_contacts space count =
  assert (count <= max_contacts_per_arbiter) ;
  let head = CpOption.(!?(space.spcontact_buffers_head)) in
  head.num_contacts <- head.num_contacts + count

let pop_contacts space count =
  let head = CpOption.(!?(space.spcontact_buffers_head)) in
  head.num_contacts <- head.num_contacts - count
end

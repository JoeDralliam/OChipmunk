module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module Private = CpPrivate.Make(Param)
  open Type

  let for_body arb body =
    if Private.Body.eql arb.abody_a body then arb.athread_a else arb.athread_b
end


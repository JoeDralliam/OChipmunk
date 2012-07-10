open CpType

let for_body arb body =
  if CpPrivate.Body.eql arb.abody_a body then arb.athread_a else arb.athread_b



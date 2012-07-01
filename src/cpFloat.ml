
let clamp f min' max' = min (max f min') max'

let clamp01 f = max 0. (min f 1.)

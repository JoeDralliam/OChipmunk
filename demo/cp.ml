module NoData = 
struct
  type body_data = unit
  type space_data = unit
  type shape_data = unit
  type constraint_data = unit
end

module CpLib = OChipmunk.Make(NoData)
include CpLib

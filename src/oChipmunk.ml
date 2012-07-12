module Make = functor (Param : CpType.UserData) ->
struct
  module Arbiter = CpArbiter.Make(Param)
  module BB = CpBB
  module Body = CpBody.Make(Param)
  module Contact = CpContact
  module NearestPointQueryInfo = CpNearestPointQueryInfo.Make(Param)
  module SegmentQueryInfo = CpSegmentQueryInfo.Make(Param)
  module Shape = CpShape.Make(Param)
  module ShapeType = CpShapeType
  module Space = CpSpace.Make(Param)
  module Type = CpType.Make(Param)
  module Vector = CpVector


  module Constraint = CpConstraint.Make(Param)
  module ConstraintUtils = CpConstraintUtils.Make(Param)
  module DampedRotarySpring = CpDampedRotarySpring.Make(Param)
  module DampedSpring = CpDampedSpring.Make(Param)
  module GearJoint = CpGearJoint.Make(Param)
  module GrooveJoint = CpGrooveJoint.Make(Param)
  module PinJoint = CpPinJoint.Make(Param)
  module PivotJoint = CpPivotJoint.Make(Param)
  module RatchetJoint = CpRatchetJoint.Make(Param)
  module RotaryLimitJoint = CpRotaryLimitJoint.Make(Param)
  module SimpleMotor = CpSimpleMotor.Make(Param)
  module SlideJoint = CpSlideJoint.Make(Param)
end

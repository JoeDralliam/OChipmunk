module CircleImpl =
struct
  type t =
      {
        c:CpVector.t ;
        mutable tc:CpVector.t ;
        r:float
      }
end

module SegmentImpl =
struct
  type t = 
      {
        a:CpVector.t ;
        b:CpVector.t ;
        n:CpVector.t ;

        mutable ta:CpVector.t ;
        mutable tb:CpVector.t ;
        mutable tn:CpVector.t ;

        r:float ;

        mutable a_tangent:CpVector.t ;
        mutable b_tangent:CpVector.t
      }

end

module PolyShapeImpl =
struct
  type t =
      {
        verts:CpVector.t array ;
        t_verts:CpVector.t array ;

        planes:CpSplittingPlane.t array;
        t_planes:CpSplittingPlane.t array
      }
end

type t =
    Circle of CircleImpl.t
  | Segment of SegmentImpl.t
  | Poly of PolyShapeImpl.t

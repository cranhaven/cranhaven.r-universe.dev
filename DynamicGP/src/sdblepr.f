      subroutine sdblepr(label, nchar, scalar)
      integer nchar
      character*(*) label
      double precision scalar, arr(1)
      arr(1) = scalar
      call dblepr(label, nchar, arr, 1)
      end

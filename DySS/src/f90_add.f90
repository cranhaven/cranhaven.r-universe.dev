subroutine f90_add(a, b, ret)
  implicit none
  double precision, intent(in) :: a, b
  double precision, intent(out) :: ret
  ret = a + b
end subroutine

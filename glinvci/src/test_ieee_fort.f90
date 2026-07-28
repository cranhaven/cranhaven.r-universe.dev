module test
  use, intrinsic :: iso_c_binding
  implicit none
contains
  subroutine glinvtestfloatIEEE01(x,out) bind(C, name='glinvtestfloatIEEE01_')
    real(c_double) x, out
    out=exp(x)
  end subroutine
end module

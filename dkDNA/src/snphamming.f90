subroutine snphamming(n, x, y, theta, k)
  integer, intent(in):: n, x(n), y(n)
  double precision, intent(in) :: theta
  integer :: d
  double precision :: t5, t6, k

  d = count(x /= y)
  t5 = 1 - exp(-3*theta)
  t6 = 1 + 2*exp(-3*theta)
  k = exp(d * log(t5/t6))

end subroutine

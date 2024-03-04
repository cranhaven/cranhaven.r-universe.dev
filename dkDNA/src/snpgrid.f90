subroutine snpgrid(n, x, y, theta, k)
  integer, intent(in):: n, x(n), y(n)
  double precision, intent(in) :: theta
  integer :: m11tmp(n), m11, ktmp(n), n1, n2
  double precision :: t1, t2, t3, t4, k
  m11tmp = x*y
  m11 = count(m11tmp == 1)
  ktmp = abs(x-y)
  n1 = count(ktmp == 1)
  n2 = count(ktmp == 2)
  
  t1 = -2*exp(-3*theta) + 2
  t2 = exp(-3*theta) + 3*exp(-theta) +2 
  t3 = 4*exp(-3*theta) + 2
  t4 = exp(-3*theta) - 3*exp(-theta) +2
  k = exp( (n1*log(t1/t2)) +  ((m11)*log(t3/t2)) + (n2*log(t4/t2)) )
end subroutine



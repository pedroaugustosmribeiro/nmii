program test
  use values
  use kalman_filter
  implicit none
  integer :: m(2,2),v(2)
  real(rk),dimension(3) :: t,x
  real :: a
  m=reshape([1,2,3,4],shape(m))
  v=[5,6]
  print *,m(1,1),m(1,2)
  print *,m(2,1),m(2,2)
  print *
  print *,matmul(m,v)
  t=real([1,2,3],rk)
  x=real([4,5,6],rk)
  print '(3g)',transpose(cross(t,x))
end program test

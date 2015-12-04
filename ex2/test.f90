program test
  implicit none
  integer :: m(2,2),v(2)
  m=reshape([1,0,1,0],shape(m))
  v=[3,4]
  print *,m(1,1),m(1,2)
  print *,m(2,1),m(2,2)
  print *
  print *,matmul(m,v)
end program test

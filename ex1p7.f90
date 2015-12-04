program ex1p7
  use cmd_line
  implicit none
  real(rk),parameter :: a=0.01,c=0.0001
  real(rk) :: T1,T2,sig1,sig2,J,T
  integer :: i
  T1=-2
  sig1=2
  T2=3
  sig2=3
  i=0
  T=T1
  open(1,file='min.txt',status='new',action='write')
  do
     if (abs(a*jlinha(T,T1,T2,sig1,sig2))>c) then
        T=T-a*jlinha(T,T1,T2,sig1,sig2)
        i=i+1
        write(1,*) i,abs(jlinha(T,T1,T2,sig1,sig2)),cost(T,T1,sig1,T2,sig2)
     else
        exit
     end if
  end do
  close(1)
contains
  real(rk) function jlinha(T,T1,T2,sig1,sig2)
    implicit none
    real(rk),intent(in) :: T,T1,T2,sig1,sig2
    jlinha=(((T-T1)/sig1)+((T-T2)/sig2))
    return
  end function jlinha
  real(rk) function cost(x,x1,sig1,x2,sig2)
    implicit none
    real(rk),intent(in) :: x,x1,sig1,x2,sig2
    cost=0.5_rk*(((x-x1)**2/sig1)+((x-x2)**2/sig2))
    return
  end function cost
end program ex1p7

program ex1p5
  use cmd_line
  implicit none
  integer,parameter :: prec=10
  real(rk) :: T1,sig1,T2,sig2
  real(rk),allocatable :: y(:)
  integer :: x,a,sz
  character(len=120) :: filename
  a=cmd2int(1)
  T1=cmd2real(2)
  sig1=cmd2real(3)
  T2=cmd2real(4)
  sig2=cmd2real(5)
  if ((a==-1).or.(T1==-1).or.(sig1==-1).or.(T2==-1).or.(sig2==-1)) then
     print *,'Usage: cost interval mean1 variance1 mean2 variance2 filename'
     stop
  end if
  call get_command_argument(6,filename)
  open(1,file=filename,status='new',action='write')
  sz=(2*prec*a+1)
  write(1,'(i0)') sz
  allocate(y(sz))
  y=[(real(x,rk)/real(prec,rk),x=-a*prec,a*prec)]
  write(1,'(g0)') y
  write(1,'(g0)') [(cost((real(x,rk)/real(prec,rk)),T1,sig1,T2,sig2),x=-a*prec,a*prec)]
  close(1)
contains
  real(rk) function cost(x,x1,sig1,x2,sig2)
    implicit none
    real(rk),intent(in) :: x,x1,sig1,x2,sig2
    cost=0.5_rk*(((x-x1)**2/sig1)+((x-x2)**2/sig2))
    return
  end function cost
end program ex1p5

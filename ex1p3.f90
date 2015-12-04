program ex1p3
  use cmd_line
  implicit none
  integer,parameter :: prec=10
  !prec is precision
  real(rk) :: mean,variance
  real(rk),allocatable :: y(:)
  integer :: x,a,sz
  character(len=120) :: filename
  !  print *,'Enter the interval number a[-a,a], mean and std deviation: '
  !  read *,a,mean,stddev
  a=cmd2int(1)
  mean=cmd2real(2)
  variance=cmd2real(3)
  if ((a==-1).or.(mean==-1).or.(variance==-1)) then
     print *,'Usage: gauss interval mean variance filename'
     stop
  end if
  call get_command_argument(4,filename)
  open(1,file=filename,status='new',action='write')
  sz=(2*prec*a+1)
  write(1,'(i0)') sz
  allocate(y(sz))
  y=[(real(x,rk)/real(prec,rk),x=-a*prec,a*prec)]
  write(1,'(g0)') y
  write(1,'(g0)') [(gauss((real(x,rk)/real(prec,rk)),mean,sqrt(variance)),x=-a*prec,a*prec)]
  close(1)
contains
  real(rk) function gauss(x,mi,sigma)
    implicit none
    integer,parameter :: rk=selected_real_kind(10,20)
    real(rk),parameter :: pi=2_rk*acos(.0_rk)
    real(rk),intent(in) ::  x,mi,sigma
    gauss=exp((-1_rk*(x-mi)**2)/(2_rk*sigma**2))/(sigma*sqrt(2_rk*pi))
    return
  end function gauss
end program ex1p3

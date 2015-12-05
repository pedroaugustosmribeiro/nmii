program task2p5d
  use lin_model
  use synth_obs
  use kalman_filter
  use stat
  implicit none
  integer :: i,years,readline
  real(rk) :: w(3),y,B(3,3),H(3),d(3),xb(3)
  years=10
  xa=real([0,1,0],rk)
  H=real([1,1,0],rk)
  A=reshape(real([10,0,0,0,10,0,0,0,10],rk),shape(A))

  do i=1,12*years
     print *,i
     print *
     y=obs(xa)
     B=err_model(A)
     print *,'y= ',y
     print *
     w=[N_random(0.0_rk,0.05_rk),N_random(0.0_rk,0.01_rk),N_random(0.0_rk,0.01_rk)]
     print *,'w= ',w
     print *
     xb=model(xa)
     !xb=model(xa)+w !with noise c
     print *,'xb = ',xb
     print *

     print *,'B= '
     print '(3(g0,x))',transpose(B)
     print *
     call analysis(xb,B,H,y)
     print *,'By= ',By
     print *,'K= ',K
     print *,'A= '
     print '(3(g0,x))',transpose(A)
     print *
     print *,'xa= ',xa
     d=xa-xb
     !print *,i,d
     !print *,i,K
     !print *
     read *,readline
     if (readline==-1) then
        stop
     end if

  end do
end program task2p5d

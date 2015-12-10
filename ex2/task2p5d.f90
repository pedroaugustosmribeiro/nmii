program task2p5d
  use values
  use lin_model
  use synth_obs
  use kalman_filter
  use stat
  implicit none
  integer :: i,years,readline
  real(rk) :: w(3),y,B(3,3),H(3),d(3),xb(3),xa(3),A(3,3),K(3),By,x(3)
  years=10
  xa=real([0,1,0],rk)
  H=real([1,1,0],rk)
  K=real([0,0,0],rk)
  A=reshape(real([10,0,0,0,10,0,0,0,10],rk),shape(A))
  print *,rk
  do i=1,12*years

     y=obs(xa)
     !time evolution
     B=err_model(A)
     w=[N_random(0.0_rk,0.05_rk),N_random(0.0_rk,0.01_rk),N_random(0.0_rk,0.01_rk)]
     x=xb
     xb=model(xa)+w !with noise c
     !update step
     call analysis(xb,B,H,y,sig0,xa,A)
     d=xa-xb  !x-xb or xa-xb?
     write(6,'(i0,3(x,g0))'),i,d
  end do
end program task2p5d

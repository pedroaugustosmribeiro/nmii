program task2p3
  use lin_model
  use synth_obs
  implicit none
  real(rk) :: x(3),w(3),y
  integer :: i
  
  x=real([0,0,1],rk)
  do i=1,120
     y=obs(x)
     write(3,'(i,x,4(f,x))'),i,y,x(1)+x(2)
     w=[N_random(0.0_rk,0.05_rk),N_random(0.0_rk,0.01_rk),N_random(0.0_rk,0.01_rk)]
     !x=model(x) !no noise
     x=model(x)+w !with noise
  end do
end program task2p3

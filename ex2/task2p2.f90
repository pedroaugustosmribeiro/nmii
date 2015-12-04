program task2p2
  use lin_model
  real(rk) :: x(3),dx(3)
  integer :: i,k
  
  x=real([0,0,1],rk) !initial state
  dx=real([0.1,0.1,0.9],rk) !initial perturbation
  k=12*10 !12 observations/y for 10 years

  do i=0,k
     write(2,'(i,x,3(f,x))'),i,(x-dx)
     x=model(x)
     dx=model(dx)
  end do
  
end program task2p2

program task2p1
  use values
  use lin_model
  implicit none
  real(rk) :: x(3)
  integer :: i,k
  
  x=real([0,0,1],rk) !initial state
  k=12*10 !12 observations/y for 10 years

  do i=1,k
     write(1,'(i,x,3(f,x))'),i,x
     x=model(x)
  end do
  
end program task2p1

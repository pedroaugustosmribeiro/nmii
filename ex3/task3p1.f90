program task3p1
  use values
  use l63
  implicit none
  real(rk) :: x(3),dx(3),x2(3),dt
  integer :: i,k

  x=real([0,1,0],rk)
  k=1000
  dt=0.01_rk
  
  do i=1,k
     write(1,'(4(g,x))'),i*dt,x
     dx=model(x) !calculate x'i
     x=x+dx*dt !calculate xi+1
     dx=model(x) !calculate x'i+1
     x2=x+dx*dt !calculate xi+2
     x=0.5_rk*(x+x2) !calculate xi+1
  end do

end program task3p1

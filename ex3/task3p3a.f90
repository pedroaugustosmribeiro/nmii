program task3p3a
  use values
  use l63
  implicit none
  real(rk) :: x(3),dx(3),x2(3),dt,xp(3),dxp(3),xp2(3) !xp: pertubated x
  integer :: i,k

  x=real([11,22,10],rk)
  !x=real([11,22,10]+[1,1,1],rk)
  xp=real([1,1,1],rk)
  k=100
  dt=0.01_rk
  
  do i=1,k
     write(2,*),i*dt,x/xp
     dx=model(x) !calculate x'i
     dxp=err_model(x,xp)
     x=x+dx*dt !calculate xi+1
     xp=xp+dxp*dt
     dx=model(x) !calculate x'i+1
     dxp=err_model(x,xp)
     x2=x+dx*dt !calculate xi+2
     xp2=xp+dxp*dt
     x=0.5_rk*(x+x2) !calculate xi+1
     xp=0.5_rk*(xp+xp2)
  end do

end program task3p3a

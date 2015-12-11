program task3p5
  use values
  use l63
  use synth_obs
  use kalman_filter
  implicit none
  real(rk) :: M(3,3) !xp: pertubated x
  real(rk) :: y,dt
  real(rk),dimension(3) :: xb,H,xa,dx,xp,dxp,xp2
  real(rk),dimension(3,3) :: B,A,W

  integer :: i,j,k

  xa=real([11,22,10],rk)
  xp=real([1,1,1],rk)
  !xa=xa-(xp/1.0_rk)
  k=100
  dt=0.01_rk

  xa=real([0,0,1],rk)
  H=real([1,1,1],rk)
  A=reshape(real([10,0,0,0,10,0,0,0,10],rk),shape(A))
  W=reshape(real([1,0,0,0,1,0,0,0,1],rk),shape(W))
  xb=xa
  j=0
  do i=1,k
     dx=model(xb) !calculate x'i
     call err_model(xb,xp,dxp,M)
     xb=xb+model(xb)*dt !calculate xi+1
     xp=xp+dxp*dt
     dx=model(xb) !calculate x'i+1
     call err_model(xb,xp,dxp,M)
     !calculate xi+2
     xp2=xp+dxp*dt
     xb=0.5_rk*(2*xb+model(xb)*dt) !calculate xi+1
     xp=0.5_rk*(xp+xp2)
     M=reshape([-sigma,(rho-xb(3)),xb(2),sigma,-1.0_rk,xb(1),.0_rk,-xb(1),-beta],shape(M))
     B=matmul(matmul(M,A),transpose(M))
     j=j+1
     if (j==10) then
        call analysis(xb,B+W,H,y,sig0,xa,A)
        xb=xa
        j=0
        write(6,'(i,x,3(g,x))'),i,xa
     end if
  end do

end program task3p5

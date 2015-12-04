program task2p4
  use lin_model
  use synth_obs
  use kalman_filter
  use stat
  implicit none
  integer :: i,years
  real(rk) :: xb(3),w(3),y,B(3,3),H(3)
  real(rk),allocatable :: d(:),r(:),dn(:)
  character(len=20) :: dl='inovations',rl='residuals',dnl='norm inovations'
  years=10
  xa=real([0,0,1],rk)
  H=real([1,1,0],rk)
  A=reshape(real([10,0,0,0,10,0,0,0,10],rk),shape(A))
  allocate(d(12*years),r(12*years),dn(12*years))
  
  do i=1,12*years
     y=obs(xa)
     w=[N_random(0.0_rk,0.05_rk),N_random(0.0_rk,0.01_rk),N_random(0.0_rk,0.01_rk)]
     xb=model(xa)+w !with noise
     B=err_model(A)
     call analysis(xb,B,H,y)
     d(i)=y-dot_product(H,xb) !inovations
     r(i)=y-dot_product(H,xa) !residuals
     dn(i)=d(i)/sqrt(By)
     !     write(1,'(i,x,2(f,x))'),i,y,dot_product(H,xa)
     !print *,i,y,dot_product(H,xb),dot_product(H,xa),d,r
     !print *,i,d(i),dn(i)!,r(i)
     !write(7,*),i,sum([(A(i,i),i=1,3)]),sum([(B(i,i),i=1,3)])
  end do
  call statistics(d,dl)
  call statistics(dn,dnl)
end program task2p4
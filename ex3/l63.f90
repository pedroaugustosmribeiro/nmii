module l63
  use values
  implicit none
  real(rk),parameter :: sigma=10.0_rk,rho=28.0_rk,beta=8.0_rk/3.0_rk
contains

    subroutine model(x,dx)
    implicit none
    real(rk),intent(in) :: x(3)
    real(rk),intent(out) :: dx(3)!,sigma,rho,beta
    real(rk) :: x2(3)
    !real(rk) :: model(3)

    !sigma=10.0_rk
    !rho=28.0_rk
    !beta=(8.0_rk/3.0_rk)

    dx(1)=sigma*(x(2)-x(1))
    dx(2)=x(1)*(rho-x(3))-x(2)
    dx(3)=x(1)*x(2)-beta*x(3)
    !model=dx
  end subroutine model


   subroutine err_model(x,xp,dxp,sigma,rho,beta)
    implicit none
    real(rk),intent(in) :: x(3),xp(3),sigma,rho,beta
    real(rk) :: M(3,3)
    real(rk),intent(out) :: dxp(3)
    !real(rk) :: err_model(3)

    M=reshape([-sigma,rho-x(3),x(2),sigma,-1.0_rk,x(1),.0_rk,-x(1),-beta],shape(M))
    dxp=matmul(M,xp)
  end subroutine err_model
  
  end module l63


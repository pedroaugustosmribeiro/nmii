module l63
  use values
  implicit none
  real(rk),parameter :: sigma=10.0_rk,rho=28.0_rk,beta=8.0_rk/3.0_rk
contains

  pure  function model(x)
    implicit none
    real(rk),intent(in),dimension(3) :: x
    real(rk) :: dx(3)
    real(rk) :: model(3)

    dx(1)=sigma*(x(2)-x(1))
    dx(2)=x(1)*(rho-x(3))-x(2)
    dx(3)=x(1)*x(2)-beta*x(3)
    model=dx
  end function model

  pure function err_model(x,xp)
    implicit none
    real(rk),intent(in),dimension(3) :: x,xp
    real(rk) :: M(3,3)
    real(rk) :: err_model(3)

    M=reshape([-sigma,rho-x(3),x(2),sigma,-1.0_rk,x(1),.0_rk,-x(1),-beta],shape(M))
    err_model=matmul(M,xp)
  end function err_model

end module l63


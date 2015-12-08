module l63
  use values
  implicit none
  real(rk),parameter :: sigma=10.0_rk,rho=28.0_rk,beta=8.0_rk/3.0_rk
contains

  pure function model(x)
    implicit none
    real(rk),intent(in) :: x(3)
    real(rk) :: dx(3)!,sigma,rho,beta
    real(rk) :: model(3)

    !sigma=10.0_rk
    !rho=28.0_rk
    !beta=(8.0_rk/3.0_rk)

    dx(1)=sigma*(x(2)-x(1))
    dx(2)=x(1)*(rho-x(3))-x(2)
    dx(3)=x(1)*x(2)-beta*x(3)
    model=dx
  end function model

  pure function err_model(x)
    implicit none
    real(rk),intent(in) :: x(3)
    real(rk) :: M(3,3),delta(3),err_model(3)!,sigma,rho,beta
    !real(rk) :: err_model(3)

    delta=real([1,1,1],rk)
    M=reshape([-sigma,rho-x(3),x(2),sigma,-1.0_rk,x(1),.0_rk,-x(1),-beta],shape(M))
    err_model=matmul(M,x)
  end function err_model
  
end module l63


module l63
  use values
  implicit none
contains

  function model(x)
    implicit none
    real(rk) :: model(3),dx(3),sigma,rho,beta
    real(rk),intent(in) :: x(3)
    sigma=10.0_rk
    rho=28.0_rk
    beta=(8.0_rk/3.0_rk)
    
    dx(1)=sigma*(x(2)-x(1))
    dx(2)=x(1)*(rho-x(3))-x(2)
    dx(3)=x(1)*x(2)-beta*x(3)
    model=dx
  end function model

end module l63


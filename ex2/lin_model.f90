module lin_model
  use values
  implicit none
  real(rk),parameter :: c=cos(pi/(6.0_rk)),s=sin(pi/(6.0_rk)) 
contains

  pure function model(x)
    implicit none
    real(rk),intent(in) :: x(3)
    real(rk) :: M(3,3),model(3)

    M=reshape([1.0_rk,0.0_rk,0.0_rk,0.0_rk,c,-s,0.0_rk,s,c],shape(M))
    model=matmul(M,x)
  end function model

  pure function err_model(A)
    implicit none
    real(rk) :: M(3,3),W(3,3)
    real(rk),intent(in) :: A(3,3)
    real(rk) :: err_model(3,3)

    M=reshape([1.0_rk,0.0_rk,0.0_rk,0.0_rk,c,-s,0.0_rk,s,c],shape(M))
    W=reshape([0.05_rk**2,0.0_rk,0.0_rk,0.0_rk,0.01_rk,0.0_rk,0.0_rk,0.0_rk,0.01_rk],shape(W))
    err_model=matmul(matmul(M,A),transpose(M))+W
  end function err_model

end module lin_model

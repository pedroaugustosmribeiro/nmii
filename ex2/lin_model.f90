module lin_model
  use values
  implicit none
  real(rk),parameter :: c=cos(pi/(6.0_rk)),s=sin(pi/(6.0_rk)) 
contains

  function model(x)
    implicit none
    real(rk),intent(in) :: x(3)
    real(rk) :: M(3,3),model(3)

    M=reshape([1.0_rk,0.0_rk,0.0_rk,0.0_rk,c,-s,0.0_rk,s,c],shape(M))
    !  M=reshape(real([1,0,0,0,1,0,0,0,1],rk),shape(M)) !testing that it works
    model=matmul(M,x)
  end function model
  
  function err_model(A)
    implicit none
    real(rk) :: M(3,3),W(3,3)
    real(rk),intent(in) :: A(3,3)
    real(rk) :: err_model(3,3)

    M=reshape([1.0_rk,0.0_rk,0.0_rk,0.0_rk,c,-s,0.0_rk,s,c],shape(M))
    print *,'Ma= '
    print *,M(1,1),M(1,2),M(1,3)
    print *,M(2,1),M(2,2),M(2,3)
    print *,M(3,1),M(3,2),M(3,3)
    print '(3g)',transpose(M)
    W=reshape([0.05_rk**2,0.0_rk,0.0_rk,0.0_rk,0.01_rk,0.0_rk,0.0_rk,0.0_rk,0.01_rk],shape(W))
    err_model=matmul(matmul(M,A),transpose(M))+W
  end function err_model

end module lin_model

module kalman_filter
  use values
  implicit none
  real(rk) :: By,K(3),xa(3),A(3,3)
contains
  subroutine analysis(xb,B,H,y)
    implicit none
    real(rk),intent(in) :: xb(3),B(3,3),H(3),y

    By=dot_product(matmul(H,B),H)+sig0
    K=matmul(B,H)/By
    A= B-(dot_product(K,H)*B)
    xa=xb+K*(y-dot_product(H,xb))
  end subroutine analysis

end module kalman_filter

module kalman_filter
  use values
  implicit none
contains
  
  pure subroutine analysis(xb,B,H,y,sig0,xa,A)
    implicit none
    real(rk),intent(in) :: xb(3),B(3,3),H(3),y,sig0
    real(rk),intent(out) :: A(3,3),xa(3)
    real(rk) :: K(3),By

    By=(dot_product(H,matmul(B,H)))+(sig0**2)
    K=matmul(B,H)/By
    A=B-matmul(cross(K,H),B)
    xa=xb+K*(y-dot_product(H,xb))

  end subroutine analysis

end module kalman_filter

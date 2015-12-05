module kalman_filter
  use values
  implicit none
contains
  subroutine analysis(xb,B,H,y,sig0,xa,A)
    implicit none
    real(rk),intent(in) :: xb(3),B(3,3),H(3),y,sig0
    real(rk),intent(out) :: A(3,3),xa(3)
    real(rk) :: K(3),By

    By=(dot_product(matmul(H,B),H))+(sig0**2) !!is wright

    K=matmul(B,(H/By)) !ahaaaaaaaaaaan, Houston we have a problem!!

    !K=(B(1,1)+B(1,2)+B(2,1)+B(2,2))
    A= B*(1-dot_product(K,H))
    !print *,(y-dot_product(H,xb)),xb
    xa=xb+K*(y-dot_product(H,xb))

    !print *,K!,By
    
  end subroutine analysis

end module kalman_filter

module kalman_filter
  use values
  implicit none
contains
  pure subroutine analysis(xb,B,H,y,sig0,xa,A)
    implicit none
    real(rk),intent(in) :: xb(3),B(3,3),H(3),y,sig0
    real(rk),intent(out) :: A(3,3),xa(3)
    real(rk) :: K(3),By

    !!is wright
    By=(dot_product(H,matmul(B,H)))+(sig0**2)
    !ahaaaaaaaaaaan, Houston we have a problem!!
    K=matmul(B,H)/By
    A=B-matmul(cross(K,H),B)
    !print *,(y-dot_product(H,xb)),xb
    xa=xb+K*(y-dot_product(H,xb))

    !print *,K!,By

  end subroutine analysis

  pure function cross(u,v)
    implicit none
    real(rk),intent(in) :: u(:),v(:)
    real(rk),allocatable :: cross(:,:)
    integer :: i,j
    allocate(cross(size(u),size(v)))
    cross=reshape([((v(i)*u,i=1,size(u)),j=1,size(v))],shape(cross))
  end function cross
  

end module kalman_filter

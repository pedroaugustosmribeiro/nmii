module values
  implicit none
  !important constants
  integer,parameter :: rk=selected_real_kind(15,307) !select type of real
  real(rk),parameter :: pi=4.0_rk*atan(1.0_rk),sig0=sqrt(1.4_rk)

contains
  !general use functions and subroutines
  real(rk) function N_random(mi,sig)
    implicit none
    real(rk),intent(in) :: mi,sig !parameters of normal distribution
    real(rk) :: r
    call random_number(r)
    N_random=mi+sig*(sqrt(-2*log(r))*cos(2*pi*r))
  end function N_random

  pure function cross(u,v)
    implicit none
    real(rk),intent(in) :: u(:),v(:)
    real(rk),allocatable :: cross(:,:)
    integer :: i,j
    allocate(cross(size(u),size(v)))
    cross=reshape([((v(i)*u,i=1,size(u)),j=1,size(v))],shape(cross))
  end function cross
  
end module values

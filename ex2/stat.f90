module stat
  use values
  implicit none
contains
  
  subroutine statistics(e,c4)
    
    real(rk),intent(in) :: e(:)        !! input vector e with dimension n
    character(len=20),intent(in) :: c4   !! a label of four characters
    write(6,*) "--- Statistics --- ", c4
    write(6,*) "mean ",sum(e)/real(size(e),rk)
    write(6,*) "sigma",sqrt(dot_product(e,e)/real(size(e),rk))
    write(6,*) "min/max",minval(e),maxval(e),"at",minloc(e),maxloc(e)

  end subroutine statistics

end module stat

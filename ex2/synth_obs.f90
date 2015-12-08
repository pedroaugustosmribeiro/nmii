module synth_obs
  use values
  implicit none

contains
  real(rk) function obs(x)
    implicit none
    real(rk),intent(in) :: x(3)
    real(rk) :: H(3),v

    H=real([1,1,0],rk)
    v=N_random(0.0_rk,sig0)
    obs=dot_product(H,x)+v
  end function obs

end module synth_obs

module cmd_line

  implicit none
  integer,parameter :: rk=selected_real_kind(15,307)
  integer,parameter :: MAXBUF=200
  character(len=MAXBUF),private :: argu

contains

  ! 
  ! Define here functions 
  ! 
  ! cmd2real(i) 
  ! of type real(rk) that reads the ith command line
  ! argument, converts it to real(rk) and returns this value
  !
  ! cmd2int(i) 
  ! of type integer that reads the ith command line
  ! argument, converts it to integer and returns this value
  ! 
  ! Note that constant 'rk' and variable 'argu' can be used here. Use
  ! 'internal io' to do the conversion.
  !

  real(kind=rk) function cmd2real(i)
    implicit none
    integer,intent(in) :: i
    integer :: ios
    call get_command_argument(i,argu)
    read(argu,*,iostat=ios),cmd2real
    if (ios/=0) then
       cmd2real=-1
       return
    else
       return
    end if
  end function cmd2real
  
  integer function cmd2int(i)
    implicit none
    integer,intent(in) :: i
    integer :: ios
    call get_command_argument(i,argu)
    read(argu,*,iostat=ios),cmd2int
    if (ios/=0) then
       cmd2int=-1
       return
    else
       return
    end if
  end function cmd2int
  
  
end module cmd_line

subroutine mixingMatrix23(U23,t23)
    implicit none
    real(8) :: U23(3,3)    ! U23 is the rotation matrix between states 2 and 3
    real(8) :: t23                ! t23 is the mixing angle theta_23

    real(8) :: c23                ! c23 is the cosine of t23
    real(8) :: s23                ! s23 is the sine of t23

    c23=cos(t23)
    s23=sin(t23)

    U23(1,1)=1.0d0;  U23(1,2)=0.0d0;  U23(1,3)=0.0d0;
    U23(2,1)=0.0d0;  U23(2,2)=c23;    U23(2,3)=s23;
    U23(3,1)=0.0d0;  U23(3,2)=-s23;   U23(3,3)=c23;
    return
end subroutine mixingMatrix23
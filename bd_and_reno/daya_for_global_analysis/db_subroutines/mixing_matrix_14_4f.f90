subroutine mixingMatrix14_4f(U14,t14)
    implicit none
    real(8) :: U14(4,4)           ! U14 is the rotation matrix between states 2 and 3
    real(8) :: t14                ! t14 is the mixing angle theta_23

    real(8) :: c14                ! c14 is the cosine of t14
    real(8) :: s14                ! s14 is the sine of t14

    c14=cos(t14)
    s14=sin(t14)

    U14(1,1)=c14;    U14(1,2)=0.0d0;   U14(1,3)=0.0d0;  U14(1,4)=s14;
    U14(2,1)=0.0d0;  U14(2,2)=1.0d0;   U14(2,3)=0.0d0;  U14(2,4)=0.0d0;
    U14(3,1)=0.0d0;  U14(3,2)=0.0d0;   U14(3,3)=1.0d0;  U14(3,4)=0.0d0;
    U14(4,1)=-s14;   U14(4,2)=0.0d0;   U14(4,3)=0.0d0;  U14(4,4)=c14;
    return
end subroutine mixingMatrix14_4f
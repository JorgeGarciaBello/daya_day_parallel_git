subroutine mixingMatrix24_4f(U24,t24,delta2)
    implicit none
    double complex :: U24(4,4)    ! U24 is the rotation matrix between states 2 and 3
    real(8) :: t24                ! t24 is the mixing angle theta_23
    real(8) :: delta2             ! delta2 is a factor of phase

    real(8) :: c24                ! c24 is the cosine of t24
    real(8) :: s24                ! s24 is the sine of t24

    c24=cos(t24)
    s24=sin(t24)

    U24(1,1)=1.0d0;  U24(1,2)=0.0d0;   U24(1,3)=0.0d0;                         U24(1,4)=0.0d0;
    U24(2,1)=0.0d0;  U24(2,2)=c24;     U24(2,3)=0.0d0;               U24(2,4)=s24*exp(complex(0.0d0,-delta2));
    U24(3,1)=0.0d0;  U24(3,2)=0.0d0;   U24(3,3)=1.0d0;                         U24(3,4)=0.0d0;
    U24(4,1)=0.0d0;  U24(4,2)=-s24*exp(complex(0.0d0,delta2)); U24(4,3)=0.0d0; U24(4,4)=c24;
    return
end subroutine mixingMatrix24_4f
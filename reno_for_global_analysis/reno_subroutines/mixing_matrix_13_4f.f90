subroutine mixingMatrix13_4f(U13,t13,delta1)
    implicit none
    double complex :: U13(4,4)    ! U13 is the rotation matrix between states 2 and 3
    real(8) :: t13                ! t13 is the mixing angle theta_23
    real(8) :: delta1             ! delta1 is the CP violation phase factor

    real(8) :: c13                ! c13 is the cosine of t13
    real(8) :: s13                ! s13 is the sine of t13

    c13=cos(t13)
    s13=sin(t13)

    U13(1,1)=c13;                           U13(1,2)=0.0d0; U13(1,3)=s13*exp(cmplx(0.0d0,-delta1));  U13(1,4)=0.0d0;
    U13(2,1)=0.0d0;                         U13(2,2)=1.0d0; U13(2,3)=0.0d0;                          U13(2,4)=0.0d0;
    U13(3,1)=-s13*exp(cmplx(0.0d0,delta1)); U13(3,2)=0.0d0; U13(3,3)=c13;                            U13(3,4)=0.0d0;
    U13(4,1)=0.0d0;                         U13(4,2)=0.0d0; U13(4,3)=0.0d0;                          U13(4,4)=1.0d0;
    return
end subroutine mixingMatrix13_4f
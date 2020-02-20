subroutine mixingMatrix34_4f(U34,t34,delta3)
    implicit none
    double complex :: U34(4,4)    ! U34 is the rotation matrix between states 2 and 3
    real(8) :: t34                ! t34 is the mixing angle theta_23
    real(8) :: delta3             ! delta3 is a factor of phase

    real(8) :: c34                ! c34 is the cosine of t34
    real(8) :: s34                ! s34 is the sine of t34

    c34=cos(t34)
    s34=sin(t34)

    U34(1,1)=1.0d0;  U34(1,2)=0.0d0;   U34(1,3)=0.0d0;                         U34(1,4)=0.0d0;
    U34(2,1)=0.0d0;  U34(2,2)=1.0d0;   U34(2,3)=0.0d0;                         U34(2,4)=0.0d0;
    U34(3,1)=0.0d0;  U34(3,2)=0.0d0;   U34(3,3)=c34;                           U34(3,4)=s34*exp(complex(0.0d0,-delta3));
    U34(4,1)=0.0d0;  U34(4,2)=0.0d0;   U34(4,3)=-s34*exp(complex(0.0d0,delta3));U34(4,4)=c34;
    return
end subroutine mixingMatrix34_4f
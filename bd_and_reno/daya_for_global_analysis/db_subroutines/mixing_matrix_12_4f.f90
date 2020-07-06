subroutine mixingMatrix12_4f(U12,t12)
    implicit none
    real(8) :: U12(4,4)           ! U12 is the rotation matrix between states 2 and 3
    real(8) :: t12                ! t12 is the mixing angle theta_23

    real(8) :: c12                ! c12 is the cosine of t12
    real(8) :: s12                ! s12 is the sine of t12

    c12=cos(t12)
    s12=sin(t12)

    U12(1,1)=c12;   U12(1,2)=s12;   U12(1,3)=0.0d0; U12(1,4)=0.0d0;
    U12(2,1)=-s12;  U12(2,2)=c12;   U12(2,3)=0.0d0; U12(2,4)=0.0d0;
    U12(3,1)=0.0d0; U12(3,2)=0.0d0; U12(3,3)=1.0d0; U12(3,4)=0.0d0;
    U12(4,1)=0.0d0; U12(4,2)=0.0d0; U12(4,3)=0.0d0; U12(4,4)=1.0d0;
    return
end subroutine mixingMatrix12_4f
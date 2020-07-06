subroutine mixingMatrix_4f(U,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3)
    implicit none
    double complex :: U(4,4)      ! U is the mixing matrix in the neutrino oscillation model
    real(8) :: t12,t13,t14,t23,t24,t34! Are the mixing angles
    real(8) :: delta1,delta2,delta3! delta is the CP violation phase factor

    real(8) :: U23(4,4)           ! U23 is the rotation matrix between states 2 and 3
    double complex :: U13(4,4)    ! U13 is the rotation matrix between states 1 and 3
    real(8) :: U12(4,4)           ! U12 is the rotation matrix between states 1 and 2
    real(8) :: U14(4,4)           ! U14 is the rotation matrix between states 1 and 4
    double complex :: U24(4,4)    ! U24 is the rotation matrix between states 2 and 4
    double complex :: U34(4,4)    ! U34 is the rotation matrix between states 3 and 4

    call mixingMatrix23_4f(U23,t23)
    call mixingMatrix13_4f(U13,t13,delta1)
    call mixingMatrix12_4f(U12,t12)
    call mixingMatrix14_4f(U14,t14)
    call mixingMatrix24_4f(U24,t24,delta2)
    call mixingMatrix34_4f(U34,t34,delta3)

    U=matmul(matmul(matmul(matmul(matmul(U23,U13),U12),U14),U24),U34)
    return
end subroutine mixingMatrix_4f
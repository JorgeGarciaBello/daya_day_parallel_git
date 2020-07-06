subroutine mixingMatrix(U,t12,t13,t23,delta)
    implicit none
    double complex :: U(3,3)      ! U is the mixing matrix in the neutrino oscillation model
    real(8) :: t12,t13,t23        ! Are the mixing angles
    real(8) :: delta              ! delta is the CP violation phase factor
    real(8) :: U12(3,3)           ! U12 is the rotation matrix between states 2 and 3
    double complex :: U13(3,3)    ! U13 is the rotation matrix between states 2 and 3
    real(8) :: U23(3,3)           ! U23 is the rotation matrix between states 2 and 3

    call mixingMatrix12(U12,t12)
    call mixingMatrix13(U13,t13,delta)
    call mixingMatrix23(U23,t23)

    U=matmul(U23,matmul(U13,U12))
    return
end subroutine mixingMatrix
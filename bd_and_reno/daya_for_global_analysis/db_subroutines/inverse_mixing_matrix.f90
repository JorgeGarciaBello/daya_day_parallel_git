subroutine inverseMixingMatrix(U_1,t12,t13,t23,delta)
    implicit none
    double complex :: U_1(3,3)    ! Inverse mixing matrix
    real(8) :: t12,t13,t23        ! Are the mixing angles
    real(8) :: delta              ! delta is the CP violation phase factor

    double complex :: U(3,3)      ! U is the mixing matrix in the neutrino oscillation model       

    call mixingMatrix(U,t12,t13,t23,delta)
    call inverseMatrix(U,U_1)

    return
end subroutine inverseMixingMatrix
subroutine inverseMixingMatrix_4f(U_1,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3)
    implicit none
    double complex :: U_1(4,4)    ! Inverse mixing matrix
    real(8) :: t12,t13,t14,t23,t24,t34! Are the mixing angles
    real(8) :: delta1,delta2,delta3! delta is the CP violation phase factor

    double complex :: U(4,4)      ! U is the mixing matrix in the neutrino oscillation model       

   
    call mixingMatrix_4f(U,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3)
    call inverseMatrix_4_4(U,U_1)

    return
end subroutine inverseMixingMatrix_4f
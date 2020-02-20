subroutine massState(state,mVector,t12,t13,t23,delta)
    implicit none
    integer :: state              ! state is the number of the mass state
    double complex :: mVector(3)  ! mVector is the vector of a stationary mass state
    real(8) :: t12,t13,t23        ! Are the mixing angles
    real(8) :: delta              ! delta is the CP violation phase factor    

    double complex :: U_1(3,3)    ! Inverse mixing matrix
    real(8) :: eVector(3)
    real(8) :: muVector(3)
    real(8) :: tauVector(3)

    call flavourState(1,eVector)
    call flavourState(2,muVector)
    call flavourState(3,tauVector)   
    call inverseMixingMatrix(U_1,t12,t13,t23,delta)    

    select case(state)
        case(1)
            mVector=U_1(1,1)*eVector+U_1(1,2)*muVector+U_1(1,3)*tauVector
        case(2)
            mVector=U_1(2,1)*eVector+U_1(2,2)*muVector+U_1(2,3)*tauVector
        case(3)
            mVector=U_1(3,1)*eVector+U_1(3,2)*muVector+U_1(3,3)*tauVector
        case DEFAULT
    end select
    return
end subroutine massState
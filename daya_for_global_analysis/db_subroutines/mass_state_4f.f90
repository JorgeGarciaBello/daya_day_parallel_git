subroutine massState_4f(state,mVector,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3)
    implicit none
    integer :: state              ! state is the number of the mass state
    double complex :: mVector(4)  ! mVector is the vector of a stationary mass state
    real(8) :: t12,t13,t14,t23,t24,t34! Are the mixing angles
    real(8) :: delta1,delta2,delta3! delta is the CP violation phase factor

    double complex :: U_1(4,4)    ! Inverse mixing matrix
    real(8) :: eVector(4)
    real(8) :: muVector(4)
    real(8) :: tauVector(4)
    real(8) :: strlVector(4)

    call flavourState_4f(1,eVector)
    call flavourState_4f(2,muVector)
    call flavourState_4f(3,tauVector)
    call flavourState_4f(4,strlVector)
    call inverseMixingMatrix_4f(U_1,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3)

    select case(state)
        case(1)
            mVector=U_1(1,1)*eVector+U_1(1,2)*muVector+U_1(1,3)*tauVector+U_1(1,4)*strlVector
        case(2)
            mVector=U_1(2,1)*eVector+U_1(2,2)*muVector+U_1(2,3)*tauVector+U_1(2,4)*strlVector
        case(3)
            mVector=U_1(3,1)*eVector+U_1(3,2)*muVector+U_1(3,3)*tauVector+U_1(3,4)*strlVector
        case(4)
            mVector=U_1(4,1)*eVector+U_1(4,2)*muVector+U_1(4,3)*tauVector+U_1(4,4)*strlVector
        case DEFAULT
            print*,'No existe la opción de estado de masa'
            print*,state
    end select
    return
end subroutine massState_4f
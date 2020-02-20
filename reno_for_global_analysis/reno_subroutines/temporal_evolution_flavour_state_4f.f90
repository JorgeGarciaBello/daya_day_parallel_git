subroutine temporalEvolutionFlavourState_4f(fState,temporalflavourVector,                 &
                                            t12,t13,t14,t23,t24,t34,delta1,delta2,delta3, &
                                            dm21,dm31,dm41,L,P)
    implicit none
    integer :: fState             ! fState is the number of the flavour state
    double complex :: temporalflavourVector(4)! temporalflavourVector is the vector of the flavour state
    real(8) :: t12,t13,t14,t23,t24,t34! Are the mixing angles
    real(8) :: delta1,delta2,delta3! delta is the CP violation phase factor
    real(8) :: dm21,dm31,dm41     ! dm21,dm31,dm41 are the squared mass differences
    real(8) :: L                  ! L is the length between the source of neutrinos an the position
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total    

    double complex :: U(4,4)      ! U is the mixing matrix in the neutrino oscillation model
    double complex :: temporalMass1Vector(4)! temporalMass1Vector is the vector of the mass state
    double complex :: temporalMass2Vector(4)! temporalMass2Vector is the vector of the mass state
    double complex :: temporalMass3Vector(4)! temporalMass3Vector is the vector of the mass state
    double complex :: temporalMass4Vector(4)! temporalMass4Vector is the vector of the mass state
    
    call mixingMatrix_4f(U,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3)
    call temporalEvolutionMassState_4f(1,temporalMass1Vector,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3,dm21,dm31,dm41,L,P)
    call temporalEvolutionMassState_4f(2,temporalMass2Vector,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3,dm21,dm31,dm41,L,P)
    call temporalEvolutionMassState_4f(3,temporalMass3Vector,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3,dm21,dm31,dm41,L,P)
    call temporalEvolutionMassState_4f(4,temporalMass4Vector,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3,dm21,dm31,dm41,L,P)    
   
    select case(fState)
        case(1)
            temporalflavourVector= U(1,1)*temporalMass1Vector+U(1,2)*temporalMass2Vector &
                                  +U(1,3)*temporalMass3Vector+U(1,4)*temporalMass4Vector
        case(2)
            temporalflavourVector= U(2,1)*temporalMass1Vector+U(2,2)*temporalMass2Vector &
                                  +U(2,3)*temporalMass3Vector+U(2,4)*temporalMass4Vector
        case(3)
            temporalflavourVector= U(3,1)*temporalMass1Vector+U(3,2)*temporalMass2Vector &
                                  +U(3,3)*temporalMass3Vector+U(3,4)*temporalMass4Vector
        case(4)
            temporalflavourVector= U(4,1)*temporalMass1Vector+U(4,2)*temporalMass2Vector &
                                  +U(4,3)*temporalMass3Vector+U(4,4)*temporalMass4Vector
        case DEFAULT
        print*, 'Error: El estado de sabor evolucionado en el timepo no existe '
        print*, fState
    end select
    return
end subroutine temporalEvolutionFlavourState_4f
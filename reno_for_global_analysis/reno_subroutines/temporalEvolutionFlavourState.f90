subroutine temporalEvolutionFlavourState(fState,temporalflavourVector,t12,t13,t23,delta,sm,aM,L,P)
    implicit none
    integer :: fState             ! fState is the number of the flavour state
    double complex :: temporalflavourVector(3)! temporalflavourVector is the vector of the flavour state
    real(8) :: t12,t13,t23        ! Are the mixing angles
    real(8) :: delta              ! delta is the CP violation phase factor    
    real(8) :: sm,aM              ! sm,aM are the squared flavour difference m=m_21 y M=m_32
    real(8) :: L                  ! L is the length between the source of neutrinos an the position
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total    

    double complex :: U(3,3)      ! U is the mixing matrix in the neutrino oscillation model
    double complex :: temporalMass1Vector(3)! temporalMass1Vector is the vector of the mass state
    double complex :: temporalMass2Vector(3)! temporalMass2Vector is the vector of the mass state
    double complex :: temporalMass3Vector(3)! temporalMass3Vector is the vector of the mass state
    
    call mixingMatrix(U,t12,t13,t23,delta)
    call temporalEvolutionMassState(1,temporalMass1Vector,t12,t13,t23,delta,sm,aM,L,P)
    call temporalEvolutionMassState(2,temporalMass2Vector,t12,t13,t23,delta,sm,aM,L,P)
    call temporalEvolutionMassState(3,temporalMass3Vector,t12,t13,t23,delta,sm,aM,L,P)
   
    select case(fState)
        case(1)
            temporalflavourVector=U(1,1)*temporalMass1Vector+U(1,2)*temporalMass2Vector+U(1,3)*temporalMass3Vector
        case(2)
            temporalflavourVector=U(2,1)*temporalMass1Vector+U(2,2)*temporalMass2Vector+U(2,3)*temporalMass3Vector
        case(3)
            temporalflavourVector=U(3,1)*temporalMass1Vector+U(3,2)*temporalMass2Vector+U(3,3)*temporalMass3Vector
        case DEFAULT
        print*, 'Error: El estado de sabor evolucionado en el timepo no existe '
        print*, fState
    end select
    return
end subroutine temporalEvolutionFlavourState
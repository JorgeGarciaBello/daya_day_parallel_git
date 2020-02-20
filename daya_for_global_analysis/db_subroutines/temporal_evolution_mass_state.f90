subroutine temporalEvolutionMassState(mState,temporalMassVector,t12,t13,t23,delta,sm,aM,L,P)
    implicit none
    integer :: mState             ! mState is the number of the mass state
    double complex :: temporalMassVector(3)! temporalMassVector is the vector of the mass state
    real(8) :: t12,t13,t23        ! Are the mixing angles
    real(8) :: delta              ! delta is the CP violation phase factor    
    real(8) :: sm,aM              ! sm,aM are the squared mass difference m=m_21 y M=m_32
    real(8) :: L                  ! L is the length between the source of neutrinos an the position
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total    
        
    real(8) :: Hm(3,3)            ! Hm is the Hamiltonian in vaccum
    double complex :: m1Vector(3) ! m1Vector is the vector of a stationary mass state
    double complex :: m2Vector(3) ! m2Vector is the vector of a stationary mass state
    double complex :: m3Vector(3) ! m3Vector is the vector of a stationary mass state
    
    call hamiltonianInVaccum(Hm,sm,aM,P)
    call massState(1,m1Vector,t12,t13,t23,delta)
    call massState(2,m2Vector,t12,t13,t23,delta)
    call massState(3,m3Vector,t12,t13,t23,delta)

    select case(mState)
        case(1)
            temporalMassVector=exp(cmplx(0.0d0,-Hm(1,1)*L))*m1Vector
        case(2)
            temporalMassVector=exp(cmplx(0.0d0,-Hm(2,2)*L))*m2Vector
        case(3)
            temporalMassVector=exp(cmplx(0.0d0,-Hm(3,3)*L))*m3Vector
        case DEFAULT
        print*, 'Error: El estado de masa evolucionado en el timepo no existe'
        print*, mState
    end select

    return
end subroutine temporalEvolutionMassState
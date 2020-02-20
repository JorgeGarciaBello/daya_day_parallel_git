subroutine temporalEvolutionMassState_4f(mState,temporalMassVector,                   &
                                        t12,t13,t14,t23,t24,t34,delta1,delta2,delta3, &
                                        dm21,dm31,dm41,L,P)
    implicit none
    integer :: mState             ! mState is the number of the mass state
    double complex :: temporalMassVector(4)! temporalMassVector is the vector of the mass state
    real(8) :: t12,t13,t14,t23,t24,t34! Are the mixing angles
    real(8) :: delta1,delta2,delta3! delta is the CP violation phase factor
    real(8) :: dm21,dm31,dm41     ! dm21,dm31,dm41 are the squared mass differences
    real(8) :: L                  ! L is the length between the source of neutrinos an the position
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total    
        
    real(8) :: Hm(4,4)            ! Hm is the Hamiltonian in vaccum
    double complex :: m1Vector(4) ! m1Vector is the vector of a stationary mass state
    double complex :: m2Vector(4) ! m2Vector is the vector of a stationary mass state
    double complex :: m3Vector(4) ! m3Vector is the vector of a stationary mass state
    double complex :: m4Vector(4) ! m4Vector is the vector of a stationary mass state
    
    call hamiltonianInVaccum_4f(Hm,dm21,dm31,dm41,P)
    call massState_4f(1,m1Vector,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3)
    call massState_4f(2,m2Vector,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3)
    call massState_4f(3,m3Vector,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3)
    call massState_4f(4,m4Vector,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3)    

    select case(mState)
        case(1)
            temporalMassVector=exp(cmplx(0.0d0,-Hm(1,1)*L))*m1Vector
        case(2)
            temporalMassVector=exp(cmplx(0.0d0,-Hm(2,2)*L))*m2Vector
        case(3)
            temporalMassVector=exp(cmplx(0.0d0,-Hm(3,3)*L))*m3Vector
        case(4)
            temporalMassVector=exp(cmplx(0.0d0,-Hm(4,4)*L))*m4Vector
        case DEFAULT
        print*, 'Error: El estado de masa evolucionado en el timepo no existe'
        print*, mState
    end select
    return
end subroutine temporalEvolutionMassState_4f
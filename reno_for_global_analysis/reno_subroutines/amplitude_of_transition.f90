double complex function amplitudeOfTransition(flvr,fState,t12,t13,t23,delta,sm,aM,L,P)
    implicit none
    integer :: flvr               ! is the number of the flavour of the quantum bra 
    integer :: fState             ! fState is the number of the quantum temporal flavour ket state
    real(8) :: t12,t13,t23        ! Are the mixing angles
    real(8) :: delta              ! delta is the CP violation phase factor    
    real(8) :: sm,aM              ! sm,aM are the squared flavour difference m=m_21 y M=m_32
    real(8) :: L                  ! L is the length between the source of neutrinos an the position
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total    

    real(8) :: fVector(3)         ! is the quantum-bra in the model
    double complex :: temporalflavourVector(3)! temporalflavourVector is the vector of the flavour state

    call flavourState(flvr,fVector)
    call temporalEvolutionFlavourState(fState,temporalflavourVector,t12,t13,t23,delta,sm,aM,L,P)

    amplitudeOfTransition=0.0d0
    amplitudeOfTransition=dot_product(fVector,temporalflavourVector)
    return
end function amplitudeOfTransition
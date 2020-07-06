double complex function amplitudeOfTransition_4f(flvr,fState,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3, &
                                                             dm21,dm31,dm41,L,P)
    implicit none
    integer :: flvr               ! is the number of the flavour of the quantum bra 
    integer :: fState             ! fState is the number of the quantum temporal flavour ket state
    real(8) :: t12,t13,t14,t23,t24,t34! Are the mixing angles
    real(8) :: delta1,delta2,delta3! delta is the CP violation phase factor
    real(8) :: dm21,dm31,dm41     ! dm21,dm31,dm41 are the squared mass differences
    real(8) :: L                  ! L is the length between the source of neutrinos an the position
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total    

    real(8) :: fVector(4)         ! is the quantum-bra in the model
    double complex :: temporalflavourVector(4)! temporalflavourVector is the vector of the flavour state

    call flavourState_4f(flvr,fVector)
    call temporalEvolutionFlavourState_4f(  fState,temporalflavourVector,                 &
                                            t12,t13,t14,t23,t24,t34,delta1,delta2,delta3, &
                                            dm21,dm31,dm41,L,P)

    amplitudeOfTransition_4f=0.0d0
    amplitudeOfTransition_4f=dot_product(fVector,temporalflavourVector)
    return
end function amplitudeOfTransition_4f
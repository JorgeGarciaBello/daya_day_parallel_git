!##################################################################
!
!   probabilityOfTransition_4f: is the probability of transition
!       for a model of neutrino oscillations with a 
!       4-framework
!
!##################################################################
real(8) function probabilityOfTransition_4f(flvr,fState,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3, &
                                                        dm21,dm31,dm41,L,P)
    implicit none
    integer :: flvr               ! is the number of the flavour of the quantum bra 
    integer :: fState             ! fState is the number of the quantum temporal flavour ket state
    real(8) :: t12,t13,t14,t23,t24,t34! Are the mixing angles
    real(8) :: delta1,delta2,delta3! delta is the CP violation phase factor
    real(8) :: dm21,dm31,dm41     ! dm21,dm31,dm41 are the squared mass differences
    real(8) :: L                  ! L is the length between the source of neutrinos an the position
    real(8) :: P                  ! P es el momento del neutrino o la enerǵia total

    double complex :: amplitudeOfTransition_4f! is the amplitude of transition in the neutrino oscillaiton model

    probabilityOfTransition_4f=0.0d0
    probabilityOfTransition_4f= amplitudeOfTransition_4f(flvr,fState,t12,t13,t14,t23,t24,t34,        &
                                                                     delta1,delta2,delta3,           &
                                                                     dm21,dm31,dm41,L,P)             &
                                *CONJG(amplitudeOfTransition_4f(flvr,fState,t12,t13,t14,t23,t24,t34, &
                                                                            delta1,delta2,delta3,    &
                                                                            dm21,dm31,dm41,L,P))


    return
end function probabilityOfTransition_4f

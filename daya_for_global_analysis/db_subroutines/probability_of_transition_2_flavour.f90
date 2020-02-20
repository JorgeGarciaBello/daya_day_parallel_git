real(8) function probability_of_transition_2_flavour(angle,sm,L,P)
    implicit none
    real(8) :: angle
    real(8) :: sm
    real(8) :: L
    real(8) :: P

    probability_of_transition_2_flavour=0.0d0
    probability_of_transition_2_flavour=sin(1.27d0*(sm*L/P))**2
    !probability_of_transition_2_flavour=sin(2.0d0*angle)**2*sin(1.27d0*(sm*L/P))**2
    return
end function probability_of_transition_2_flavour
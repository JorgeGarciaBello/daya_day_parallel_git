!##################################################################
!
!   averageProbabilityOfSurvival_dr_bin_4f: is the average 
!       probability of survival by detector.reactor-bin 
!       for a model of neutrino oscillations with a 
!       effective mass difference delta_ee
!
!##################################################################

real(8) function averageProbabilityOfSurvival_dr_bin_4f(flvr,fState,bin,leng,   &
                                                       t12,t13,t14,t23,t24,t34, &
                                                       delta1,delta2,delta3,    &                                                    
                                                       dm21,dm31,dm41)
!real(8) function ProbabilityOfSurvival(t13, m13, x, leng)
    implicit none
    integer :: flvr               ! is the number of the flavour of the quantum bra 
    integer :: fState             ! fState is the number of the quantum temporal flavour ket state
    real(8) :: bin(2)             ! bin is an array of the limits of the energy-bin
    real(8) :: leng               ! leng is the distance from reactor to detector
    real(8) :: t12,t13,t14,t23,t24,t34! Are the mixing angles
    real(8) :: delta1,delta2,delta3! delta is the CP violation phase factor
    real(8) :: dm21,dm31,dm41     ! dm21,dm31,dm41 are the squared mass differences
    real(8) :: x  ! x es la energía del neutrino en MeV0
    real(8) :: probabilityOfTransition_4f! is the probability of transition for a model of neutrino oscillations with a 4-framework
    real(8) :: a,b,h
    integer :: i,n=400!000

    a=bin(1) ; b=bin(2);
    h = (b-a)/real(n)   
    averageProbabilityOfSurvival_dr_bin_4f=0.d0
    do i=1,n
        x = a + h*real(i-1)
        averageProbabilityOfSurvival_dr_bin_4f = averageProbabilityOfSurvival_dr_bin_4f &
            + h*( probabilityOfTransition_4f(flvr,fState,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3,dm21,dm31,dm41,leng,x)    &
                + probabilityOfTransition_4f(flvr,fState,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3,dm21,dm31,dm41,leng,x+h)) &
                / real(2)
    enddo
    averageProbabilityOfSurvival_dr_bin_4f = averageProbabilityOfSurvival_dr_bin_4f/(b-a)
    return
end function averageProbabilityOfSurvival_dr_bin_4f ! [SIN UNIDADES]
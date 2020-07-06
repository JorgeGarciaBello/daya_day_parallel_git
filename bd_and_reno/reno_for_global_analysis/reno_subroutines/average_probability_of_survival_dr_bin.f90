!##################################################################
!
!   reno_averageProbabilityOfSurvival_dr_bin: is the average 
!       probability of survival by detector.reactor-bin 
!       for a model of neutrino oscillations with a 
!       effective mass difference delta_ee
!
!##################################################################
real(8) function reno_averageProbabilityOfSurvival_dr_bin(d,r,bin)
    use reno_data, only: bines, length_d_r, engyPull
    use neu_osc_parameters, only: t13, dm31
    implicit none
    integer :: d                 ! d is the number of detector {1 for near / 2 for far}
    integer :: r                 ! r is the reactor's number
    integer :: bin               ! bin is the number (integer) of the ith-bin
    
    real(8) :: leng    
    real(8) :: x                  ! x es la energía del neutrino en MeV0
    real(8) :: ProbabilityOfSurvival,probability
    real(8) :: a,b,h
    integer :: i,n=50!000

    leng=length_d_r(d,r)    
    a=bines(bin,1); b=bines(bin,2);    
    h = (b-a)/real(n)   
    reno_averageProbabilityOfSurvival_dr_bin=0.d0
    do i=1,n
        x = a + h*real(i-1)
        x = x*(1.0d0 + engyPull)
        reno_averageProbabilityOfSurvival_dr_bin = reno_averageProbabilityOfSurvival_dr_bin &
            + h*( probability(t13,dm31,leng,x) &
                + probability(t13,dm31,leng,x + h) ) &
                !ProbabilityOfSurvival(x, leng, t13, dm31)  &
                !+ ProbabilityOfSurvival(x + h, leng, t13, dm31) ) &
                / real(2)
    enddo
    reno_averageProbabilityOfSurvival_dr_bin = reno_averageProbabilityOfSurvival_dr_bin/(b-a)
    return
end function reno_averageProbabilityOfSurvival_dr_bin
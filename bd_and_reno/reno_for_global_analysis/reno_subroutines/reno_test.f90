subroutine reno_test()
    use types
    use reno_data,only: bines 
    implicit none
    real(dp) :: t13,dmee
    real(dp) :: val,val2,reno_M_reactorSpectrum,reno_M_ExpectedNeutrinoSpectrum_dr
    real(dp) :: reno_M_expected_antineutrino_number_detector_reactor_bin
    real(dp) :: reno_expected_antineutrino_number_detector_reactor_bin
    real(dp) :: reno_reactorSpectrum,renoExpectedNeutrinoSpectrum_dr
    real(dp) :: x, probability,l
    integer :: i,d,r,bin,k
    call reno_generate_MC()

    t13=0.0_dp
    dmee=0.0_dp
    !val=reno_M_reactorSpectrum(1,5.0_dp,1,1)
    !val=reno_M_ExpectedNeutrinoSpectrum_dr(5.0_dp,1,1,1,1,t13,dmee)
    !do i=1,26
    !    val2=  reno_expected_antineutrino_number_detector_reactor_bin(1,1,i,t13,dmee)
    !    val=reno_M_expected_antineutrino_number_detector_reactor_bin(1,1,i,1,t13,dmee)
    !    print*,val,val2
    !enddo
    bin=1
    k=1

    d=1
    r=1
    !x=5.0_dp
    !print*, reno_reactorSpectrum(x,r), reno_M_reactorSpectrum(bin,x,k,r)
    !sprint*, renoExpectedNeutrinoSpectrum_dr(x,d,r,t13,dmee), reno_M_ExpectedNeutrinoSpectrum_dr(x,bin,d,r,k,t13,dmee)
     print*,   reno_expected_antineutrino_number_detector_reactor_bin(d,r,bin,  t13,dmee), &
             reno_M_expected_antineutrino_number_detector_reactor_bin(d,r,bin,k,t13,dmee)
    !call reno_thermal_power_test()
    !call reno_thermal_power_per_reactor_test()
    !call reno_sigma_thermal_power_test()

    !t13=asin(sqrt(0.086_dp))/2.0
    !dmee=2.68d-3
    !l=1090

    !do i=1,26        
    !    print*,i,probability(t13,dmee,l,bines(i,2))
    !enddo

    return
end subroutine reno_test
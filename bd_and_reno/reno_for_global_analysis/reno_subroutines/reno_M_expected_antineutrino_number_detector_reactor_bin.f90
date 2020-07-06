!####################################################################
!
!   reno_M_expected_antineutrino_number_detector_reactor_bin 
!       is the function that return the
!       expected neutirno spectrum by hall
!
!####################################################################
function reno_M_expected_antineutrino_number_detector_reactor_bin(d,r,bin,k_rand,t13,dmee)
    use types
    use reno_data, only: NBIN,bines,detector_efficiency,detector_calibration,LT_d,neutrino_energy, &
                         rand_Nefficiency,rand_Nenergy
    implicit none    
    real(dp) :: reno_M_expected_antineutrino_number_detector_reactor_bin
    integer  :: d,r,bin,k_rand
    real(dp) :: t13,dmee
    real(dp) :: reno_M_ExpectedNeutrinoSpectrum_dr !is the expected neutirno spectrum in the detector "d" from all reactors
    real(dp) :: a,b,h               ! limits of the bin    
    integer  :: k,n=5
    real(dp) :: x,result

    a=bines(bin,1); b=bines(bin,2); h=(b-a)/real(n)
    result=0.0d0    
    do k=1,n
        x=a+h*real(k-1)
        x=neutrino_energy(bin)*x
        !x=rand_Nenergy(bin,k_rand)*x
        if (x<1.8010001) x=1.8010001
        result=result+h*(reno_M_ExpectedNeutrinoSpectrum_dr(x,   bin,d,r,k_rand,t13,dmee)+     & 
                         reno_M_ExpectedNeutrinoSpectrum_dr(x+h, bin,d,r,k_rand,t13,dmee))/2.0d0
    enddo
    select case(d)
        case(1) ! Near detector
            reno_M_expected_antineutrino_number_detector_reactor_bin=rand_Nefficiency(bin,d,k_rand)*           &
                                                                   1.01267474277184d0*detector_efficiency(d)*  &
                                                                                      86400.0d0*LT_d(d)*       &
                                                                                      detector_calibration(d)* &
                                                                                      0.2d0*(result/(b-a))
                                                                                      
        case(2) ! Far detector
            reno_M_expected_antineutrino_number_detector_reactor_bin= rand_Nefficiency(bin,d,k_rand)*          &
                                                                   1.08102772018778d0*detector_efficiency(d)*  &
                                                                                      86400.0d0*LT_d(d)*       &
                                                                                      detector_calibration(d)* &
                                                                                      0.2d0*(result/(b-a))
                                                                                      
    end select
    !reno_M_expected_antineutrino_number_detector_reactor_bin=result
    return
end function reno_M_expected_antineutrino_number_detector_reactor_bin
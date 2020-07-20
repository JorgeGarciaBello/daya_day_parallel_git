!####################################################################
!
!   DC_expected_antineutrino_number_detector_reactor_bin 
!       is the function that return the
!       expected neutirno spectrum by hall
!
!####################################################################
function DC_expected_antineutrino_number_detector_reactor_bin(d,r,bin,t13,dmee)
    use types
    use DC_settings, only: NBIN
    use DC_data, only: bines_p,efficiency,live_days!,detector_calibration,neutrino_energy
                         
    implicit none    
    real(dp) :: DC_expected_antineutrino_number_detector_reactor_bin
    integer  :: d,r,bin
    real(dp) :: t13,dmee
    real(dp) :: DC_expected_neutrino_spectrum_dr !is the expected neutirno spectrum in the detector "d" from all reactors
    real(dp) :: DC_prompt_to_antineutrino_energy
    real(dp) :: a,b,h               ! limits of the bin    
    integer  :: k,n=5
    real(dp) :: x,result

    a=bines_p(bin,1); b=bines_p(bin,2); h=(b-a)/real(n)
    result=0.0d0
    do k=1,n
        x=a+h*real(k-1)
        !x=neutrino_energy(bin)*x
        x=DC_prompt_to_antineutrino_energy(1,x)
        result=result+h*(DC_expected_neutrino_spectrum_dr(x,   d,r,t13,dmee)+     & 
                         DC_expected_neutrino_spectrum_dr(x+h, d,r,t13,dmee))/2.0d0
    enddo
    select case(d)
        case(1) ! Near detector
            DC_expected_antineutrino_number_detector_reactor_bin=efficiency(d)*                    &
                                                                 (86400.0d0*live_days(d))*           &                                                                 
                                                                 0.25d0*(result/(b-a))!*           &
                                                                 !detector_calibration(d)
                                                                                      
        case(2) ! Far detector
            DC_expected_antineutrino_number_detector_reactor_bin=efficiency(d)*                     &
                                                                  (86400.0d0*live_days(d))*           &                                                                  
                                                                  0.25d0*(result/(b-a))!*           &
                                                                  !detector_calibration(d)
                                                                                      
    end select    
    return
end function DC_expected_antineutrino_number_detector_reactor_bin
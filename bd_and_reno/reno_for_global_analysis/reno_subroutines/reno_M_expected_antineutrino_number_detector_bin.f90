function reno_M_expected_antineutrino_number_detector_bin(d,bin,k,t13,dmee)
    use types
    implicit none
    real(dp) :: reno_M_expected_antineutrino_number_detector_bin
    integer  :: d,r,bin,k
    real(dp) :: t13,dmee
    real(dp) :: reno_M_expected_antineutrino_number_detector_reactor_bin
    real(dp) :: result

    result=0.0_dp
    result=reno_M_expected_antineutrino_number_detector_reactor_bin(d,1,bin,k,t13,dmee)+ &
           reno_M_expected_antineutrino_number_detector_reactor_bin(d,2,bin,k,t13,dmee)+ &
           reno_M_expected_antineutrino_number_detector_reactor_bin(d,3,bin,k,t13,dmee)+ &
           reno_M_expected_antineutrino_number_detector_reactor_bin(d,4,bin,k,t13,dmee)+ &
           reno_M_expected_antineutrino_number_detector_reactor_bin(d,5,bin,k,t13,dmee)+ &
           reno_M_expected_antineutrino_number_detector_reactor_bin(d,6,bin,k,t13,dmee)

    reno_M_expected_antineutrino_number_detector_bin=result
    return
end function reno_M_expected_antineutrino_number_detector_bin
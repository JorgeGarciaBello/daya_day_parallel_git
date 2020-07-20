subroutine DC_test()
    use types
    implicit none
    real(dp) :: DC_reactor_spectrum
    real(dp) :: DC_expected_neutrino_spectrum_dr
    real(dp) :: DC_expected_antineutrino_number_detector_reactor_bin
    real(dp) :: x
    real(dp) :: t13,dmee
    integer  :: i,j,d,r
    
    d=2
    r=1
    t13=0.0_dp
    dmee=0.0_dp

    do i=1,38
        !x=1.81_dp+(12.0_dp/50)*i
        print*, i, DC_expected_antineutrino_number_detector_reactor_bin(d,r,i,t13,dmee)
    enddo
    return
end subroutine DC_test
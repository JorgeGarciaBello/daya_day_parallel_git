subroutine reno_create_antineutrino_number_detector_reactor_bin(dmee,t13)
    use types
    !use data_settings, only: n,dm_M_data,t13_M_data
    use reno_data, only: NBIN,ADS,RCTS,data
    implicit none
    real(dp) :: t13,dmee                                                      ! are the neutrino oscillation parameters we are interested
    real(dp) :: reno_expected_antineutrino_number_detector_reactor_bin    
    real(dp) :: reno_M_expected_antineutrino_number_detector_reactor_bin
    integer  :: d,r,bin,k

    data=0.0_dp
    k=1
    do d=1,ADS
        do r=1,RCTS
            !!$omp parallel do !private(dmee,t13,bin)
            do bin=1,NBIN
                !data(bin,d,r)=reno_expected_antineutrino_number_detector_reactor_bin(d,r,bin,t13,dmee)
                data(bin,d,r)=reno_M_expected_antineutrino_number_detector_reactor_bin(d,r,bin,k,t13,dmee)
            enddo
            !!$omp end parallel do
        enddo
    enddo
    
    !do bin=1,NBIN
    !    print*, bin,data(bin,1,1)
    !enddo
    return
end subroutine reno_create_antineutrino_number_detector_reactor_bin
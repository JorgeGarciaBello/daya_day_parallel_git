subroutine reno_generate_weight_w_systematic_array(t13,dmee,rand_w_i)
    use types
    use reno_data, only: NBIN,num_experiments
    implicit none
    real(dp) :: t13,dmee
    real(dp) :: rand_w_i(NBIN,num_experiments)
    real(dp) :: reno_M_expected_antineutrino_number_detector_bin
    integer  :: bin,k

    rand_w_i=0.0_dp
    ! Obtención de vectores Nbar    
    !$omp parallel do
    do bin=1,NBIN
        do k=1,num_experiments
            !rand_w_i(i,k)=(reno_M_expected_antineutrino_number_detector_reactor_bin(i,k,t13,dmee)+bkg_t_N_f(i))/ &
            !              (reno_M_expected_antineutrino_number_detector_reactor_bin(i,k,t13,dmee)+bkg_t_N_n(i))
            !rand_w_i(i,k)=(rand_Nspectramodel_f(i,k)*reno_M_expected_antineutrino_number_detector_reactor_bin(i,k,t13,dmee))/ &
            !              (rand_Nspectramodel_n(i,k)*reno_M_expected_antineutrino_number_detector_reactor_bin(i,k,t13,dmee))
            rand_w_i(bin,k)=reno_M_expected_antineutrino_number_detector_bin(2,bin,k,t13,dmee)/ &
                            reno_M_expected_antineutrino_number_detector_bin(1,bin,k,t13,dmee)
        enddo
    enddo
    !$omp end parallel do
    return
end subroutine reno_generate_weight_w_systematic_array
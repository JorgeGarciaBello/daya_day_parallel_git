module DC_data
    use types
    use DC_settings
    implicit none
    real(dp) :: bins(NBIN,2)
    real(dp) :: far_obs(NBIN)
    real(dp) :: far_exp(NBIN)

    real(dp) :: bines_p(NBIN,2)
    real(dp) :: length_d_r(ADS,RCTRS)
    
    real(dp) :: N_n_obs(NBIN)
    real(dp) :: N_f_obs(NBIN)
    real(dp) :: N_n_exp(NBIN)
    real(dp) :: N_f_exp(NBIN)

    real(dp) :: fast_neutron_n(NBIN)
    real(dp) :: litio_n(NBIN)
    real(dp) :: accidental_n(NBIN)
    real(dp) :: fast_neutron_f(NBIN)
    real(dp) :: litio_f(NBIN)
    real(dp) :: accidental_f(NBIN)

    real(dp) :: thermal_power(ADS)
    real(dp) :: fission_fractions(4)
    real(dp) :: target_proton(ADS)
    real(dp) :: efficiency(ADS)
    real(dp) :: live_days(ADS)



    real(dp) :: sigma_thermal_power(ADS)
    real(dp) :: sigma_efficiency(ADS)

end module DC_data
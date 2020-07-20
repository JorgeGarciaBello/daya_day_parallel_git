function DC_FUNC(t13,dmee)
    use types
    use DC_settings, only: NBIN
    use DC_data, only: far_obs,far_exp,bins,bines_p,N_n_obs,N_f_obs,N_n_exp,N_f_exp
    implicit none
    real(dp) :: DC_FUNC
    real(dp) :: t13,dmee
    real(dp) :: averageProbability3f,probability
    real(dp) :: DC_prompt_to_antineutrino_energy
    REAL(dp) :: newbin(2)
    real(dp) :: l
    integer  :: bin
    l=1056.0_dp
    DC_FUNC=0.0_dp
    do bin=1,NBIN
        newbin(1)=  DC_prompt_to_antineutrino_energy(bin,bines_p(bin,1))
        newbin(2)=  DC_prompt_to_antineutrino_energy(bin,bines_p(bin,2))
        !print*,bin,newbin(1),newbin(2),averageProbability3f(newbin,l,t13,dmee)
        !print*,N_f_obs(bin),N_f_exp(bin)
        DC_FUNC=DC_FUNC+(N_f_obs(bin)-N_f_exp(bin)*averageProbability3f(newbin,l,t13,dmee))**2/N_f_obs(bin)
    enddo    
end function DC_FUNC
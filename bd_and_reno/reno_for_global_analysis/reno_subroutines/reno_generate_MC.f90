subroutine reno_generate_MC()
    implicit none
    call reno_generate_total_Bkg()
    !call db_generate_B_Nfastn()
    !call db_generate_B_Nacc()
    !call db_generate_B_NLiHe()
    !call db_generate_B_NCf()




    call reno_generate_thermal_power()  
    call reno_generate_fission_fraction()
    call reno_generate_average_energy_released_per_fission()
    call reno_generate_reactor_flux_model()
    call reno_generate_cross_section()
    call reno_generate_efficiency()
    call reno_generate_energy()


    !call db_generate_spectra_model()
    return
end subroutine reno_generate_MC
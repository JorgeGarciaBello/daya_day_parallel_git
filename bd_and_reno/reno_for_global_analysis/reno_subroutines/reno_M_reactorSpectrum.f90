function reno_M_reactorSpectrum(bin,x,k,r) ! [MeV/s]
    use types
    use reno_data, only:RCTS,RIR,FNF,TP_r,&
                        rand_Nthermalpower,rand_Nfissionfraction,rand_Navrgenergyperfission,rand_Nreactorfluxmodel
    implicit none
    real(dp) :: reno_M_reactorSpectrum
    real(dp) :: x                  ! x es la energía de neutrino
    integer  :: bin,k,r
    real(dp) :: reactorFlu
    real(dp) :: spectrum, spectrum235U, spectrum238U, spectrum239Pu, spectrum241Pu        
       
    reactorFlu = 0.0
    reactorFlu = rand_Nfissionfraction(bin,1,k)*spectrum235U(x)  &
               + rand_Nfissionfraction(bin,2,k)*spectrum238U(x)  &
               + rand_Nfissionfraction(bin,3,k)*spectrum239Pu(x) &
               + rand_Nfissionfraction(bin,4,k)*spectrum241Pu(x)
    
    !La definición esta en: 
    !New measurement of θ 13 via neutron capture on hydrogen at Daya Bay    
    reno_M_reactorSpectrum = rand_Nreactorfluxmodel(bin,k)* &
                             (rand_Nthermalpower(bin,r,k)/rand_Navrgenergyperfission(bin,1,k))*reactorFlu*6.2415D+21
    ! 6.2415*1E+21 => [1GW] = [1GJ/s] = [6,2415 × 10^27 eV/s] = [6,2415 × 10^21 MeV/s]
    !                   [MeV/s]*[MeV-1*fission]*[# MeV-1 fission-1]=[# MeV-1 s-1]
end function reno_M_reactorSpectrum

! Las unidades no son consistentes
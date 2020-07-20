function DC_reactor_spectrum(x,r) ! [MeV/s]
    use types        
    use DC_data, only: fission_fractions,thermal_power
    implicit none
    real(dp) :: DC_reactor_spectrum
    real(dp) :: spectrum, spectrum235U, spectrum238U, spectrum239Pu, spectrum241Pu
    real(dp) :: x                  ! x es la energía de neutrino
    real(dp) :: meanThermalEnergy = 205.95586D0 ![MeV fission-1]
    real(dp) :: reactorFlu
    integer  :: r    
       
    reactorFlu = 0.0
    reactorFlu =   fission_fractions(1)*spectrum235U(x)  &
                 + fission_fractions(2)*spectrum238U(x)  &
                 + fission_fractions(3)*spectrum239Pu(x) &  
                 + fission_fractions(4)*spectrum241Pu(x)    
    
    DC_reactor_spectrum = (thermal_power(r)/meanThermalEnergy)*reactorFlu*6.2415D+21
    ! 6.2415*1E+21 => [1GW] = [1GJ/s] = [6,2415 × 10^27 eV/s] = [6,2415 × 10^21 MeV/s]
    !                   [MeV/s]*[MeV-1*fission]*[# MeV-1 fission-1]=[# MeV-1 s-1]
end function DC_reactor_spectrum
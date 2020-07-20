function DC_prompt_to_antineutrino_energy(i,E_prompt)
    use types
    implicit none    
    real(dp) :: DC_prompt_to_antineutrino_energy
    real(dp) :: E_prompt    ! E_prompt
    integer :: i

    DC_prompt_to_antineutrino_energy=0.0d0    
    DC_prompt_to_antineutrino_energy = E_prompt + 0.78d0 ! [MeV]

    if (DC_prompt_to_antineutrino_energy<=1.8010001) DC_prompt_to_antineutrino_energy=1.8010001
    return
end function DC_prompt_to_antineutrino_energy
!####################################################################
!
!   reno_M_ExpectedNeutrinoSpectrum_dr is the function that return the
!       expected neutirno spectrum in the detector "d" from the
!       reactor "r"
!
!####################################################################
function reno_M_ExpectedNeutrinoSpectrum_dr(x,bin,d,r,k,t13,dmee) ![#/MeV]
    use types
    use reno_data, only: osc,length_d_r,TP_d,NBIN, &
                         rand_Ncrosssection
    implicit none    
    real(dp) :: reno_M_ExpectedNeutrinoSpectrum_dr
    real(dp) :: x,t13,dmee                  ! Neutrino energy in [MeV]
    integer  :: bin,d,r,k                ! the number ot detector and reactor respectively            
    real(dp) :: reno_probability_of_survival
    real(dp) :: crossSection       ! Is the cross section for the inverse beta decay [m^2]
    real(dp) :: reno_M_reactorSpectrum    ! Is the spectrum of the reactors
    real(dp) :: val1,val2,val3,val4,val5,val6
    !Datos tomados de: Daya Bay Reactor Neutrino Experiment
    !Supplemental Material
    !March 2016
    reno_M_ExpectedNeutrinoSpectrum_dr = (1.0D0 / (4.0D0*3.1415D0*length_d_r(d,r)**2))*  &   ![1/m^2]                            
                    reno_probability_of_survival(x,length_d_r(d,r),t13,dmee)*            &
                    reno_M_reactorSpectrum(bin,x,k,r)*                                       &   ![#/MeV]                            
                    rand_Ncrosssection(bin,1,k)*crossSection(x)*                                &   ![m^2]                            
                    TP_d(d)                                                                 ![#ofProtons]
    return
end function reno_M_ExpectedNeutrinoSpectrum_dr
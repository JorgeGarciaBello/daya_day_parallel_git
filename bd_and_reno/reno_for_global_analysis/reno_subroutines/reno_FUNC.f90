!#################################################################
!
!       FUNC: es el nombre la función en la que se define 
!             la estadística CHI^{2} utilizada en RENO (2018)
!             (Tiene 2 parametros que se fijan t13-m_ee y pulls)
!
!##################################################################                 
function reno_FUNC(t13,dmee,P)
    use  types
    use reno_data, only: NDIM,NBIN,ADS,RCTS,nearObs,nearExp,farObs,farExp,bkg,sigma_background_d, &
                         sigmaNear,sigmaFar,bkgFarAcc,bkgFarCf,bkgFarFN,bkgFarLH,uS, &
                         rFluxC,rFluxU,bkgTotal,engyScale,DeffU,DeffCS,engyPull,     &
                         c_i,sigma_reactor_flux,sigma_detection_efficiency,sigma_energy_scale, &
                         data
    implicit none
    integer,parameter ::near=1
    integer,parameter ::far=2
    real(dp) :: t13,dmee
    real(dp) :: reno_FUNC
    real(dp) :: P(NDIM)            ! Parámetros de oscilacióna determinar
        
    
    real(dp) :: sigma_r_flux               ! sigma_r_flux is the uncorrelated reactor.flux uncertainty
    real(dp) :: sigma_eff                  ! sigma_eff uncorrelated detection uncertainty
    real(dp) :: sigma_scale                ! sigma_scale uncorrelated energy scale uncertainty
    real(dp) :: b_d(ADS)                   ! b_d are the pulls associated to sigma_background_d respectively    
    real(dp) :: fr(RCTS)                   ! fr are the pulls associated to sigma_background_d respectively    
    real(dp) :: eps                        ! eps are the pulls associated to sigma_detection efficiencys    
    real(dp) :: eta                        ! eps are the pulls associated to sigma energy scale
    

    real(dp) :: observedFarToNearRatioIBD  ! observedFarToNearRatioIBD is the observed fat-to-near ratio of IBD candidates in the i-th bin after background subtraction
    real(dp) :: expectedFarToNearRatioIBD  ! expectedFarToNearRatioIBD is the expected fat-to-near ratio of IBD candidates in the i-th bin after background subtraction
    real(dp) :: statisticalUncertainty     ! statisticalUncertainty is the statistical uncertainty of the observed far.to-near ratio of IBD in the i-th bin
    real(dp) :: reno_model, oscModel
    real(dp) :: chi_2
    integer  :: i,j
    real(dp) :: O_i,T_i,U2_i
    real(dp) :: nearExpC,farExpC
    integer  :: d,r

    real(dp) :: totalBackgroundBin
    real(dp) :: renoExpectedNeutrinoSpectrumByBinNear
    real(dp) :: renoExpectedNeutrinoSpectrumByBinFar
    real(dp) :: reno_expected_antineutrino_number_detector_reactor_bin

    real(dp) :: N_near
    real(dp) :: N_far
    
    chi_2=0.0d0  
    sigma_eff=uS(3)/100.0d0
    sigma_r_flux=uS(4)/100.0d0
    sigma_scale=uS(5)/100.0d0    

    b_d=(/P(1),P(2)/)
    fr=(/P(3),P(4),P(5),P(6),P(7),P(8)/)
    eps=P(9)
    eta=P(10)   

    !c_i=(/P(11),P(12),P(13),P(14),P(15),P(16),P(17),P(18),P(19),P(2),   &
    !      P(),P(),P(),P(),P(),P(),P(),P(),P(),P(),   &
    !      P(),P(),P(),P(),P(),P()/)
    select case(6)
        case(1)
            !#############################################################
            !
            !   Propuesta de Chi cuadrada por Benito Vargas Perez
            !
            !#############################################################
            do i=1,NBIN
                oscModel=reno_model(far,i)
                chi_2=chi_2 + ( farObs(i)  - farExp(i)*oscModel )**2/sigmaFar(i)**2
            enddo
        case(2)
            !#############################################################
            !
            !   Propuesta de Chi cuadrada por Benito Vargas Perez
            !
            !#############################################################
            !engyPull=P(5)
            do i=1,NBIN
                oscModel=reno_model(far,i)
             !   chi_2=chi_2 + ( farObs(i)  - farExp(i)*oscModel     &
             !                              + ( bkgFarFN(i)*(1.0d0+P(1))    &
             !                                  +bkgFarAcc(i)*(1.0d0+P(2))   &
             !                                  +bkgFarLH(i)*(1.0d0+P(3))    &
             !                                  +bkgFarCf(i)*(1.0d0+P(4))    &                                               
                !) )**2/sigmaFar(i)**2
              !  ) )**2/ (  rFluxC(i)**2     &
              !            +rFluxU(i)**2     &
              !            +DeffU(i)**2      &                          
              !            +DeffCS(i)**2     &
              !            +sigmaFar(i)**2  &
              !          )
            enddo
            !chi_2=chi_2 +(P(1)/0.027d0)**2  &
            !            +(P(2)/0.021d0)**2  &
            !            +(P(3)/0.081d0)**2  &
            !            +(P(4)/0.093d0)**2

            !chi_2=chi_2 +(P(5)/0.0015d0)**2
        case(3)
            !#############################################################
            !
            !   Modificaciones sugeridas por Jorge a la 
            !   Chi cuadrada propuesta por Benito - 5 pulls
            !
            !#############################################################
            !engyPull=P(5)
            do i=1,NBIN
                oscModel=reno_model(far,i)
            !    chi_2=chi_2 + ( farObs(i)  - farExp(i)*oscModel     &
            !                               + ( bkgFarFN(i)*(1.0d0+P(1))    &
            !                                   +bkgFarAcc(i)*(1.0d0+P(2))   &
            !                                   +bkgFarLH(i)*(1.0d0+P(3))    &
            !                                   +bkgFarCf(i)*(1.0d0+P(4))    &                                               
                !) )**2/sigmaFar(i)**2
            !    ) )**2/ (  rFluxC(i)**2     &
            !              +rFluxU(i)**2     &
            !              +DeffU(i)**2      & 
            !              +DeffCS(i)**2     &
            !              +sigmaFar(i)**2  &
            !            )
            enddo
            !chi_2=chi_2 +(P(1)/0.027d0)**2  &
            !            +(P(2)/0.021d0)**2  &
            !            +(P(3)/0.081d0)**2  &
            !            +(P(4)/0.093d0)**2

            !chi_2=chi_2 +(P(5)/0.0015d0)**2
        case(4)
            !#############################################################
            !   
            !   Propuesta de Chi cuadrada por 
            !   Jorge Garcia 
            !
            !#############################################################
            !engyPull=P(7)
            do i=1,NBIN
            oscModel=reno_model(far,i)
             !   chi_2=chi_2 + ( farObs(i)  - farExp(i)*oscModel*(1.0d0+P(1)+P(2))& !*(1.0d0+P(3)+P(4))       &
             !                              + (          bkgFarFN(i)*(P(3))         &
             !                                          +bkgFarAcc(i)*(P(4))        &
             !                                          +bkgFarLH(i)*(P(5))         &
             !                                          +bkgFarCf(i)*(P(6))         &              
             !                                     )& 
             !                              )**2/ (&!farObs(i)+(bkgFarFN(i)+bkgFarAcc(i)+bkgFarLH(i)+bkgFarCf(i))&!rFluxC(i)**2     &
                                                  !+rFluxU(i)**2     &
                                                  !+DeffU(i)**2      & 
                                                  !+DeffCS(i)**2     &
             !                                     +sigmaFar(i)**2&
           !                                     )!farObs(i)**2)
            enddo
            !chi_2=chi_2 +(P(1)/0.009d0)**2   & ! reactor Flux Uncorrelated systematic uncertainty
                        !+(P(2)/0.02d0)**2    & ! reactor Flux Correlated systematic uncertainty
            !            +(P(2)/0.0013d0)**2   ! detector efficiency Uncorrelated systematic uncertainty
                        !+(P(4)/0.0016d0)**2   ! detector efficiency Correlated systematic uncertainty or statistical
            !chi_2=chi_2 &!+(P(3)/0.0561d0)**2    ! P(3) is the pull associated with the background

            !            +(P(3)/0.027d0)**2  &
            !            +(P(4)/0.021d0)**2  &
            !            +(P(5)/0.081d0)**2  &
             !           +(P(6)/0.093d0)**2

            !chi_2=chi_2 +(P(7)/0.0015d0)**2   ! energy scale uncorrelated systematic uncertainty
          case(5)
            !#############################################################
            !   
            !   Propuesta de Chi cuadrada por 
            !   Benito y Jorge
            !
            !#############################################################
            
            ! P(1) correccion a la eficiencia de detección
            ! P(2-5) corrección a las fuentes de background
            ! P(6) corrección a energy scale 
            !engyPull=P(6)
            do i=1,NBIN
              oscModel=reno_model(far,i)
              totalBackgroundBin=bkgFarFN(i)+bkgFarAcc(i)+bkgFarLH(i)+bkgFarCf(i)
             ! chi_2=chi_2 + ( farObs(i)+totalBackgroundBin - farExp(i)*oscModel*(1.0d0+P(1))& !*(1.0d0+P(3)+P(4))       &
             !                              - (          bkgFarFN(i)*(P(2))         &
             !                                          +bkgFarAcc(i)*(P(3))        &
             !                                          +bkgFarLH(i)*(P(4))         &
             !                                          +bkgFarCf(i)*(P(5))         &              
              !                                    )& 
             !                              )**2/ (rFluxU(i)**2+sigmaFar(i)**2)
            enddo
            !chi_2=chi_2+(P(1)/0.0021d0)**2   ! corrección a la eficiencia de detección
            ! Terminos correspondientes al background
            !chi_2=chi_2 +((1.0d0-P(2))/0.027d0)**2  &
            !            +((1.0d0-P(3))/0.021d0)**2  &
            !            +((1.0d0-P(4))/0.081d0)**2  &
            !            +((1.0d0-P(5))/0.093d0)**2

            !chi_2=chi_2 +(P(6)/0.0015d0)**2   ! energy scale uncorrelated systematic uncertainty
          case(6)
            !#############################################################
            !
            !   Ji-cuadrada tomada el paper referenciado en el más reciente
            !
            !#############################################################
            do i=1,NBIN
              N_near=0.0d0; N_far=0.0d0;
              do r=1,RCTS
                !N_near=N_near+(1.0d0 + fr(r))*reno_expected_antineutrino_number_detector_reactor_bin(1,r,i,t13,dmee)
                !N_far =N_far +(1.0d0 + fr(r))*reno_expected_antineutrino_number_detector_reactor_bin(2,r,i,t13,dmee)
                N_near=N_near+(1.0d0 + fr(r))*data(i,1,r)
                N_far =N_far +(1.0d0 + fr(r))*data(i,2,r)                
              enddo              
              nearExpC=( bkg(i,1)*b_d(1) ) + (1.0d0+eps+eta)*N_near
              farExpC =( bkg(i,2)*b_d(2) ) + (1.0d0+eps+eta)*N_far              

              O_i=farObs(i)/nearObs(i)
              !T_i=P(11)*((farExpC)/(nearExpC))
              T_i=((farExpC)/(nearExpC))

              !U2_i=(O_i**2)*(  (sqrt(farObs(i)+bkg(i,2))/farObs(i))**2 + (sqrt(nearObs(i)+bkg(i,1))/nearObs(i))**2   )
              U2_i=(O_i**2)*(  (sqrt(farObs(i))/farObs(i))**2 + (sqrt(nearObs(i))/nearObs(i))**2   )              
              U2_i=U2_i*0.964d0
              chi_2=chi_2 + (O_i - T_i)**2/U2_i
              !print*,i,chi_2
            enddo
            do d=1,ADS
              chi_2=chi_2 + (b_d(d)/(sigma_background_d(d)/100.0d0))**2              
            enddo
            do r=1,RCTS
              chi_2=chi_2 + (fr(r)/(sigma_reactor_flux(r)/100.0d0))**2
            enddo
            chi_2=chi_2 + (eps/(sigma_detection_efficiency/100.0d0))**2 + (eta/(sigma_energy_scale/100.0d0))**2
    end select    
    reno_FUNC=chi_2
    return
end function reno_FUNC
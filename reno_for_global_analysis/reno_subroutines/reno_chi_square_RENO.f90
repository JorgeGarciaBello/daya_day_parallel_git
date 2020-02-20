!#################################################################
!
!       FUNC: es el nombre la función en la que se define 
!             la estadística CHI^{2} utilizada en RENO (2018)
!             (Tiene 2 parametros que se fijan t13-m_ee y pulls)
!
!##################################################################                 
real(8) function FUNC(P)
    use reno_data, only: NDIM,NBIN,ADS,RCTS,nearObs,nearExp,farObs,farExp, &
                         sigmaNear,sigmaFar,bkgFarAcc,bkgFarCf,bkgFarFN,bkgFarLH,uS, &
                         rFluxC,rFluxU,bkgTotal,engyScale,DeffU,DeffCS,engyPull,     &
                         fr,eps,eta,c_i
    implicit none
    integer,parameter ::near=1
    integer,parameter ::far=2
    real(8) :: P(NDIM)            ! Parámetros de oscilacióna determinar
        
    real(8) :: sigma_d_bkg(ADS)           ! sigma_d_bkg is the background uncertainty (near,far)
    real(8) :: sigma_r_flux               ! sigma_r_flux is the uncorrelated reactor.flux uncertainty
    real(8) :: sigma_eff                  ! sigma_eff uncorrelated detection uncertainty
    real(8) :: sigma_scale                ! sigma_scale uncorrelated energy scale uncertainty
    real(8) :: b_d(ADS)                   ! b_d are the pulls associated to sigma_d_bkg respectively    
    

    real(8) :: observedFarToNearRatioIBD  ! observedFarToNearRatioIBD is the observed fat-to-near ratio of IBD candidates in the i-th bin after background subtraction
    real(8) :: expectedFarToNearRatioIBD  ! expectedFarToNearRatioIBD is the expected fat-to-near ratio of IBD candidates in the i-th bin after background subtraction
    real(8) :: statisticalUncertainty     ! statisticalUncertainty is the statistical uncertainty of the observed far.to-near ratio of IBD in the i-th bin
    real(8) :: model, oscModel
    real(8) :: chi_2
    integer :: i,j
    real(8) :: O_i,T_i,U_i
    real(8) :: nearExpC,farExpC
    integer :: d,r

    real(8) :: totalBackgroundBin
    real(8) :: renoExpectedNeutrinoSpectrumByBinNear
    real(8) :: renoExpectedNeutrinoSpectrumByBinFar
    
    chi_2=0.0d0   

    sigma_d_bkg(1)=uS(1)/100.0d0
    sigma_d_bkg(2)=uS(2)/100.0d0
    sigma_eff=uS(3)/100.0d0
    sigma_r_flux=uS(4)/100.0d0
    sigma_scale=uS(5)/100.0d0    

    !b_d=(/P(1),P(2)/)
    !fr=(/P(3),P(4),P(5),P(6),P(7),P(8)/)
    !eps=P(9)
    !eta=P(10)

    !c_i=(/P(11),P(12),P(13),P(14),P(15),P(16),P(17),P(18),P(19),P(2),   &
    !      P(),P(),P(),P(),P(),P(),P(),P(),P(),P(),   &
    !      P(),P(),P(),P(),P(),P()/)
    select case(5)
        case(1)
            !#############################################################
            !
            !   Propuesta de Chi cuadrada por Benito Vargas Perez
            !
            !#############################################################
            do i=1,NBIN
                oscModel=model(far,i)
                chi_2=chi_2 + ( farObs(i)  - farExp(i)*oscModel )**2/sigmaFar(i)**2
            enddo
        case(2)
            !#############################################################
            !
            !   Propuesta de Chi cuadrada por Benito Vargas Perez
            !
            !#############################################################
            engyPull=P(5)
            do i=1,NBIN
                oscModel=model(far,i)
                chi_2=chi_2 + ( farObs(i)  - farExp(i)*oscModel     &
                                           + ( bkgFarFN(i)*(1.0d0+P(1))    &
                                               +bkgFarAcc(i)*(1.0d0+P(2))   &
                                               +bkgFarLH(i)*(1.0d0+P(3))    &
                                               +bkgFarCf(i)*(1.0d0+P(4))    &                                               
                !) )**2/sigmaFar(i)**2
                ) )**2/ (  rFluxC(i)**2     &
                          +rFluxU(i)**2     &
                          +DeffU(i)**2      &                          
                          +DeffCS(i)**2     &
                          +sigmaFar(i)**2  &
                        )
            enddo
            chi_2=chi_2 +(P(1)/0.027d0)**2  &
                        +(P(2)/0.021d0)**2  &
                        +(P(3)/0.081d0)**2  &
                        +(P(4)/0.093d0)**2

            chi_2=chi_2 +(P(5)/0.0015d0)**2
        case(3)
            !#############################################################
            !
            !   Modificaciones sugeridas por Jorge a la 
            !   Chi cuadrada propuesta por Benito - 5 pulls
            !
            !#############################################################
            engyPull=P(5)
            do i=1,NBIN
                oscModel=model(far,i)
                chi_2=chi_2 + ( farObs(i)  - farExp(i)*oscModel     &
                                           + ( bkgFarFN(i)*(1.0d0+P(1))    &
                                               +bkgFarAcc(i)*(1.0d0+P(2))   &
                                               +bkgFarLH(i)*(1.0d0+P(3))    &
                                               +bkgFarCf(i)*(1.0d0+P(4))    &                                               
                !) )**2/sigmaFar(i)**2
                ) )**2/ (  rFluxC(i)**2     &
                          +rFluxU(i)**2     &
                          +DeffU(i)**2      & 
                          +DeffCS(i)**2     &
                          +sigmaFar(i)**2  &
                        )
            enddo
            chi_2=chi_2 +(P(1)/0.027d0)**2  &
                        +(P(2)/0.021d0)**2  &
                        +(P(3)/0.081d0)**2  &
                        +(P(4)/0.093d0)**2

            chi_2=chi_2 +(P(5)/0.0015d0)**2
        case(4)
            !#############################################################
            !   
            !   Propuesta de Chi cuadrada por 
            !   Jorge Garcia 
            !
            !#############################################################
            !engyPull=P(7)
            do i=1,NBIN
            oscModel=model(far,i)
                chi_2=chi_2 + ( farObs(i)  - farExp(i)*oscModel*(1.0d0+P(1)+P(2))& !*(1.0d0+P(3)+P(4))       &
                                           + (          bkgFarFN(i)*(P(3))         &
                                                       +bkgFarAcc(i)*(P(4))        &
                                                       +bkgFarLH(i)*(P(5))         &
                                                       +bkgFarCf(i)*(P(6))         &              
                                                  )& 
                                           )**2/ (&!farObs(i)+(bkgFarFN(i)+bkgFarAcc(i)+bkgFarLH(i)+bkgFarCf(i))&!rFluxC(i)**2     &
                                                  !+rFluxU(i)**2     &
                                                  !+DeffU(i)**2      & 
                                                  !+DeffCS(i)**2     &
                                                  +sigmaFar(i)**2&
                                                  )!farObs(i)**2)
            enddo
            chi_2=chi_2 +(P(1)/0.009d0)**2   & ! reactor Flux Uncorrelated systematic uncertainty
                        !+(P(2)/0.02d0)**2    & ! reactor Flux Correlated systematic uncertainty
                        +(P(2)/0.0013d0)**2   ! detector efficiency Uncorrelated systematic uncertainty
                        !+(P(4)/0.0016d0)**2   ! detector efficiency Correlated systematic uncertainty or statistical
            chi_2=chi_2 &!+(P(3)/0.0561d0)**2    ! P(3) is the pull associated with the background

                        +(P(3)/0.027d0)**2  &
                        +(P(4)/0.021d0)**2  &
                        +(P(5)/0.081d0)**2  &
                        +(P(6)/0.093d0)**2

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
            engyPull=P(6)
            do i=1,NBIN
            oscModel=model(far,i)
            totalBackgroundBin=bkgFarFN(i)+bkgFarAcc(i)+bkgFarLH(i)+bkgFarCf(i)
                chi_2=chi_2 + ( farObs(i)+totalBackgroundBin - farExp(i)*oscModel*(1.0d0+P(1))& !*(1.0d0+P(3)+P(4))       &
                                           - (          bkgFarFN(i)*(P(2))         &
                                                       +bkgFarAcc(i)*(P(3))        &
                                                       +bkgFarLH(i)*(P(4))         &
                                                       +bkgFarCf(i)*(P(5))         &              
                                                  )& 
                                           )**2/ (rFluxU(i)**2+sigmaFar(i)**2)
            enddo
            chi_2=chi_2+(P(1)/0.0021d0)**2   ! corrección a la eficiencia de detección
            ! Terminos correspondientes al background
            chi_2=chi_2 +((1.0d0-P(2))/0.027d0)**2  &
                        +((1.0d0-P(3))/0.021d0)**2  &
                        +((1.0d0-P(4))/0.081d0)**2  &
                        +((1.0d0-P(5))/0.093d0)**2

            chi_2=chi_2 +(P(6)/0.0015d0)**2   ! energy scale uncorrelated systematic uncertainty
          case(6)
            !#############################################################
            !
            !   Ji-cuadrada tomada el paper referenciado en el más reciente
            !
            !#############################################################
            do i=1,NBIN
              !nearExpC=(1.d0 + b_d(1))*renoExpectedNeutrinoSpectrumByBinNear(i)
              !farExpC =(1.d0 + b_d(2))*renoExpectedNeutrinoSpectrumByBinFar(i)
              nearExpC=renoExpectedNeutrinoSpectrumByBinNear(i)
              farExpC =renoExpectedNeutrinoSpectrumByBinFar(i)
              
              O_i=farObs(i)/nearObs(i);  T_i=farExpC/nearExpC

              U_i=(O_i**2)*(  (sigmaFar(i)/farObs(i))**2 + (sigmaNear(i)/nearObs(i))**2   )
              
              chi_2=chi_2 + (O_i - T_i)**2/U_i! +(/0.02d0)**2
            enddo

            !do d=1,ADS
            !  chi_2=chi_2 + (b_d(d)/sigma_d_bkg(d))**2
            !enddo

            !do r=1,RCTS
            !  chi_2=chi_2 + (fr(r)/sigma_r_flux)**2
            !enddo

            chi_2=chi_2 + (eps/sigma_eff)**2    !&
            !            + (eta/sigma_scale)**2

    end select
    !print*, chi_2
    FUNC=chi_2   
    return
end function FUNC
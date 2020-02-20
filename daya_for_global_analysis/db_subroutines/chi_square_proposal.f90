real(8) function chiSquareProposal(P)
    use db_data, only:NDIM,NBIN,ADS,RCTS,gFactor,farObs,farExp,sigmaFarBkg,farBkg,pullEngy
    !use neu_osc_parameters, only: dm31
    implicit none
    real(8) :: P(NDIM)
    integer :: d,r
    integer :: i
    real(8) :: model
    real(8) :: mod

    real(8) :: eps                ! is the parameter of absolute normalization
    real(8) :: etaBkg(5)          ! is the pull parameter for the uncertainty of total background
    real(8) :: etaBkg26(26)       ! is the pull parameter for the uncertainty of total background by bin
    real(8) :: epsD               ! is the pull parameter for the uncertainty of detection efficiency
    real(8) :: epsR               ! is the pull parameter for the uncertainty of neutrinos from the reactors

    real(8) :: sigmaBkg(5)        ! is the uncertainty of the total background
    real(8) :: sigmaBkg26(26)     ! is the uncertainty of the total background by bin
    real(8) :: sigmaD             ! is the uncertainty of the detection efficiency
    real(8) :: sigmaR             ! is the uncertainty of the production of neutrinos from the reactors

    real(8) :: expectedNeutrinoSpectrumByBinFar
    real(8) :: expectedNumberNeutrinoByBinFar

    !sigmaBkg=294.0d0
    sigmaBkg=(/33.8928791591803,33.8928791591803,271.143033273443,78.1539010315909,67.7857583183607/)

    sigmaD=0.004d0
    sigmaR=0.016d0

    eps=P(1)
    epsD=P(2)
    epsR=P(3)
    etaBkg=(/P(4),P(5),P(6),P(7),P(8)/)!,P(9),P(10),P(11),P(12),P(13)   &
      !,P(14),P(15),P(16),P(17),P(18),P(19),P(20),P(21),P(22),P(23)   &
      !,P(24),P(25),P(26),P(27),P(28),P(29)/)         

    chiSquareProposal=0.0d0
    select case(1)
        case(1)
            pullEngy=P(9)
            !pullEngy=0.0d0
            do i=1,NBIN
                !mod=model(i)
                mod=expectedNeutrinoSpectrumByBinFar(i)
                !mod=expectedNumberNeutrinoByBinFar(i)
                chiSquareProposal = chiSquareProposal &
                                  !+ (farObs(i)-farExp(i)*corrections(i)*mod*(1.0d0+eps+epsD+epsR)+etaBkg(1) &
                                  + (farObs(i)-mod*(1.0d0+eps+epsD+epsR)+etaBkg(1) &
                                                                                  +etaBkg(2) &
                                                                                  +etaBkg(3) &
                                                                                  +etaBkg(4) &
                                                                                  +etaBkg(5))**2 &
                                  / sigmaFarBkg(i)!*(1.0d0+0.1d0))!(farObs(i)+farBkg(i))
                                  !  sigmaFarBkg
            end do
            chiSquareProposal = chiSquareProposal        &
                              + (   epsD/sigmaD   )**2   &
                              + (   epsR/sigmaR   )**2
            
            do i=1,5
                chiSquareProposal = chiSquareProposal        &
                                  + ( etaBkg(i)/sigmaBkg(i) )**2
            end do

            chiSquareProposal = chiSquareProposal        &
                                  + ( pullEngy/0.002d0 )**2

            !hiSquareProposal = chiSquareProposal        &
             !                     + ( (dm31 - 2.68d-3) /0.14d-3 )**2
        case(2) !Análisis de 26 bines adicionales, uno por cada bin de bkg
            do i=1,NBIN
                mod=model(i)
                chiSquareProposal = chiSquareProposal &
                                  + (farObs(i)-farExp(i)*mod*(1.0d0+eps+epsD+epsR)+etaBkg26(i))**2 &
                                  / (farObs(i))
            end do
            chiSquareProposal = chiSquareProposal        &
                              + (   epsD/sigmaD   )**2   &
                              + (   epsR/sigmaR)**2
            do i=1,NBIN
                chiSquareProposal = chiSquareProposal        &
                                  + ( etaBkg26(i)/sigmaFarBkg(i) )**2
            end do
        case(3)
            print*, 'opcion incorrecta en chi_square_proposal'
    end select 
    
    !print*,chiSquareProposal
    return
end function chiSquareProposal
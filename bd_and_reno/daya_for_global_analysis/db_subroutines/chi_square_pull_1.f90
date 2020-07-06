real(8)  function chiSquarePull1(P)
    use db_data, only: NDIM,ADS,RCTS,Md=>Md_1607,Bd
    implicit none
    real(8) :: P(NDIM)
    real(8) :: sigmaR             ! us the uncorrelated reactor uncertainty
    real(8) :: sigmaD             ! us the uncrrelated detectiono uncertainty
    real(8) :: sigmaB(ADS)        ! us the uncorrelated background uncertainty
    real(8) :: sigmaBB(ADS)       ! us the uncorrelated background uncertainty calculado por mi como la suma de los cuadrados
    
    real(8) :: eps                ! is the parameter of absolute normalization
    real(8) :: alphaR(RCTS)       ! is an array with pull parameters reactor
    real(8) :: epsD(ADS)          ! is an array with pull parameters detector
    real(8) :: etaD(ADS)          ! is an array with pull parameters background-detector

    real(8) :: expectedNumberNeutrinosDetector
    real(8) :: sum_W_alp
    integer :: d,r
    real(8) :: Td

    eps=P(1)
    !alphaR=(/P(2),P(3),P(4),P(5),P(6),P(7)/)
    !epsD=(/P(8),P(9),P(10),P(11),P(12),P(13),P(14),P(15)/)
    !etaD=(/P(16),P(17),P(18),P(19),P(20),P(21),P(22),P(23)/)

    chiSquarePull1=0.0d0    
    sigmaR=0.008D0
    sigmaD=0.002D0
    sigmaB=(/1040,1176,875,782,155,150,150,133/);
    !sigmaB=0.30d0 ! Incertidumbres del paper [7] Se menciona que se obtinen un 0.3% 
                                                ! y se muestran valores de incertidumbre en la tabla    !
    do d=1,ADS
        Td=expectedNumberNeutrinosDetector(d)
        chiSquarePull1=chiSquarePull1   &
                        +(Md(d)-Td*(1.0d0+eps+sum_W_alp(d,alphaR)+epsD(d))+etaD(d))**2/(Md(d)+Bd(d))
    enddo
    do r=1,RCTS
        chiSquarePull1=chiSquarePull1+(alphaR(r)/sigmaR)**2
    enddo
    do d=1,ADS
        chiSquarePull1=chiSquarePull1+( (epsD(d)/sigmaD)**2 + (etaD(d)/(sigmaB(d)))**2 )
        !chiSquarePull1=chiSquarePull1+( (epsD(d)/sigmaD)**2 + (etaD(d)/sigmaBB(ADS))**2 )
    enddo

    return
end function chiSquarePull1
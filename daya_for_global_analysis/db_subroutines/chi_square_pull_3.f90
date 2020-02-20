real(8)  function chiSquarePull3(Md,Bd,Wdr,eps,alphaR,epsD,etaD,                               &
                                t12,t13,t14,t23,t24,t34,delta1,delta2,delta3,dm21,dm31,dm41,   &
                                ADS,RCTS,RIR,FNF,LT_d,TP_d,length_d_r,TP_r)
    implicit none
    integer :: osc=1              ! osc = 1 para oscilaciones, osc = 2 para no oscilaciones
    
    real(8) :: t12,t13,t14,t23,t24,t34! Are the mixing angles
    real(8) :: delta1,delta2,delta3! delta is the CP violation phase factor
    real(8) :: dm21,dm31,dm41     ! dm21,dm31,dm41 are the squared mass differences
    integer :: ADS                ! ADS is the number of detectors
    integer :: RCTS               ! RCTS is the number of reactors
    integer :: RIR                ! RIR is the number of radioactive isotopes in the reactors 
    real(8) :: FNF(RIR)           ! FNF is  the fraction nuclear fission per radioactive isotope
    real(8) :: LT_d(ADS)          ! LT_d is the life time in seconds of the detector d
    real(8) :: TP_d(ADS)          ! TP_d is the target proton per detector
    real(8) :: length_d_r(ADS,RCTS)! lengths_d_r is the length between  detector d and reactor r
    real(8) :: TP_r(RCTS)         ! TP_r is the termal power of the reactor r

    real(8) :: Md(ADS)            ! is the number of obserbed neutrino events by detector
    real(8) :: Td                 ! us the number of expected neutirno events by detector
    real(8) :: Bd(ADS)            ! us the backgroud by detector
    real(8) :: Wdr(ADS,RCTS)      ! is the fraction of IBD contribution of the reactor-r to the detoctor-d determined by baselines and reactor fluxes
    real(8) :: sigmaR             ! us the uncorrelated reactor uncertainty
    real(8) :: sigmaD             ! us the uncrrelated detectiono uncertainty
    real(8) :: sigmaB             ! us the uncorrelated background uncertainty
    real(8) :: sigmaDD             ! us the statistic uncertainty of Md
    real(8) :: sigmaBB(ADS)       ! us the uncorrelated background uncertainty calculado por mi como la suma de los cuadrados
    real(8) :: eps                ! is the parameter of absolute normalization
    real(8) :: alphaR(RCTS)       ! is an array with pull parameters reactor
    real(8) :: epsD(ADS)          ! is an array with pull parameters detector
    real(8) :: etaD(ADS)          ! is an array with pull parameters background-detector

    real(8) :: expectedNumberNeutrinosDetector
    real(8) :: sum_W_alp
    integer :: d,r

    chiSquarePull3=0.0d0
    !sigmaR=0.8D0 ; sigmaD=0.2D0 ; sigmaB=62.0d0 ! Incertidumbres del primer paper
    !sigmaR=0.8D0 ; sigmaD=0.2D0 ; sigmaB=63.2d0 ! Incertidumbres del paper [4]
    sigmaR=0.9D0 ; sigmaD=0.2D0 ; sigmaB=0.30d0; sigmaDD=0.005d0 ! Incertidumbres del paper [7] Se menciona que se obtinen un 0.3% 
                                                ! y se muestran valores de incertidumbre en la tabla
    !sigmaR=0.9D0 ; sigmaD=0.2D0 ; sigmaBB=(/1.1626d0,1.1603d0,0.6228d0,0.6079d0,0.0151d0,0.0151d0,0.0151d0,0.0051d0/) ! Incertidumbres anteriores con las calculadas por mi    
    do d=1,ADS
        Td=expectedNumberNeutrinosDetector(osc,d,t12,t13,t14,t23,t24,t34,delta1,delta2,delta3,        &
                                           dm21,dm31,dm41,ADS,RCTS,RIR,FNF,LT_d,TP_d,length_d_r,TP_r)           
        chiSquarePull3=chiSquarePull3+(Md(d)-Td*(1.0d0+eps+sum_W_alp(d,Wdr,alphaR)+epsD(d))-(1.0d0+etaD(d))*Bd(d))**2/(Md(d)+Bd(d))
    enddo
    do r=1,RCTS
        chiSquarePull3=chiSquarePull3+(alphaR(r)/sigmaR)**2
    enddo
    do d=1,ADS
        chiSquarePull3=chiSquarePull3+( (epsD(d)/sigmaD)**2 + (etaD(d)/sigmaB)**2 )
        !chiSquarePull3=chiSquarePull3+( (epsD(d)/sigmaD)**2 + (etaD(d)/sigmaBB(ADS))**2 )
    enddo

    return
end function chiSquarePull3
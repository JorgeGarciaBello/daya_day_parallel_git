module reno_data
    implicit none
    integer,parameter :: NDIM=6                     ! NDIM is the number of pull parameters
    integer,parameter :: ADS=2                       ! ADS is the number of detectors
    integer,parameter :: RCTS=6                      ! RCTS is the number of reactors
    integer,parameter :: RIR=4                        ! RIR is the number of radioactive isotopes in the reactors     
    integer,parameter :: NBIN=26                       ! NBIN is the number of energy-bin    
    integer,parameter :: osc=1
    real(8),save :: pulls(NDIM)                ! array de pulls que minimizan a chi2
    real(8),save :: LT_d(ADS)                  ! LT_d is the live time of the near and far detectors in days    
    real(8),save :: TP_d(ADS)                  ! TP_d is the target proton by detector
    real(8),save :: TBR_n_f(ADS)               ! TBR_n_f is the Total Backgroond Rate (#IBD/ day)
    real(8),save :: BSU_n_f_P_T(ADS,2)         ! BSU_n_f_P_T is the Background systematic uncertainties for near andd far detector in percentage and total IBD (%,#IBD)
    real(8),save :: length_d_r(ADS,RCTS)       ! length_d_r is the length between detector-reactor (Tesis Tiago)        
    real(8),save :: bines(NBIN,2)              ! bines es una matriz con los valores que determinan los bines de energía usados
    real(8),save :: nearObs(NBIN)              ! nearObs is the observed black-data-ṕoint spectrum in the Near detector by i-bin
    real(8),save :: farObs(NBIN)               ! farObs is the observed black-data-ṕoint spectrum in the Far detector by i-bin
    real(8),save :: nearExp(NBIN)              ! nearExp is the expected blue-data-ṕoint spectrum in the Near detector by i-bin
    real(8),save :: farExp(NBIN)               ! farExp is the expected blue-data-ṕoint spectrum in the Far detector by i-bin
    real(8),save :: sigmaNear(NBIN)            ! is the uncertainty of Near black data point
    real(8),save :: sigmaFar(NBIN)             ! is the uncertainty of Far black data point
    real(8),save :: wDR(ADS,RCTS)              ! wDR  is the contribution ratios per detector from reactor    
    real(8),save :: uS(5)                      ! uS US contains the systematic uncertainties employed in a pulls chi
    real(8),save :: bkg(NBIN,ADS)              ! bkg is the number of background events per bin in each detectore (Near,Far)
    real(8),save :: TP_r(RCTS)                 ! TP_r is the thermal power by reactor
    real(8),save :: FNF(RIR)                   ! FNF is the nuclear fission fraction
    real(8),save :: bkgFarAcc(NBIN)            ! bkgFarAcc is the accidental background for far detector by bin
    real(8),save :: bkgFarCf(NBIN)             ! bkgFarCf is the Californio background  for far detector by bin
    real(8),save :: bkgFarFN(NBIN)             ! bkgFarFN is the fast neutron background  for far detector by bin
    real(8),save :: bkgFarLH(NBIN)             ! bkgFarLH is the Helium and  background  for far detector by bin
    
    real(8),save :: rFluxC(NBIN)               ! rFluxC is the correlated systematic uncertainty
    real(8),save :: rFluxU(NBIN)               ! rFluxU is the uncorrelated systematic uncertainty
    real(8),save :: bkgTotal(NBIN)             ! bkgTotal is the 
    real(8),save :: engyScale(NBIN)            ! engyScale is the -
    real(8),save :: DeffU(NBIN)                ! DeffU is the detection efficienty of the detector -
    real(8),save :: DeffCS(NBIN)               ! DeffCS is the systematic uncorrelated uncertainty -
    real(8),save :: systUncor(NBIN)             ! systCorr is the systematic correlated uncertainty -
    

    real(8),save :: engyPull                   ! engyPull is the pull associated with the energy scale

    real(8),save :: fr(RCTS)                   ! fr are the pulls associated to sigma_r_flux
    real(8),save :: eps                        ! eps is the pull associated to sigma_eff
    real(8),save :: eta                        ! e is the pull associated to sigma_scale
    real(8),save :: c_i(NBIN)                  ! c_i is the pull associated to the uncorrelated bin-to-bin spectrum shape uncertainty 
    
end module reno_data
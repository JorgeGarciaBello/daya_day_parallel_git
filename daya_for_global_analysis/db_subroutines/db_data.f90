module db_data
    implicit none
    integer,parameter :: NDIM=9       ! NDIM is the number of pull parameters
    integer,parameter :: ADS=8         ! ADS is the number of detectors
    integer,parameter :: RCTS=6        ! RCTS is the number of reactors
    integer,parameter :: RIR=4         ! RIR is the number of radioactive isotopes in the reactors     
    integer,parameter :: NBIN=26       ! NBIN is the number of energy's bin
    integer,parameter :: osc=1         ! osc = 1 para oscilaciones,  2 para no oscilaciones 
    real(8) :: pull_min(NDIM)
    real(8) :: Bd(ADS)
    real(8) :: Sb(ADS)
    real(8) :: FNF(RIR)
    real(8) :: IBD_fdr(ADS,RCTS)       ! Set of data of the Daya Bay experimento
    real(8) :: LT_d(ADS)
    real(8) :: TP_d(ADS)
    real(8) :: length_d_r(ADS,RCTS)
    real(8) :: TP_r(RCTS)              ! Set of data of the Daya Bay experimento    
    real(8) :: bines(NBIN,2)             ! bines es una matriz con los valores que determinan los bines de energía usados
    real(8) :: v_ij_1(NBIN,NBIN)           ! Matriz de covarianza invertida
    real(8) :: N_obs(ADS)              ! Is the number of neutrino events per detector
    real(8) :: N_nom(NBIN)               ! N_nom es la densidad de neutirnos en el i-ésimo bin promedio
    real(8) :: blueH(NBIN,3)             ! blueH is the blue spectrum without oscillations per Hall (i,j); i bin, j= hall
    real(8) :: blackH(NBIN,3)            ! blackH is the observed black-ṕoint spectrum per Hall (i,j); i bin, j= hall
    real(8) :: wH(NBIN,8)
    real(8) :: wD(272,6)               ! wH, wD  ate the contribution ratios per detector to a Hall and per reacto to detectors    
    real(8) :: wNH(NBIN,2)               ! wNH is the contribution ratios per near hall to the total per bin    
    real(8) :: Md_1607(ADS)            ! Is the number of neutrino events per detector taken from  1607_05378 paper
    real(8) :: N_e(ADS)
    real(8) :: gFactor(4,RCTS)         ! Are the geometric factor that gives the cnotribution of neutrino events per reactor to far-detectors by bin 
    real(8) :: farObs(NBIN)            ! far observed background subtracted spectrum 
    real(8) :: farExp(NBIN)            ! far expected background subtracted spectrum
    real(8) :: sigmaFarBkg(NBIN)       ! sigmaFarBkg is the total uncertainty for the far spectrum by bin 
    real(8) :: farBkg(NBIN)            ! farBkg is the total background in the far detector for the three period of data taking

    real(8) :: pullEngy

end module db_data
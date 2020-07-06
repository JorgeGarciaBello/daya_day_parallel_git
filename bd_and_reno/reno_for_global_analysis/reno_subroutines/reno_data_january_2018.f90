!######################################################################
!
!  Module containing variables needed to
!  compute the neutrino oscillations 
!  parameters from therenoDataJanuary2018 
!
!######################################################################
!MODULE renoDataJanuary2018
!implicit none
!integer,parameter :: ADS=2            ! ADS is the number of detectors
!integer,parameter :: RCTS=6           ! RCTS is the number of reactors
!integer,parameter :: RIR=4            ! RIR is the number of radioactive isotopes in the reactors     
!integer,parameter :: NBIN=26          ! NBIN is the number of energy-bin
!real(8) :: LT_d(ADS)                  ! LT_d is the live time of the near and far detectors in days    
!real(8) :: TBR_n_f(ADS)               ! TBR_n_f is the Total Backgroond Rate (#IBD/ day)
!real(8) :: BSU_n_f_P_T(ADS,2)         ! BSU_n_f_P_T is the Background systematic uncertainties for near andd far detector in percentage and total IBD (%,#IBD)
!real(8) :: length_d_r(ADS,RCTS)       ! length_d_r is the length between detector-reactor (Tesis Tiago)        
!real(8) :: bines(NBIN,2)              ! bines es una matriz con los valores que determinan los bines de energía usados
!real(8) :: nearObs(NBIN)              ! nearObs is the observed black-data-ṕoint spectrum in the Near detector by i-bin
!real(8) :: farObs(NBIN)               ! farObs is the observed black-data-ṕoint spectrum in the Far detector by i-bin
!real(8) :: nearExp(NBIN)              ! nearExp is the expected blue-data-ṕoint spectrum in the Near detector by i-bin
!real(8) :: farExp(NBIN)               ! farExp is the expected blue-data-ṕoint spectrum in the Far detector by i-bin
!real(8) :: sigmaNear(NBIN)            ! is the uncertainty of Near black data point
!real(8) :: sigmaFar(NBIN)             ! is the uncertainty of Far black data point
!real(8) :: wDR(ADS,RCTS)              ! wDR  is the contribution ratios per detector from reactor    
!real(8) :: uS(5)                      ! uS US contains the systematic uncertainties employed in a pulls chi
!real(8) :: bkg(NBIN,ADS)              ! bkg is the number of background events per bin in each detectore (Near,Far)
!real(8) :: TP_r(RCTS)                 ! TP_r is the thermal power by reactor
!real(8) :: FNF(RIR)                   ! FNF is the nuclear fission fraction
!integer :: i,j,reason

    
!    !open(10,file="reno_data/reno_weightsPerNearHall.dat", status="old")
!    !open(11,file="reno_data/reno_weightsPerHs.dat", status="old")
!    open(12,file="reno_data/reno_contribution_reactor_flux.dat", status="old")    
!    open(13,file="reno_data/reno_near_obs.dat", status="old")
!    open(14,file="reno_data/reno_near_exp.dat", status="old")
!    open(15,file="reno_data/reno_total_backgroud_rate_n_f.dat", status="old")
!    open(16,file="reno_data/reno_background_systematic_uncertainties_n_f_percentage_total.dat", status="old")
!    open(17,file="reno_data/reno_error_sigmas.dat", status="old")
!    !open(18,file="reno_data/reno_detector_fraction_IBD_per_ads.dat", status="old")
!    open(19,file="reno_data/reno_live_time_days_n_f.dat", status="old")
!    !open(20,file="reno_data/reno_target_proton.dat", status="old")
!    open(21,file="reno_data/reno_lengths.dat", status="old")
!    open(22,file="reno_data/reno_thermal_power_by_reactor.dat", status="old")
!    open(23,file="reno_data/reno_bines.dat", status="old")
!    !open(24,file="reno_data/reno_v_ij_1.dat", status="old")
!    !open(25, file='reno_data/reno_n_nom_array.dat', status='old')
!    open(26,file='reno_data/reno_background_bin_near_far.dat', status='old')
!    open(27,file='reno_data/reno_fraction_nuclear_fission.dat',status='old')
!    open(28,file='reno_data/reno_far_obs.dat',status='old')
!    open(29,file='reno_data/reno_far_exp.dat',status='old')
!    open(30,file='reno_data/reno_uncertainty_per_bin_of_near_data.dat',status='old')
!    open(31,file='reno_data/reno_uncertainty_per_bin_of_far_data.dat',status='old')
!    
!        !read(10,*,IOSTAT=reason) ((wNH(i,j), j=1,2), i=1,34)
!        !read(11,*,IOSTAT=reason) ((wH(i,j), j=1,8), i=1,34)
!        read(12,*,IOSTAT=reason) ((wDR(i,j), j=1,6), i=1,2)
!        read(13,*,IOSTAT=reason) nearObs
!        read(14,*,IOSTAT=reason) nearExp
!        read(15,*,IOSTAT=reason) TBR_n_f
!        read(16,*,IOSTAT=reason) ((BSU_n_f_P_T(i,j), j=1,2), i=1,2)
!        read(17,*,IOSTAT=reason) uS
!        !read(18,*,IOSTAT=reason) ((IBD_d_r(i,j), j=1,RCTS), i=1,ADS)
!        read(19,*,IOSTAT=reason) LT_d   ! [days]
!        !read(20,*,IOSTAT=reason) TP_d
!        read(21,*,IOSTAT=reason) ((length_d_r(i,j), j=1,RCTS), i=1,ADS) ! [m]
!        read(22,*,IOSTAT=reason) TP_r
!        read(23,*,IOSTAT=reason) ((bines(i,j), j=1,2), i=1,NBIN)
!        !read(24,*,IOSTAT=reason) ((v_ij_1(i,j), j=1,34), i=1,34)
!        !read(25,*,IOSTAT=reason) N_nom
!        read(26,*,IOSTAT=reason) ((bkg(i,j),j=1,2), i=1,NBIN)
!        read(27,*,IOSTAT=reason) FNF
!        read(28,*,IOSTAT=reason) farObs
!        read(29,*,IOSTAT=reason) farExp
!        read(30,*,IOSTAT=reason) sigmaNear
!        read(31,*,IOSTAT=reason) sigmaFar
!    !close(10)
!    !close(11)
!    close(12)
!    close(13)
!    close(14)
!    close(15)
!    close(16)
!    close(17)
!    !close(18)
!    close(19)
!    !close(20)
!    close(21)
!    close(22)
!    close(23)
!    !close(24)
!    !close(25)
!    close(26)
!    close(27)
!    close(28)
!    close(29)
!    close(30)
!    close(31)

   
!END MODULE renoDataJanuary2018


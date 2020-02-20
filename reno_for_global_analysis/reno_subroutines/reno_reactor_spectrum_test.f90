subroutine reactorSpectrumTest()
    implicit none
    integer,parameter :: RCTS=6
    integer,parameter :: RIR=4              ! RIR is the number of radioactive isotopes in the reactors 
    integer,parameter :: ADS=2              ! ADS is the number of detectors        
    integer,parameter :: NBIN=26            ! NBIN is the number of energy-bin  
    integer :: MP,NP              ! NP is the number of pulls, MP is the number of points for polygon of ABOEBA    
    real(8) :: LT_d(ADS)          ! LT_d is the live time of the near and far detectors in days    
    real(8) :: TBR_n_f(ADS)       ! TBR_n_f is the Total Backgroond Rate (#IBD/ day)
    real(8) :: BSU_n_f_P_T(ADS,2) ! BSU_n_f_P_T is the Background systematic uncertainties for near andd far detector in percentage and total IBD (%,#IBD)
    real(8) :: length_d_r(ADS,RCTS)! length_d_r is the length between detector-reactor (Tesis Tiago)        
    real(8) :: bines(NBIN,2)              ! bines es una matriz con los valores que determinan los bines de energía usados
    real(8) :: blackNear(NBIN)            ! blackNear is the observed black-data-ṕoint spectrum in the Near detector by i-bin
    real(8) :: blackFar(NBIN)             ! blackFar is the observed black-data-ṕoint spectrum in the Far detector by i-bin
    real(8) :: blueNear(NBIN)             ! blueNear is the expected blue-data-ṕoint spectrum in the Near detector by i-bin
    real(8) :: blueFar(NBIN)              ! blueFar is the expected blue-data-ṕoint spectrum in the Far detector by i-bin
    real(8) :: wDR(ADS,RCTS)              ! wDR  is the contribution ratios per detector from reactor    
    real(8) :: uS(5)                      ! uS US contains the systematic uncertainties employed in a pulls chi
    real(8) :: bkg(NBIN,ADS)              ! bkg is the number of background events per bin in each detectore (Near,Far)
    real(8) :: TP_r(RCTS)                 ! TP_r is the thermal power by reactor
    real(8) :: FNF(RIR)                   ! FNF is the nuclear fission fraction
    
    real(8) :: reactorSpectrum    ! reactorSpectrum is the function that return the reactor spectrum from the r-reactor 
    real(8) :: spectrum239Pu,spectrum241Pu,spectrum235U,spectrum238U

    real(8) :: x
    integer :: r
    integer :: i,j
    
    
    !call readRENOData(ADS,RCTS,RIR,FNF,NBIN,MP,NP,LT_d,TBR_n_f,BSU_n_f_P_T,length_d_r,TP_r,bines,   &
    !                blackNear,blackFar,blueNear,blueFar,wDR,uS,bkg)

    !open(101,file='reactor_spectra/results/reactor_spectrum_1.dat')
    !open(102,file='reactor_spectra/results/reactor_spectrum_2.dat')
    !open(103,file='reactor_spectra/results/reactor_spectrum_3.dat')
    !open(104,file='reactor_spectra/results/reactor_spectrum_4.dat')
    !open(105,file='reactor_spectra/results/reactor_spectrum_5.dat')
    !open(106,file='reactor_spectra/results/reactor_spectrum_6.dat')
    !    x=0.0d0
    !    do i=1,100
    !        write(101,*) x,reactorSpectrum(x,1,RCTS,RIR,FNF,TP_r)
    !        write(102,*) x,reactorSpectrum(x,2,RCTS,RIR,FNF,TP_r)
    !        write(103,*) x,reactorSpectrum(x,3,RCTS,RIR,FNF,TP_r)
    !        write(104,*) x,reactorSpectrum(x,4,RCTS,RIR,FNF,TP_r)
    !        write(105,*) x,reactorSpectrum(x,5,RCTS,RIR,FNF,TP_r)
    !        write(105,*) x,reactorSpectrum(x,6,RCTS,RIR,FNF,TP_r)
    !        x=x+0.12d0
    !    enddo
    !close(101)
    !close(102)
    !close(103)
    !close(104)
    !close(105)
    !close(106)

    !open(107,file='reactor_spectra/results/spectrum_239_Pu.dat')
    !open(108,file='reactor_spectra/results/spectrum_241_Pu.dat')
    !open(109,file='reactor_spectra/results/spectrum_235_U.dat')
    !open(110,file='reactor_spectra/results/spectrum_238_U.dat')
    !    x=0.0d0    
    !    do j=1,100
    !        write(107,*) x, spectrum239Pu(x)
    !        write(108,*) x, spectrum241Pu(x)
    !        write(109,*) x, spectrum235U(x)
    !        write(110,*) x, spectrum238U(x)
    !        x=x+0.12d0
    !    enddo
    !close(107)
    !close(108)
    !close(109)
    !close(110)       

    return
end subroutine reactorSpectrumTest
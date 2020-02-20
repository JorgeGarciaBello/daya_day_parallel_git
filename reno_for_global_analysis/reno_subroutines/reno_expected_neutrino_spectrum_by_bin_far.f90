!####################################################################
!
!   renoExpectedNeutrinoSpectrumByBinFar is the function that return the
!       expected neutirno spectrum by hall
!
!####################################################################
real(8) function renoExpectedNeutrinoSpectrumByBinFar(i)
    use reno_data, only: NBIN,bines
    implicit none
    integer,parameter :: d=2      ! the number of detector far
    integer :: i                  ! is the number of bin

    real(8) :: renoExpectedNeutrinoSpectrum_d !is the expected neutirno spectrum in the detector "d" from all reactors

    real(8) :: a,b               ! limits of the bin
    real(8) :: h
    integer :: k,n=100
    real(8) :: x

    real(8) :: binCalibrations(NBIN)

    binCalibrations=(/  1.25300031437122d0,1.12278761124368d0,1.06728218554959d0,1.02298513587434d0,1.01070388983629d0,     &
                        0.995971284695543d0,0.98577605363075d0,0.98662547705955d0,0.97814301208061d0,0.981221399669782d0,   &
                        0.990914615263318d0,0.980376277736029d0,0.973290644878107d0,0.96476792169897d0,0.994716202732024d0, &
                        1.01607503448384d0,1.05246325947194d0,1.07544401859664d0,1.09139764183949d0,1.09775079905586d0,     &
                        1.08053412380867d0,1.03019099107063d0,0.990177617145289d0,1.01385841812283d0,0.994991558375902d0,   &
                        0.248554124278905d0/)

    a=bines(i,1); b=bines(i,2);
    renoExpectedNeutrinoSpectrumByBinFar=0.0d0
    h=(b-a)/real(n)
    do k=1,n
        x=a+h*real(k-1)   
        renoExpectedNeutrinoSpectrumByBinFar=renoExpectedNeutrinoSpectrumByBinFar       &
                                          + h*(  renoExpectedNeutrinoSpectrum_d(x,d)    &
                                                +renoExpectedNeutrinoSpectrum_d(x+h,d)  &                                                
                                        )/2.0d0
    enddo
    renoExpectedNeutrinoSpectrumByBinFar= binCalibrations(i) &
                                         *(0.491124039623128d0)*(0.2)*(renoExpectedNeutrinoSpectrumByBinFar/(b-a))!(0.2)*(renoExpectedNeutrinoSpectrumByBinFar/(b-a))
                                         
                                         
    return    
end function renoExpectedNeutrinoSpectrumByBinFar
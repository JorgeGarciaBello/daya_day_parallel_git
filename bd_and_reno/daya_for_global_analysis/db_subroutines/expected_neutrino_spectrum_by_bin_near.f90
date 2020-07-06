!#################################################################################################################
!
!       expectedNeutrinoSpectrumByBinNear: is the function that return 
!           the expected neutirno spectrum bay energy-bin and by near-hall
!
!#################################################################################################################
real(8) function expectedNeutrinoSpectrumByBinNear(i)
    use db_data, only: bines
    implicit none
    integer :: i                 ! is the number of energy bin

    real(8) :: expectedNeutrinoSpectrum_H !function that return the expected neutirno spectrum by hall    
    integer,parameter :: H1=1    ! number of hall
    integer,parameter :: H2=2    ! number of hall
    real(8) :: a,b               ! limits of the bin
    real(8) :: h
    integer :: k,n=400
    real(8) :: x
    
    a=bines(i,1); b=bines(i,2);
    expectedNeutrinoSpectrumByBinNear=0.0d0
    h=(b-a)/real(n)
    do k=1,n
        x=a+h*real(k-1)   
        expectedNeutrinoSpectrumByBinNear=expectedNeutrinoSpectrumByBinNear    &
                                          + h*(  expectedNeutrinoSpectrum_H(H1,x)   &
                                                +expectedNeutrinoSpectrum_H(H2,x)   &
                                                +expectedNeutrinoSpectrum_H(H1,x+h) &
                                                +expectedNeutrinoSpectrum_H(H2,x+h) &
                                        )/2.0d0
    enddo
    expectedNeutrinoSpectrumByBinNear=expectedNeutrinoSpectrumByBinNear/(b-a)
    return
end function expectedNeutrinoSpectrumByBinNear
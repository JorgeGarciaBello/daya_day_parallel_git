subroutine renoDetectorsTest()
    use types
    use reno_data, only: NBIN,nearObs,farObs   
    implicit none
    real(dp) :: t13, dmee
    integer  :: d
    real(dp) :: renoExpectedNumberNeutrinosDetector
    real(dp) :: renoExpectedNeutrinoSpectrumByBinNear
    real(dp) :: renoExpectedNeutrinoSpectrumByBinFar
    integer :: i


    print*, 'You are in the detector testing subrputine of RENO'
    print*, 'IBD OBS '//' AND '//' IBD THEORY '    
    print*, ''
    print*, ''
    print*, ''


    t13=asin(sqrt(0.096d0))/2.0_dp
    dmee=2.53d-3

    !t13=0.0d0
    !dmee=0.0d0
    print*,''
    print*,'Razón esperada sin oscilaciones'
    do i=1,NBIN
        print*, renoExpectedNeutrinoSpectrumByBinFar(i,t13,dmee), &
                renoExpectedNeutrinoSpectrumByBinNear(i,t13,dmee)
    enddo
    return
end subroutine renoDetectorsTest

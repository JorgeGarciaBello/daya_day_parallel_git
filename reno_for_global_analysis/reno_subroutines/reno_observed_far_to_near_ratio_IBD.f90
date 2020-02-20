!##################################################################
!
!   observedFarToNearRatioIBD: is the observed fat-to-near
!                              ratio of IBD candidates in the 
!                              i-th bin after background 
!                              subtraction
!
!##################################################################
real(8) function observedFarToNearRatioIBD(i)
    use reno_data, only: NBIN,nearObs,farObs
    implicit none
    integer :: i
        
    observedFarToNearRatioIBD=0.0
    observedFarToNearRatioIBD=farObs(i)/nearObs(i)
    return
end function observedFarToNearRatioIBD
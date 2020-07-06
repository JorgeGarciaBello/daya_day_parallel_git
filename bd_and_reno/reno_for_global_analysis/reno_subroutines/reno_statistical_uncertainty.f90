!##################################################################
!
!   statisticalUncertainty: is the statistical uncertainty 
!                           of the observerd far-to-near 
!                           ratio of IBD
!
!##################################################################
real(8) function statisticalUncertainty(i)
    use reno_data, only: farObs,nearObs,sigmaNear,sigmaFar
    implicit none    
    integer :: i    
    
    real(8) :: observedFarToNearRatioIBD !is the observed fat-to-near ratio of IBD candidates in the i-th bin after background subtraction
    real(8) :: O_i
    
    statisticalUncertainty=0.0
    O_i = observedFarToNearRatioIBD(i)
    statisticalUncertainty=(O_i)*sqrt( (sigmaFar(i)/farObs(i))**2 + (sigmaNear(i)/nearObs(i))**2 )    
    return
end function statisticalUncertainty
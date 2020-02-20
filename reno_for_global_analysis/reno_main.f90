!#############################################
!
!               R E N O
!   Reactor Experiment Netrino Oscillations
!
!#############################################
program main
    use reno_data, only:NDIM
    use neu_osc_parameters
    IMPLICIT REAL*8 (A-H,O-Z)    
    PARAMETER(MP=NDIM+1,NP=NDIM)          ! NP is the number of pulls, MP is the number of points for polygon of ABOEBA

    real(8) :: averageProbabilityOfSurvival_dr_bin,probability,leng,x,FUNC
    real(8) :: P(NDIM)            ! Parámetros de oscilacióna determinar
    call readRENOData()
    
    !####################################################

    !call renoDetectorsTest()

    !call gridMinLog()
    call gridMinInFocus()
    !call confidenceRegionsWithPullsFixed()

    
    
end program main


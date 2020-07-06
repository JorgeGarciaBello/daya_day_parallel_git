!##################################################################
!
!   model: is a function that contains the 
!          model of neutrino oscillations
!
!##################################################################
real(8) function reno_model(d,bin)
    use reno_data, only: RCTS,wDR    
    implicit none    
    integer :: d                         ! d is the number of detector {1 for near / 2 for far}    
    integer :: bin                       ! bin is the number (integer) of the ith-bin    
    
    real(8) :: reno_averageProbabilityOfSurvival_dr_bin
    integer :: r                         ! r is the reactor's number

    reno_model=0.0d0
    do r=1,RCTS        
        reno_model=reno_model+wDR(d,r)*reno_averageProbabilityOfSurvival_dr_bin(d,r,bin)
    enddo
    return
end function reno_model
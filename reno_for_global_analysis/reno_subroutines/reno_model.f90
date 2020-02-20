!##################################################################
!
!   model: is a function that contains the 
!          model of neutrino oscillations
!
!##################################################################
real(8) function model(d,bin)
    use reno_data, only: RCTS,wDR    
    implicit none    
    integer :: d                         ! d is the number of detector {1 for near / 2 for far}    
    integer :: bin                       ! bin is the number (integer) of the ith-bin    
    
    real(8) :: averageProbabilityOfSurvival_dr_bin
    integer :: r                         ! r is the reactor's number

    model=0.0d0
    do r=1,RCTS        
        model=model+wDR(d,r)*averageProbabilityOfSurvival_dr_bin(d,r,bin)
    enddo
    return
end function model
subroutine reno_create_statistical_covariance_matrix(t13,dmee,Vstat)
    use types
    use reno_data, only: NBIN,farObs
    implicit none
    real(dp) :: t13,dmee,Vstat(NBIN,NBIN)    
    integer  :: i,u

    Vstat=0.0d0    
    do i=1,NBIN        
        Vstat(i,i)=farObs(i)
    enddo    
    return
end subroutine reno_create_statistical_covariance_matrix
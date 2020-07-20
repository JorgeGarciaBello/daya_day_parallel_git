function reno_M_Total_ij(t13,dmee,i,j,Nbar_i,U)
    use types
    use reno_data, only: NBIN,num_experiments,nearObs,farObs,rand_Nbkgtotal
    implicit none
    real(dp) :: reno_M_Total_ij
    real(dp) :: t13,dmee
    integer  :: i,j,k    
    real(dp) :: Nbar_i(NBIN,num_experiments),U(NBIN)    
    ! Obtención la entrada ij de la matriz M de la incertidumbre en Accidental-Background
    reno_M_Total_ij=0.0d0
    do k=1,num_experiments
        reno_M_Total_ij=reno_M_Total_ij+(  Nbar_i(i,k) - U(i)  )  *  ( Nbar_i(j,k) - U(j) )
    enddo
    reno_M_Total_ij=reno_M_Total_ij/real(num_experiments)
    return
end function reno_M_Total_ij
subroutine reno_create_M_stat_signal_bkg(t13,dmee,U,V)
    use types
    use reno_data, only: NBIN,num_experiments
    implicit none
    real(dp) :: t13,dmee    
    real(dp) :: Nbar_i(NBIN,num_experiments)
    real(dp) :: U(NBIN),V(NBIN,NBIN)    
    real(dp) :: Vstat(NBIN,NBIN)
    real(dp) :: reno_M_Total_ij
    integer  :: i,j
    
    call reno_generate_vectors_U(t13,dmee,Nbar_i,U)
    do i=1,NBIN
        do j=1,NBIN
            V(i,j)=reno_M_Total_ij(t13,dmee,i,j,Nbar_i,U)
        enddo
    enddo
    call reno_create_statistical_covariance_matrix(t13,dmee,Vstat)
    V=V+Vstat    
    call write_matrix(NBIN,V,'reno_M_stat_signal_bkg')
    call write_matrix_m_n(NBIN,V,'reno_M_M_stat_signal_bkg')
    call db_get_inverse_covariance_matrix(NBIN,V)
    call write_matrix(NBIN,V,"M_inv_stat_signal_bkg")
    return
end subroutine reno_create_M_stat_signal_bkg
subroutine reno_generate_vectors_U(t13,dmee,Nbar_i,U)
    use types
    use reno_data, only: NBIN, num_experiments,nearObs,farObs,rand_Nbkgtotal
    implicit none
    real(8) :: t13,dmee
    real(8) :: rand_w_i(NBIN,num_experiments)    
    real(8) :: Nbar_i(NBIN,num_experiments)
    real(8) :: U(NBIN)
    integer :: i,k
    ! Obtención de vectores Ubar
    call reno_generate_weight_w_systematic_array(t13,dmee,rand_w_i)
    do i=1,NBIN
        !$omp parallel do
        do k=1,num_experiments
            Nbar_i(i,k)= farObs(i) - (  rand_w_i(i,k)*( nearObs(i) - rand_Nbkgtotal(i,1,k) ) + rand_Nbkgtotal(i,2,k) )       
        enddo
        !$omp end parallel do
        ! OBtención del promedio de los vectores Nbar
        U(i)=sum(Nbar_i(i,:))/real(num_experiments)
    enddo    
    return
end subroutine reno_generate_vectors_U
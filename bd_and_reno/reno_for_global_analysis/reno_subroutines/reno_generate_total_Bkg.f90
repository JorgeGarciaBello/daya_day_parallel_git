subroutine reno_generate_total_Bkg()
    use types
    use reno_data, only: NBIN,randDis,num_experiments,bkg,sigma_background_d,rand_Nbkgtotal
    implicit none     
    real(dp) :: min_n, max_n, min_f, max_f
    real(dp) :: r
    integer :: i,n
    
    !sigma_background_d=0.0d0
    
    do i=1,NBIN
        !min_n=bkg(i,1) - bkg(i,1)*(sigma_background_d(1)/100.0_dp)
        !max_n=bkg(i,1) + bkg(i,1)*(sigma_background_d(1)/100.0_dp)

        !min_f=bkg(i,2) - bkg(i,2)*(sigma_background_d(2)/100.0_dp)
        !max_f=bkg(i,2) + bkg(i,2)*(sigma_background_d(2)/100.0_dp)

        min_n= - bkg(i,1)*(sigma_background_d(1)/100.0_dp)
        max_n=   bkg(i,1)*(sigma_background_d(1)/100.0_dp)

        min_f= - bkg(i,2)*(sigma_background_d(2)/100.0_dp)
        max_f=   bkg(i,2)*(sigma_background_d(2)/100.0_dp)

        do n=1,num_experiments
            CALL RANDOM_NUMBER(r)
            rand_Nbkgtotal(i,1,n) = min_n*(1.0d0-r) + max_n*r
            CALL RANDOM_NUMBER(r)            
            rand_Nbkgtotal(i,2,n) = min_f*(1.0d0-r) + max_f*r
        enddo
    enddo
    do i=1,10
        print*,rand_Nbkgtotal(1,1,i),rand_Nbkgtotal(26,1,i),rand_Nbkgtotal(1,2,i),rand_Nbkgtotal(26,2,i)
    enddo
    return
end subroutine reno_generate_total_Bkg
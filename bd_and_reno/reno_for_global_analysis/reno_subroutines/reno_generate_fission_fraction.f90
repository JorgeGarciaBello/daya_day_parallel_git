subroutine reno_generate_fission_fraction()
    use types, only: dp
    use reno_data, only: NBIN,randDis,RIR,num_experiments, &
                       FNF,sigma_fission_fraction,rand_Nfissionfraction                                                   
    implicit none    
    real(dp) :: min_p1(RIR), max_p1(RIR)
    real(dp) :: ZBQLNOR
    real(dp) :: r
    integer  :: i,j,n

    select case(randDis)
    case(1)
        do i=1,NBIN
            do n=1,num_experiments
                do j=1,RIR                    
                    rand_Nfissionfraction(i,j,n) = &
                    ZBQLNOR(FNF(j),FNF(j)*(sigma_fission_fraction(j)/100.0_dp))                    
                enddo
            enddo
        enddo
    case(2)
        do i=1,RIR
            min_p1(i)=FNF(i)-FNF(i)*(sigma_fission_fraction(i)/100.0_dp)
            max_p1(i)=FNF(i)+FNF(i)*(sigma_fission_fraction(i)/100.0_dp)            
        enddo
        do i=1,NBIN
            do n=1,num_experiments
                do j=1,RIR
                    CALL RANDOM_NUMBER(r)            
                    rand_Nfissionfraction(i,j,n) = min_p1(j)*(1.0d0-r) + max_p1(j)*r
                enddo            
            enddo
        enddo
    end select
    !do i=1, num_experiments
    !    print*, rand_Nfissionfraction(1,1,i),rand_Nfissionfraction(1,2,i), rand_Nfissionfraction(1,3,i), &
    !            rand_Nfissionfraction(1,4,i)    
    !enddo
    return
end subroutine reno_generate_fission_fraction
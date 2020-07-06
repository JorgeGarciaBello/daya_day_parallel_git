subroutine reno_generate_reactor_flux_model()
    use types, only: dp
    use reno_data, only: NBIN,randDis,num_experiments,reactor_flux_model,sigma_reactor_flux_model,rand_Nreactorfluxmodel                                                    
    implicit none
    real(dp) :: min_val(NBIN), max_val(NBIN)
    real(dp) :: ZBQLNOR
    real(dp) :: r
    integer :: i,n
    select case (randDis)
        case(1)
            do i=1,NBIN
                do n=1,num_experiments                    
                    rand_Nreactorfluxmodel(i,n)=ZBQLNOR(reactor_flux_model(i), &
                                                   reactor_flux_model(i)*(sigma_reactor_flux_model(i)/100.0_dp))                   
                enddo
            enddo
        case(2)
            do i=1,NBIN
                min_val(i)= reactor_flux_model(i)-reactor_flux_model(i)*(sigma_reactor_flux_model(i)/100.0_dp)
                max_val(i)= reactor_flux_model(i)+reactor_flux_model(i)*(sigma_reactor_flux_model(i)/100.0_dp)                
            enddo
            do i=1,NBIN
                do n=1,num_experiments
                    CALL RANDOM_NUMBER(r)
                    rand_Nreactorfluxmodel(i,n) = min_val(i)*(1.0d0-r) + max_val(i)*r                    
                enddo
            enddo
    end select   
    !print*, '********'
    !do i=1, num_experiments
    !    print*, rand_Nreactorfluxmodel(1,i), rand_Nreactorfluxmodel(26,i)                
    !enddo
    !print*, '********'
    return
end subroutine reno_generate_reactor_flux_model
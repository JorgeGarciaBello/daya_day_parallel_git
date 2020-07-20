subroutine reno_generate_thermal_power()
    use types, only: dp
    use reno_data, only: NBIN,randDis,RCTS,num_experiments,                           &
                         TP_r,sigma_thermal_power,rand_Nthermalpower                       
    implicit none    
    real(dp) :: min_p1(RCTS),max_p1(RCTS)    
    real(dp) :: ZBQLNOR
    real(dp) :: r
    integer  :: i,j,n

    !sigma_thermal_power=0.0d0
    select case(randDis)
    case(1)
        do i=1,NBIN
            do n=1,num_experiments
                rand_Nthermalpower(i,1,n)=ZBQLNOR(TP_r(1),TP_r(1)*(sigma_thermal_power(1)/100.0_dp))
                rand_Nthermalpower(i,2,n)=ZBQLNOR(TP_r(2),TP_r(2)*(sigma_thermal_power(2)/100.0_dp))
                rand_Nthermalpower(i,3,n)=ZBQLNOR(TP_r(3),TP_r(3)*(sigma_thermal_power(3)/100.0_dp))
                rand_Nthermalpower(i,4,n)=ZBQLNOR(TP_r(4),TP_r(4)*(sigma_thermal_power(4)/100.0_dp))
                rand_Nthermalpower(i,5,n)=ZBQLNOR(TP_r(5),TP_r(5)*(sigma_thermal_power(5)/100.0_dp))
                rand_Nthermalpower(i,6,n)=ZBQLNOR(TP_r(6),TP_r(6)*(sigma_thermal_power(6)/100.0_dp))                
            enddo
        enddo
    case(2)
        do i=1,RCTS
            min_p1(i)=TP_r(i)-TP_r(i)*(sigma_thermal_power(i)/100.0_dp)
            max_p1(i)=TP_r(i)+TP_r(i)*(sigma_thermal_power(i)/100.0_dp)
        enddo
        do i=1,NBIN
            do n=1,num_experiments
                do j=1,RCTS
                    CALL RANDOM_NUMBER(r)
                    rand_Nthermalpower(i,j,n) = min_p1(j)*(1.0d0-r) + max_p1(j)*r                    
                enddo            
            enddo
        enddo
    end select    
    !do i=1,100
    !    print*, rand_Nthermalpower(1,1,i),rand_Nthermalpower(1,2,i), rand_Nthermalpower(1,3,i), &
    !            rand_Nthermalpower(1,4,i), rand_Nthermalpower(1,5,i), rand_Nthermalpower(1,6,i)        
    !enddo
    return
end subroutine reno_generate_thermal_power
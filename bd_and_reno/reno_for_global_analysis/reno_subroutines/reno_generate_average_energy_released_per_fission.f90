subroutine reno_generate_average_energy_released_per_fission()
    use types, only: dp
    use reno_data, only: NBIN,randDis,num_experiments, &
                        average_energy_released_per_fission,sigma_average_energy_released_per_fission, &
                        rand_Navrgenergyperfission
    implicit none    
    real(dp) :: min, max    
    real(dp) :: ZBQLNOR
    real(dp) :: r
    integer :: i,n
    
    select case(randDis)
    case(1)
        do i=1,NBIN
            do n=1,num_experiments
                rand_Navrgenergyperfission(i,1,n) = &
                            ZBQLNOR(average_energy_released_per_fission, &
                                    average_energy_released_per_fission*(sigma_average_energy_released_per_fission/100.0_dp))                
            enddo
        enddo
    case(2)
        min=average_energy_released_per_fission &
              -average_energy_released_per_fission*(sigma_average_energy_released_per_fission/100.0_dp)
        max=average_energy_released_per_fission &
              +average_energy_released_per_fission*(sigma_average_energy_released_per_fission/100.0_dp)
        do i=1,NBIN
            do n=1,num_experiments
                CALL RANDOM_NUMBER(r)
                rand_Navrgenergyperfission(i,1,n) = min*(1.0d0-r) + max*r                
            enddo
        enddo       
    end select
    !do i=1, num_experiments
    !    print*, rand_Navrgenergyperfission(1,1,i)
    !enddo
    return
end subroutine reno_generate_average_energy_released_per_fission
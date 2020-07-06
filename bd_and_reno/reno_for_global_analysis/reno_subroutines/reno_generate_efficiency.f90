subroutine reno_generate_efficiency()
    use types, only: dp
    use reno_data, only: ADS,NBIN,num_experiments,randDis, &
                               detector_efficiency,sigma_detection_efficiency,sigma_efficiency_corr,rand_Nefficiency


    ! El error de sigma_detection_efficiency parece ser el total correlated y uncorrelated
    ! Se debe realizar prueba de reigones con los valores uncorrelated = 0.16 y correlated=0.13
    implicit none    
    real(dp) :: min_val(ADS), max_val(ADS)    
    real(dp) :: ZBQLNOR
    real(dp) :: r
    integer :: i,n,d
    
    select case(randDis)
        case(1)
            do i=1,NBIN
                do n=1,num_experiments
                    do d=1,ADS
                    rand_Nefficiency(i,d,n) = ZBQLNOR(detector_efficiency(d), detector_efficiency(d)                      &
                                            *( (sigma_detection_efficiency+sigma_efficiency_corr(d))/100.0_dp )   )
                    enddo                   
                enddo
            enddo
        case(2)
          do d=1,ADS
            min_val(d)=detector_efficiency(d)-detector_efficiency(d)*            &
                                              ( (sigma_detection_efficiency + sigma_efficiency_corr(d))/100.0_dp)
            max_val(d)=detector_efficiency(d)+detector_efficiency(d)*                           &
                                              ( (sigma_detection_efficiency + sigma_efficiency_corr(d))/100.0_dp)
          enddo
           do i=1,NBIN
              do n=1,num_experiments
                  do d=1,ADS
                      CALL RANDOM_NUMBER(r)
                      rand_Nefficiency(i,d,n) = min_val(d)*(1.0d0-r) + max_val(d)*r               
                  enddo
              enddo
          enddo

    end select
    !do i=1,100
    !  print*, rand_Nefficiency(1,1,i), rand_Nefficiency(26,2,i)    
    !enddo
    return
end subroutine reno_generate_efficiency
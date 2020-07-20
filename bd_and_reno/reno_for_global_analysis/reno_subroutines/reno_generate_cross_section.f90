subroutine reno_generate_cross_section()
    use types, only: dp
    use reno_data, only: NBIN,randDis,num_experiments, &
                         cross_section_v,sigma_cross_section,rand_Ncrosssection
                                                    
    implicit none
    REAL(dp) :: ZBQLNOR
    real(dp) :: min_val(NBIN), max_val(NBIN)    
    real(dp) :: r
    integer :: i,n
    !sigma_cross_section=0.0d0
    select case(randDis)
        case(1)
            do i=1,NBIN
                do n=1,num_experiments                    
                    rand_Ncrosssection(i,1,n) = ZBQLNOR(cross_section_v(i), &
                                                   cross_section_v(i)*(sigma_cross_section(i)/100.0_dp))                    
                enddo
            enddo
        case(2)
            do i=1,NBIN       
                min_val(i)=cross_section_v(i)-cross_section_v(i)*( sigma_cross_section(i)/100.0_dp )
                max_val(i)=cross_section_v(i)+cross_section_v(i)*( sigma_cross_section(i)/100.0_dp )                
            enddo
            do i=1,NBIN
                do n=1,num_experiments
                    CALL RANDOM_NUMBER(r)
                    rand_Ncrosssection(i,1,n) = min_val(i)*(1.0d0-r) + max_val(i)*r                    
                enddo
            enddo
    end select    
    !do i=1, num_experiments
    !     print*, rand_Ncrosssection(1,1,i),rand_Ncrosssection(26,1,i),rand_Ncrosssection(27,1,i)
    !enddo
    return
end subroutine reno_generate_cross_section
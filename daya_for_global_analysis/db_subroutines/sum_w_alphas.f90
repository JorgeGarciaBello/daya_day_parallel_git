real(8) function sum_W_alp(d,alphaR)
    use db_data, only: Wdr=>IBD_fdr
    implicit none
    integer :: d                  ! is the number of detector    
    real(8) :: alphaR(6)          ! is an array with pull parameters reactor

    integer :: r                  ! is an index over the reactor number

    sum_W_alp=0.0d0
    do r=1,6
        sum_W_alp=sum_W_alp+Wdr(d,r)*alphaR(r)
    enddo
    return
end function sum_W_alp
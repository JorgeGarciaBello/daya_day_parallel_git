subroutine reno_reWriteSimplex(newSimplex)
    use reno_data, only: NDIM
    implicit none
    real(8) :: newSimplex(NDIM+1,NDIM)
    integer :: i

    open(200,file='reno_for_global_analysis/reno_data/reno_simplex.dat',status='old')
        do i=1,NDIM+1
            write(200,*) newSimplex(i,:)
        enddo
    close(200)

    return
end subroutine reno_reWriteSimplex
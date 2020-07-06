subroutine reWriteSimplex(newSimplex)
    use db_data, only: NDIM
    implicit none
    real(8) :: newSimplex(NDIM+1,NDIM)
    integer :: i

    open(200,file='daya_for_global_analysis/db_data/db_simplex.dat',status='old')
        do i=1,NDIM+1
            write(200,*) newSimplex(i,:)
        enddo
    close(200)

    return
end subroutine reWriteSimplex
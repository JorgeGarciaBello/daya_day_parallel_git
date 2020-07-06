!#####################################################
!
!   confidenceRegions: es una subroutina que 
!       genera los archivos de datos para las 
!       crear las regiones de confianza
!
!#####################################################
subroutine reno_confidenceRegions(n,val,data,filename)
    implicit none
    integer :: n
    real(8) :: val(3)    
    real(8) :: data(n,3)
    Character(len=*) :: filename
    integer  :: i,j,u,uu,uuu
    real(8) :: s13,m_ee,chi_2
    
    print*, 'making confidence regions . . . '
    open(newunit=u,file='data/reno_cr_99_73_fortran.dat')
    open(newunit=uu,file='data/reno_cr_95.45_fortran.dat')
    open(newunit=uuu,file='data/reno_cr_68_27_fortran.dat')
    
    do i=1,n
        s13=data(i,1); m_ee=data(i,2); chi_2=data(i,3)-val(1)
        !Confidence region 1 - 68.27% - 2.30 - (1-sigma)
        if(chi_2 <= 2.3d0) then
            write(u,*) s13, m_ee
        endif
        !Confidence region 2 - 95.45% - 6.18 - (2-sigma)
        if(chi_2 <= 6.18d0) then
            write(uu,*) s13, m_ee            
        endif
        !Confidence region 3 - 99.73% - 11.83 (3-sigma)
        if(chi_2 <= 11.83d0) then
            write(uuu,*) s13, m_ee            
        endif
    enddo
    close(u)
    close(uu)
    close(uuu)
    return
end subroutine reno_confidenceRegions
!################################################################
!
!   readSimplex: is the subroutine that read
!                   the initial simplex and 
!                   initialize the NDIM+1 polygon
!################################################################
subroutine reno_readSimplex(t13,dmee,P,Y)
    use types
    use reno_data, only:NDIM
    implicit none
    integer,parameter :: MP=NDIM+1                    ! MP is the number of points for polygon of ABOEBA
    integer,parameter :: NP=NDIM                      ! NP is the number of pulls                   
    real(dp),dimension(MP,NP) :: P          ! P is the set of MP points with NP coordenates that generate the polygon of AMOEBA
    real(dp),dimension(MP)    :: Y
    real(dp),dimension(NDIM)  :: PT
    real(dp) :: t13,dmee
             
    real(dp) :: reno_FUNC
    integer :: m,k,l,i,u
    character(100) :: filename
    ! Lectura de NDIM+1 puntos con NDIM coordenadas en readSimplex
    filename='reno_simplex.dat'
    ! Leer el array de pulls 
    ! Se leer NDIM+1 vérties
    ! con NDIM coordenadas
    ! del archivo "filename"
    open(newunit=u,file="reno_for_global_analysis/reno_data/"//filename, status="old")
    !open(20,file="dataService/PRUEBA_AMOEBA.dat", status="old")
        read(u,*) ( (P(k,l), l=1,NP), k=1,MP )
    close(u)
    
    DO m=1, NDIM+1
        ! Se carga la i-ésima coordenada del m-ésimo punto al arreglo PT
        do i=1,NDIM
            PT(i)=P(m,i)
        enddo
        Y(m)=reno_FUNC(t13,dmee,PT)
    END DO    
    return
end subroutine reno_readSimplex
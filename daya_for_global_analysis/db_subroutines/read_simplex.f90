!################################################################
!
!   readSimplex: is the subroutine that read
!                   the initial simplex and 
!                   initialize the NDIM+1 polygon
!################################################################
subroutine readSimplex(P,Y)
    use db_data, only:NDIM
    !use omp_lib
    implicit none    
    integer,parameter :: MP=NDIM+1                    ! MP is the number of points for polygon of ABOEBA
    integer,parameter :: NP=NDIM                      ! NP is the number of pulls                   
    real(8),dimension(MP,NP) :: P          ! P is the set of MP points with NP coordenates that generate the polygon of AMOEBA
    real(8),dimension(MP)    :: Y
    real(8),dimension(NDIM)  :: PT
             
    real(8) :: FUNC
    integer :: m,k,l,i
    character(100) :: filename
    ! Lectura de NDIM+1 puntos con NDIM coordenadas en readSimplex
    
    filename='db_simplex.dat'
    ! Leer el array de pulls 
    ! Se leer NDIM+1 vérties
    ! con NDIM coordenadas
    ! del archivo "filename"
    !open(34,file="data/dataService/"//filename, status="old")
    open(35,file="db_data/db_simplex.dat", status="old")
    !open(20,file="dataService/PRUEBA_AMOEBA.dat", status="old")
        read(35,*) ( (P(k,l), l=1,NP), k=1,MP )
    close(35)

    DO m=1, NDIM+1
        ! Se carga la i-ésima coordenada del m-ésimo punto al arreglo PT    
        do i=1,NDIM
            PT(i)=P(m,i)
        enddo        
        Y(m)=FUNC(PT)
    END DO    
    return
end subroutine readSimplex
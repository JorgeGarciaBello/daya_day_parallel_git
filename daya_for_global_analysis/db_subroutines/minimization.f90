!#####################################################
!
!   minimization es una subroutina que se encargar de 
!       ejecutar las subroutinas paa realizar la 
!       minimización de la función FUNC, y escribe
!       los resultaods en un archivo.
!
!#####################################################
subroutine minimization(Y,P)
    use db_data, only:NDIM,pull_min
    implicit none    
    integer,parameter :: MP=NDIM+1                ! MP is the number of points for polygon of ABOEBA
    integer,parameter :: NP=NDIM                  ! NP is the number of pulls                   
    real(8) :: P(MP,NP), Y(MP)          ! Array con el que se construye AMOEBA    
    real(8) :: FTOL=1.D-5               ! Required tolerance
    integer :: ITER                     ! Número de iteraciones realizadas por AMOEBA

    real(8) :: newSimplex(MP,NDIM)
    integer :: i
    
    call readSimplex(P,Y)
    do i=1,NDIM
        newSimplex(i,:)=P(i,:)
    end do

    CALL AMOEBA(P,Y,MP,NP,NDIM,FTOL,ITER)
    pull_min=P(1,:)

    newSimplex(MP,:)=P(1,:)
    call reWriteSimplex(newSimplex)
    return
end subroutine minimization
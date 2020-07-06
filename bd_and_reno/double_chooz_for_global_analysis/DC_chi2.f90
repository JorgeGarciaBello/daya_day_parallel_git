!##############################################################################################
!
!   Instituto Politécnico Nacional. 
!   Escuela Superior de Física y Matemáticas
!   
!   Autor: M. en C. Benito Vargaz Perez
!
!   chi2_D_C. Subroutina del experimento de oscilaciones de neutrinos Double CHOOZ.
!       Tiene la estructura ( Vector Y: son los parámetros de oscilación en un modelo de 4 neutrinos,
!                             chi_doubleCHOOZ: es el valor minimo de la estadśitica chi-square para
!                                       valores definidos en el vector Y )
!
!   Descripción =>
!
!        Vector Y(12): t12 es el ángulo de mezcla theta_12
!                    t13 es el ángulo de mezcla theta_13
!                    t14 es el ángulo de mezcla theta_14
!                    t23 es el ángulo de mezcla theta_23
!                    t24 es el ángulo de mezcla theta_24
!                    t34 es el ángulo de mezcla theta_34
!                    delta1 es el factor de fase la violación de CP_13
!                    delta2 es el factor de fase la violación de CP_24
!                    delta3 es el factor de fase la violación de CP_34
!                    dm21 es la direfencia del cuadrado de las masas Solar (m^2_2 - m^2_1)
!                    dm31 es la direfencia del cuadrado de las masas       (m^2_3 - m^2_1)
!                    dm41 es la direfencia del cuadrado de las masas       (m^2_4 - m^2_1)
!
!       chi_doubleCHOOZ:    Es el valor minimo develto de la estadística chi utilizada en daya bay  
!                    para valores definidos en Y (parámetros de osciulación)
!
!##############################################################################################
!##################################################
!
!   subroutine chi2_D_C: subroutine que recibe un 
!       vector Y de parámetros de oscilación y 
!       regresa un chi_doubleCHOOZ que es el Valor
!       mínimo de la estadśitica Chi-cuadrada del 
!       experimento Double CHOOZ
!
!##################################################


Subroutine chi2_D_C(Y,chi_doubleCHOOZ)  !Calcula las integrales Ppromedio
    use DC_espectros
    use DC_condiciones
    use matriz
    implicit none
    real*8, dimension(12) :: Y                                ! Vector Y que contiene parámetros de oscilación
    real(8) :: chi_doubleCHOOZ                                ! Valor minimo de chi para Double CHOOZ

    real*8 chi2_DC
    real*8, dimension(9,8) :: PP
    real*8, dimension(9) :: YY !chi^2 del simplex
    real*8, dimension(8) :: entrada
    integer k,kk    
    
    CALL parametros(deltachi,ciclos,region,diagon,mm)
    CALL Invermat(M,diagon)    
    !###########################################
    !
    !   Leyendo el simplex para double chooz
    !
    !###########################################
    open(53,file='double_chooz_for_global_analysis/dc_data/DC_simplex.dat')
        read (53,*) PP
    close(53)
     !###########################################
    !
    !   Inicializando chi2_DC conel simplex
    !
    !############################################
    DO k=1,9
        do kk=1,8
            entrada(kk)=PP(k,kk)    !  (Puntos del simplex)
        end do
        YY(k)=chi2_DC(Y,entrada)
    END DO
     !###########################################
    !
    !   Minimizando chi2 de Double CHOOZ
    !
    !############################################
    CALL AMOEBA_DC(PP,YY,Y)
    
    chi_doubleCHOOZ = YY(1)
    
    58 format (3(E16.10,12X))
    CALL SaveP_DC(PP)
    return
end Subroutine chi2_D_C

!##################################################################
!
!   ProbabilityOfSurvival: is the probability of survival
!       for a model of neutrino oscillations with a effective
!       mass difference delta_ee
!
!##################################################################
real(8) function ProbabilityOfSurvival(x, leng, t13, m13)
    implicit none    
    real(8) :: t13, m13
    real(8) :: x    ! x is the neutrino energy in [MeV]
    real(8) :: leng ! leng is the length between reactor-detector                                               ! leng es la distancia entre r_d en Metros 
    real(8) :: m21=7.53D-5, m32= 2.44D-3 !Jerarquia normal ! m32= 2.52E-3 !Jerarquia invertida
    real(8) :: s2_2t12=0.846D0, c2_t12=0.6962D0, s2_t12=0.3038D0
    
    ProbabilityOfSurvival = 1.D0 - (cos(t13)**4)*s2_2t12*(sin(1.267D0*m21*leng/x)**2) &    
                                 - (sin(2.0D0*t13)**2)*(sin(1.267D0*m13*leng/x)**2) 
                              
    !ProbabilityOfSurvival = 1.D0 - (cos(t13)**4)*s2_2t12*(sin(1.267D0*m21*leng/x)**2) &
     !                           - (sin(2.0D0*t13)**2)*c2_t12*(sin(1.267D0*m13*leng/x)**2) &
      !                         - (sin(2.0D0*t13)**2)*s2_t12*(sin(1.267D0*m32*leng/x)**2)
    !ProbabilityOfSurvival = 1.D0 - sin(2.0D0*t13)**2*sin(1.267D0*m13*leng/x)**2
    return
end function ProbabilityOfSurvival ! [SIN UNIDADES]
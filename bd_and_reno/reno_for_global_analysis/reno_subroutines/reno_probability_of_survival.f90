!##################################################################
!
!   reno_probability_of_survival: is the probability of survival
!       for a model of neutrino oscillations with a effective
!       mass difference delta_ee
!
!##################################################################
function reno_probability_of_survival(x, leng, t13, dmee)
    use types
    implicit none    
    real(dp) :: reno_probability_of_survival
    real(dp) :: x,t13, dmee    
    real(dp) :: leng ! leng is the length between reactor-detector                                               ! leng es la distancia entre r_d en Metros 
    real(dp) :: m21=7.53D-5, m32= 2.44D-3 !Jerarquia normal ! m32= 2.52E-3 !Jerarquia invertida
    real(dp) :: s2_2t12=0.846D0, c2_t12=0.6962D0, s2_t12=0.3038D0
    
    reno_probability_of_survival = 1.D0 - (cos(t13)**4)*s2_2t12*(sin(1.267D0*m21*leng/x)**2) &    
                                        - (sin(2.0D0*t13)**2)*(sin(1.267D0*dmee*leng/x)**2)
    return
end function reno_probability_of_survival
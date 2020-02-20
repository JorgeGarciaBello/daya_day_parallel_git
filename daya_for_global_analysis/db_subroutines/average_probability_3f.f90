real(8) function averageProbability3f(bin,l,t13,dm31)
    use db_data, only: pullEngy
    implicit none
    real(8) :: bin(2)
    real(8) :: l
    real(8) :: t13
    real(8) :: dm31

    real(8) :: probability       ! is the probability of transition for a model of neutrino oscillations with a 3-framework
    real(8) :: x                 ! x es la energía del neutrino en MeV0    
    real(8) :: a,b,h
    integer :: i,n=100!400

    a=bin(1) ; b=bin(2);
    h = (b-a)/real(n)   
    averageProbability3f=0.d0
    do i=1,n
        x = a + h*real(i-1)
        x = x*(1.0d0 + pullEngy)
        averageProbability3f = averageProbability3f &
            + h*( probability(t13,dm31,l,x)     &
                + probability(t13,dm31,l,x+h))  &
                / real(2)
    enddo
    averageProbability3f = averageProbability3f/(b-a)
    return
end function averageProbability3f
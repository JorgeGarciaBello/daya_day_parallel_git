function DC_probability_of_survival(x,l,t13,dmee)
    use types
    implicit none
    real(dp) :: DC_probability_of_survival
    real(dp) :: x,l
    real(dp) :: t13
    real(dp) :: dmee

    DC_probability_of_survival=1.0_dp-(sin(2.0_dp*t13)**2*sin(1.267_dp*dmee*l/x)**2)

    return
end function DC_probability_of_survival
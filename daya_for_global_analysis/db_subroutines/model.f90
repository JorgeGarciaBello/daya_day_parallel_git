real(8) function model(i)
use db_data, only: bines,ADS,RCTS,gFactor,length_d_r
use neu_osc_parameters
    implicit none
    integer :: i
    real(8) :: averageProbability3f
    real(8) :: bin(2)
    real(8) :: l
    integer :: d,r
    real(8) :: wegthsDays(4)

    !t13=0.1472d0
    !dm31=2.5d-3

    wegthsDays=(/0.256929262781914,0.256929262781914,0.256929262781914,0.229212211654259/)
    bin=bines(i,:)
    model=0.0d0
    do d=5,ADS
        do r=1,RCTS
            l=length_d_r(d,r)            
            model=model+wegthsDays(d - 4)*gFactor(d-4,r)*averageProbability3f(bin,l,t13,dm31)
        enddo        
    enddo
    return
end function model

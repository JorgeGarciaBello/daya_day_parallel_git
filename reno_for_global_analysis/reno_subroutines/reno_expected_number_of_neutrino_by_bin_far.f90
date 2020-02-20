real*8 function renoExpectedNumberNeutrinoByBinFar(i)
    use reno_data, only: bines,NBIN
    implicit none   
    integer :: i                          ! is the number of energy bin
    real*8  :: renoExpectedNeutrinoSpectrumByBinFar ! is the function that return the expected neutirno spectrum bay energy-bin and by far-hall
    real(8) :: a,b                        ! limits of the bin
        
   a=bines(i,1); b=bines(i,2)

   renoExpectedNumberNeutrinoByBinFar=0.0d0
   renoExpectedNumberNeutrinoByBinFar=(renoExpectedNeutrinoSpectrumByBinFar(i)*(b-a))

   return
end function renoExpectedNumberNeutrinoByBinFar

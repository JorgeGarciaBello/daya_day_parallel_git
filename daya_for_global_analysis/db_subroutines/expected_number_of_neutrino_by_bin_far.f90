real*8 function expectedNumberNeutrinoByBinFar(i)
    use db_data, only: bines,NBIN
    implicit none   
    integer :: i                          ! is the number of energy bin
    real*8  :: expectedNeutrinoSpectrumByBinFar ! is the function that return the expected neutirno spectrum bay energy-bin and by far-hall
    real(8) :: a,b                        ! limits of the bin
    real(8) :: corrections(NBIN)

    corrections=(/  -103.89657047632,-270.986517925699,-269.163615503501,-0.115712597100355,413.451348487502,   &
                    778.010534368801,1194.8691188513,1316.8147970454,1528.6982103257,1406.0861735423,           &
                    1437.40515863,1202.9486114568,1145.0292036031,906.254070548701,825.2123766029,              &
                    629.123186767101,553.8776463216,438.147934018602,316.099403903941,261.0386925381,           &
                    177.620828072,136.06228732178,84.03840978363,57.08196822635,30.4644193224901,               &
                    32.45978378823/)
   a=bines(i,1); b=bines(i,2)

   expectedNumberNeutrinoByBinFar=0.0d0
   expectedNumberNeutrinoByBinFar=(expectedNeutrinoSpectrumByBinFar(i)*(b-a))! + corrections(i)

   return
end function expectedNumberNeutrinoByBinFar

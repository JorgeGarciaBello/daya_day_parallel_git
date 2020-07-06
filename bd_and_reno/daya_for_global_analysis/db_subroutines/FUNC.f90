real(8) function FUNC(P)
    use db_data, only:NDIM
    implicit none
    real(8) :: P(NDIM)

    real(8) :: chiSquarePull1
    real(8) :: chiSquarePull2
    real(8) :: chiSquareProposal

    FUNC=0.0D0
    !FUNC=chiSquarePull1(P)
    !FUNC=chiSquarePull2(P)
    FUNC=chiSquareProposal(P)
    !print*,'FUNC',FUNC
    return
end function FUNC
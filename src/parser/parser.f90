MODULE parser
  USE units
  IMPLICIT NONE

  CONTAINS
!---------------------------------------------------------------------
!		get_input
!			James H. Thorpe
!		-gets the input, returns calculation type 
!		-creates data files for individual calculations
!
!		calc options:
!		0) Langevin dynamics for ion(A) neutral(B)
!---------------------------------------------------------------------
  !Variables
  ! cal		: int, calculation type
  SUBROUTINE get_input(calc)
    IMPLICIT NONE
    !INOUT
    INTEGER, INTENT(INOUT) :: calc

    REAL(KIND=8) :: val
    CHARACTER(LEN=4) :: prop
    val = 10
    prop = 'mass'

    WRITE(*,*) "get_input called"
    WRITE(*,*) conv_SI_cgs(val,prop)

  END SUBROUTINE get_input  
!---------------------------------------------------------------------

END MODULE parser


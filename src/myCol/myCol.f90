!---------------------------------------------------------------------
!		myCol
!			James H. Thorpe
!			Dec 1, 2018
!		-runs the myCol program
!---------------------------------------------------------------------
  !calc
PROGRAM myCol
  USE parser
  IMPLICIT NONE
  INTEGER :: calc

  calc = 0

  CALL print_init()
  CALL get_input(calc)

  CONTAINS
!---------------------------------------------------------------------
!               print_init
!                       James H. Thorpe
!                       Dec 1, 2018
!               -prints initial stuff
!---------------------------------------------------------------------
  SUBROUTINE print_init()
    IMPLICIT NONE
    
    WRITE(*,*)
    WRITE(*,*) "======================================================"
    WRITE(*,*) "                Starting myCol v0.0"
    WRITE(*,*) 
    WRITE(*,*) "                  James H. Thorpe"
    WRITE(*,*) "                   Stanton Group"
    WRITE(*,*) "             QTP, University of Florida"
    WRITE(*,*) 
    
  END SUBROUTINE print_init
!---------------------------------------------------------------------

END PROGRAM myCol

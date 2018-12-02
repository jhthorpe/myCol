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

  CALL clean()
  CALL print_init()
  CALL get_input(calc)

  CONTAINS
!---------------------------------------------------------------------
!		clean
!			James H. Thorpe
!		-cleans old junk
!---------------------------------------------------------------------
  SUBROUTINE clean()
    CALL EXECUTE_COMMAND_LINE('rm Lgv_vals error 2> /dev/null')
  END SUBROUTINE clean
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

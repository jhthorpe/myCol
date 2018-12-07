!---------------------------------------------------------------------
!		myCol
!			James H. Thorpe
!			Dec 1, 2018
!		-runs the myCol program
!---------------------------------------------------------------------
PROGRAM myCol
  USE parser
  USE langevin
  USE lda 
  USE fda
  USE ado
  IMPLICIT NONE
  INTEGER :: calc

  calc = 0

  CALL clean()
  CALL print_init()
  CALL get_input(calc)
  IF (calc .EQ. 0) THEN 
    CALL calc_Lgv()
    CALL calc_lda()
    CALL calc_fda()
    CALL calc_ado()
  ELSE IF (calc .EQ. 1) THEN
    CALL calc_Lgv()
  ELSE IF (calc .EQ. 2) THEN
    CALL calc_lda()
  ELSE IF (calc .EQ. 3) THEN
    CALL calc_fda()
  ELSE IF (calc .EQ. 4) THEN
    CALL calc_ado()
  END IF
  CALL clean()

  CONTAINS
!---------------------------------------------------------------------
!		clean
!			James H. Thorpe
!		-cleans old junk
!---------------------------------------------------------------------
  SUBROUTINE clean()
    CALL EXECUTE_COMMAND_LINE('rm vals error 2> /dev/null')
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
    WRITE(*,*) "====================================================================="
    WRITE(*,*) "                         Starting myCol v0.1"
    WRITE(*,*) 
    WRITE(*,*) "                           James H. Thorpe"
    WRITE(*,*) "                            Stanton Group"
    WRITE(*,*) "                      QTP, University of Florida"
    WRITE(*,*) 
    
  END SUBROUTINE print_init
!---------------------------------------------------------------------

END PROGRAM myCol

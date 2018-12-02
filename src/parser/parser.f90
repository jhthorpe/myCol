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
    !INTERNAL
    CHARACTER(LEN=20),DIMENSION(0:2) :: line
    CHARACTER(LEN=20) :: str
    INTEGER :: fline
    LOGICAL :: flag

    WRITE(*,*) "get_input called"
    CALL EXECUTE_COMMAND_LINE('cat input')
    CALL get_fline(flag,fline)
    CALL get_calc(fline,calc) 
    IF (calc .EQ. 0) THEN
      call get_Lgv(fline,flag)
    ELSE 
      WRITE(*,*) "Sorry, that calculation type not coded yet"
    END IF

    IF (flag) THEN
      WRITE(*,*) "There is a problem with your input file"
      WRITE(*,*) "------------------------------------------------------" 
      STOP
    END IF

  END SUBROUTINE get_input  
!---------------------------------------------------------------------
!		get_fline
!			James H. Thorpe
!			Dec 1, 2018
!               -check if file exists and get number of lines    
!---------------------------------------------------------------------
  SUBROUTINE get_fline(flag,fline)
    IMPLICIT NONE
    !Inout
    INTEGER, INTENT(INOUT) :: fline
    LOGICAL, INTENT(INOUT) :: flag
    !Internal
    INTEGER :: io
    LOGICAL :: ex
    flag = .FALSE.
    INQUIRE(file='input',EXIST=ex)
    IF (.NOT. ex) THEN
      WRITE(*,*) "You need to create the input file : 'input'"
      CALL EXECUTE_COMMAND_LINE('touch error')
      flag = .TRUE.
      fline = -1
      RETURN
    END IF
    fline = 0
    io = 0
    OPEN(unit=1,file='input',status='old',access='sequential')
    DO WHILE (io .EQ. 0)
      READ(1,*,iostat=io)
      IF (io .EQ. 0) fline = fline + 1
    END DO
    CLOSE(unit=1)
  END SUBROUTINE get_fline

!---------------------------------------------------------------------
!		get_calc
!			James H. Thorpe
!		-find what type of calculation we're dealing with
!---------------------------------------------------------------------
  ! fline		: int, number of lines in file
  SUBROUTINE get_calc(fline,calc)
    IMPLICIT NONE
    !INOUT
    INTEGER, INTENT(INOUT) :: calc
    INTEGER, INTENT(IN) :: fline
    !INTERNAL
    CHARACTER(LEN=20), DIMENSION(0:1) :: line
    INTEGER :: i
    LOGICAL :: found
    
    found = .FALSE.

    OPEN(unit=100,file='input',status='old')
    DO i=0,fline-1
      READ(100,*) line  
      IF (line(0) == 'calc') THEN
        IF (line(1) .EQ. 'Lgv') THEN
          calc = 0
          found = .TRUE.
        ELSE IF (line(1) .EQ. 'ADO') THEN
          calc = 0
          found = .TRUE.
        END IF
      END IF
    END DO
    CLOSE(unit=100)

    IF (.NOT. found) THEN
      WRITE(*,*) "No calculation type found"
      WRITE(*,*) "Possible options are..."
      WRITE(*,*) "Lgv"
      WRITE(*,*) "ADO"
    END IF

  END SUBROUTINE get_calc

!---------------------------------------------------------------------
!	get_Lgv
!		James H. Thorpe
!		Dec 1, 2018
!	-gets options needed for Langevin calculations
!---------------------------------------------------------------------
  ! fline		: int, number of lines in file 
  SUBROUTINE get_Lgv(fline,flag)
    IMPLICIT NONE
    !INOUT
    LOGICAL, INTENT(INOUT) :: flag
    INTEGER, INTENT(IN) :: fline
    !INTERNAL
    CHARACTER(LEN=20), DIMENSION(0:1) :: line 
    CHARACTER(LEN=10), DIMENSION(0:3) :: names
    REAL(KIND=8), DIMENSION(0:3) :: vals
    LOGICAL, DIMENSION(0:4) :: found
    INTEGER :: i,units

    names = ['massA+','massB ','aB    ','temp  ']
    found = .FALSE.

    OPEN(unit=100,file='input',status='old')
    DO i=0,fline-1
      READ(100,*) line 
      !Possible options
      IF (line(0) == 'massA+') THEN
        CALL get_mass(line(1),vals(0),found(0)) 
      ELSE IF (line(0) == 'massB') THEN
        CALL get_mass(line(1),vals(1),found(1))
      ELSE IF (line(0) == 'aB') THEN
        CALL get_pol(line(1),vals(2),found(2))
      ELSE IF (line(0) == 'temp') THEN
        CALL get_temp(line(1),vals(3),found(3))
      END IF
    END DO
    CLOSE(unit=100)

    DO i=0,3 
      IF (.NOT. found(i)) THEN 
        WRITE(*,*) "You are missing ", names(i)
        CALL EXECUTE_COMMAND_LINE('touch error')
        flag = .TRUE.
      END IF
    END DO

  END SUBROUTINE get_Lgv

!---------------------------------------------------------------------
!	get_mass
!		James H. Thorpe
!		Dec 1, 2018
!---------------------------------------------------------------------
  SUBROUTINE get_mass(chr,val,found)
    IMPLICIT NONE
    CHARACTER(LEN=20), INTENT(IN) :: chr
    REAL(KIND=8), INTENT(INOUT) :: val
    LOGICAL, INTENT(INOUT) :: found
    READ (chr,*) val
    IF (val .LE. 0.0D0) THEN
      WRITE(*,*) "mass cannot be negative"
      found = .FALSE.
    ELSE
      found = .TRUE.
    END IF
  END SUBROUTINE get_mass
!---------------------------------------------------------------------
!	get_pol
!		James H. Thorpe
!		Dec 1, 2018
!	-gets polarizability
!---------------------------------------------------------------------
  SUBROUTINE get_pol(chr,val,found)
    IMPLICIT NONE
    CHARACTER(LEN=20), INTENT(IN) :: chr
    REAL(KIND=8), INTENT(INOUT) :: val
    LOGICAL, INTENT(INOUT) :: found
    READ (chr,*) val
    IF (val .LE. 0.0D0) THEN
      WRITE(*,*) "polarizibility cannot be negative"
      found = .FALSE.
    ELSE
      found = .TRUE.
    END IF
  END SUBROUTINE get_pol
!---------------------------------------------------------------------
!	get_temp
!		James H. Thorpe
!		Dec 1, 2018
!	-gets temperatures 
!---------------------------------------------------------------------
  SUBROUTINE get_temp(chr,val,found)
    IMPLICIT NONE
    CHARACTER(LEN=20), INTENT(IN) :: chr
    REAL(KIND=8), INTENT(INOUT) :: val
    LOGICAL, INTENT(INOUT) :: found
    READ (chr,*) val
    IF (val .LE. 0.0D0) THEN
      WRITE(*,*) "temperatures cannot be negative"
      found = .FALSE.
    ELSE
      found = .TRUE.
    END IF
  END SUBROUTINE get_temp
!---------------------------------------------------------------------

END MODULE parser


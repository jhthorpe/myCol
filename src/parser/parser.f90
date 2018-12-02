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
    WRITE(*,*) "------------------------------------------------------" 

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
    CHARACTER(LEN=10), DIMENSION(0:5) :: names
    REAL(KIND=8), DIMENSION(0:4) :: vals
    LOGICAL, DIMENSION(0:5) :: found
    INTEGER :: i,units

    units = 0 !cgs by default
    names = ['massA+','massB ','temp  ','aB    ','charge','units ']
    found = .FALSE.

    !read in options
    OPEN(unit=100,file='input',status='old')
    DO i=0,fline-1
      READ(100,*) line 
      !Possible options
      IF (line(0) == 'massA+') THEN
        CALL get_mass(line(1),vals(0),found(0)) 
      ELSE IF (line(0) == 'massB') THEN
        CALL get_mass(line(1),vals(1),found(1))
      ELSE IF (line(0) == 'temp') THEN
        CALL get_temp(line(1),vals(2),found(2))
      ELSE IF (line(0) == 'aB') THEN
        CALL get_pol(line(1),vals(3),found(3))
      ELSE IF (line(0) == 'charge') THEN
        CALL get_charge(line(1),vals(4),found(4))
      ELSE IF (line(0) == 'units') THEN
        CALL get_units(line(1),units,found(5))
      END IF
    END DO
    CLOSE(unit=100)

    !check we have what we need
    DO i=0,4
      IF (.NOT. found(i)) THEN 
        WRITE(*,*) "You are missing ", names(i)
        CALL EXECUTE_COMMAND_LINE('touch error')
        flag = .TRUE.
      END IF
    END DO

    IF (flag) RETURN
    IF (.NOT. found(5)) WRITE(*,*) "Assuming cgs units"

    !transform units
    CALL unit_trans_Lgv(vals,units)

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
!	get_charge
!		James H. Thorpe
!		Dec 1, 2018
!	-gets temperatures 
!---------------------------------------------------------------------
  SUBROUTINE get_charge(chr,val,found)
    IMPLICIT NONE
    CHARACTER(LEN=20), INTENT(IN) :: chr
    REAL(KIND=8), INTENT(INOUT) :: val
    LOGICAL, INTENT(INOUT) :: found
    READ (chr,*) val
    found = .TRUE.
  END SUBROUTINE get_charge
!---------------------------------------------------------------------
!	get_units
!		James H. Thorpe
!		Dec 1, 2018
!	-gets units 
!---------------------------------------------------------------------
  SUBROUTINE get_units(chr,val,found)
    IMPLICIT NONE
    CHARACTER(LEN=20), INTENT(IN) :: chr
    INTEGER, INTENT(INOUT) :: val 
    LOGICAL, INTENT(INOUT) :: found
    IF (chr .EQ. 'cgs') THEN
      val = 0
      found = .TRUE.
    ELSE IF (chr .EQ. 'au') THEN
      val = 1
      found = .TRUE. 
    ELSE IF (chr .EQ. 'SI') THEN
      val = 2
      found = .TRUE.  
   END IF
  END SUBROUTINE get_units
!---------------------------------------------------------------------
!	unit_trans_Lgv
!		James H. Thorpe
!		Dec 1., 2018
!	-transform values into cgs units for Langevin calculation
!---------------------------------------------------------------------
  SUBROUTINE unit_trans_Lgv(old_val,units)
    IMPLICIT NONE
    REAL(KIND=8), PARAMETER :: A2B=1.8897161646320724D0,&
                               ec=4.80320427D-10
    !INOUT
    REAL(KIND=8), DIMENSION(0:), INTENT(IN) :: old_val 
    INTEGER, INTENT(IN) :: units
    !INTERNAL
    REAL(KIND=8), DIMENSION(0:4) :: new_val
    INTEGER :: i

    !all masses must be transformed
    new_val(0) = old_val(0)*1.660539E-24 !gfm -> g/molecule
    new_val(1) = old_val(1)*1.660539E-24 !gfm -> g/molecule
    new_val(2) = old_val(2) !temp is fine
    new_val(4) = old_val(4)*ec !q -> Fr 
    
    !polarizability is more complicated
    IF (units .EQ. 0) THEN !cgs, do nothing
      new_val(3) = old_val(3) 
    ELSE IF (units .EQ. 1) THEN !au. transform
      new_val(3) = old_val(3)/(A2B**3.0D0)   
      new_val(3) = new_val(3)*((1.0D-8)**3.0)
    END IF

    WRITE(*,*) "-------------------------------------"
    WRITE(*,*) "Values in cgs" 
    WRITE(*,*) "massA+",new_val(0)
    WRITE(*,*) "massB",new_val(1)
    WRITE(*,*) "temp",new_val(2)
    WRITE(*,*) "aB",new_val(3)
    WRITE(*,*) "charge",new_val(4)

    OPEN(file='Lgv_vals',unit=101,status='replace')
      WRITE(101,*) new_val
    CLOSE(unit=101,status='keep')

  END SUBROUTINE unit_trans_Lgv
!---------------------------------------------------------------------

END MODULE parser


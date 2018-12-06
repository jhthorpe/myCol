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
!		1) Ion-Dipole approximations 
!---------------------------------------------------------------------
  !Variables
  ! cal		: int, calculation type
  SUBROUTINE get_input(calc)
    IMPLICIT NONE
    !INOUT
    INTEGER, INTENT(INOUT) :: calc
    !INTERNAL
    CHARACTER(LEN=20),DIMENSION(0:2) :: line
    INTEGER :: fline
    LOGICAL :: flag

    WRITE(*,*) "get_input called"
    CALL EXECUTE_COMMAND_LINE('cat input')
    CALL get_fline(flag,fline)
    CALL get_calc(fline,calc) 
    CALL get_opts(fline,calc,flag)

    IF (flag) THEN
      WRITE(*,*) "There is a problem with your input file"
      WRITE(*,*) "------------------------------------------------------" 
      STOP
    END IF
    WRITE(*,*) "====================================================================="

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
    calc = 0

    OPEN(unit=100,file='input',status='old')
    DO i=0,fline-1
      READ(100,*) line  
      IF (line(0) == 'calc') THEN
        IF (line(1) .EQ. 'All' .OR. line(1) .EQ. 'all') THEN
          calc = 0
          found = .TRUE.
        ELSE IF (line(1) .EQ. 'Lgv') THEN
          calc = 1
          found = .TRUE.
        ELSE IF (line(1) .EQ. 'LDA') THEN
          calc = 2
          found = .TRUE.
        END IF
      END IF
    END DO
    CLOSE(unit=100)

    IF (.NOT. found) THEN
      WRITE(*,*) "No calculation type found"
      WRITE(*,*) "Possible options are..."
      WRITE(*,*) "All"
      WRITE(*,*) "Lgv"
      WRITE(*,*) "LDA"
    END IF

  END SUBROUTINE get_calc

!---------------------------------------------------------------------
!	get_opts
!		James H. Thorpe
!		Dec 1, 2018
!	-gets options needed
!---------------------------------------------------------------------
  ! fline		: int, number of lines in file 
  ! calc		: int, calculation ID
  SUBROUTINE get_opts(fline,calc,flag)
    IMPLICIT NONE
    !INOUT
    LOGICAL, INTENT(INOUT) :: flag
    INTEGER, INTENT(IN) :: fline,calc
    !INTERNAL
    CHARACTER(LEN=20), DIMENSION(0:1) :: line 
    CHARACTER(LEN=10), DIMENSION(0:6) :: names
    REAL(KIND=8), DIMENSION(0:5) :: vals
    LOGICAL, DIMENSION(0:6) :: found
    INTEGER :: i,units

    units = 0 !cgs by default
    names = ['massA+','massB ','temp  ','aB    ','charge','dipole','units ']
    vals = 0.0D0
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
      ELSE IF (line(0) == 'dipole') THEN
        CALL get_dipole(line(1),vals(5),found(5))
      ELSE IF (line(0) == 'units') THEN
        CALL get_units(line(1),units,found(6))
      END IF
    END DO
    CLOSE(unit=100)

    !check we have what we need
    !For all
    DO i=0,4
      IF (.NOT. found(i)) THEN 
        WRITE(*,*) "You are missing ", names(i)
        CALL EXECUTE_COMMAND_LINE('touch error')
        flag = .TRUE.
      END IF
    END DO

    !for dipole treatments
    IF (.NOT. found(5) .AND. calc .NE. 1) THEN
      WRITE(*,*) "You are missing ", names(5)
      CALL EXECUTE_COMMAND_LINE('touch error')
      flag = .TRUE.
    END IF

    IF (flag) RETURN
    IF (.NOT. found(6)) WRITE(*,*) "Assuming cgs units"

    !transform units
    CALL unit_trans(vals,units)

  END SUBROUTINE get_opts

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
!	get_dipole
!		James H. Thorpe
!		Dec 1, 2018
!	-gets temperatures 
!---------------------------------------------------------------------
  SUBROUTINE get_dipole(chr,val,found)
    IMPLICIT NONE
    CHARACTER(LEN=20), INTENT(IN) :: chr
    REAL(KIND=8), INTENT(INOUT) :: val
    LOGICAL, INTENT(INOUT) :: found
    READ (chr,*) val
    IF (val .LT. 0.0D0) THEN
      WRITE(*,*) "Dipole moment cannot be negative"
      found = .FALSE.
    ELSE
      found = .TRUE.
    END IF
  END SUBROUTINE get_dipole
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
    ELSE IF (chr .EQ. 'lit') THEN
      val = 2
      found = .TRUE.  
    ELSE IF (chr .EQ. 'direct') THEN
      val = -1
      found = .TRUE.
   END IF
  END SUBROUTINE get_units
!---------------------------------------------------------------------
!	unit_trans
!		James H. Thorpe
!		Dec 1., 2018
!	-transform values into cgs units
!---------------------------------------------------------------------
  SUBROUTINE unit_trans(old_val,units)
    IMPLICIT NONE
    REAL(KIND=8), PARAMETER :: A2B=1.8897161646320724D0,&
                               ec=4.80320427D-10
    !INOUT
    REAL(KIND=8), DIMENSION(0:), INTENT(IN) :: old_val 
    INTEGER, INTENT(IN) :: units
    !INTERNAL
    REAL(KIND=8), DIMENSION(0:5) :: new_val

    !all masses must be transformed to grams
    new_val(0) = old_val(0)*1.660539E-24 !gfm -> g/molecule
    new_val(1) = old_val(1)*1.660539E-24 !gfm -> g/molecule
    new_val(2) = old_val(2) !temp is fine

    !the non-trivial set
    IF (units .EQ. -1) THEN !direct, do nothing
      new_val = old_val
    ELSE IF (units .EQ. 0) THEN !cgs, decide later
      WRITE(*,*) "Sorry, cgs unit transform is not finishe yet"
      STOP
    ELSE IF (units .EQ. 1) THEN !au
      new_val(3) = conv_au_cgs(old_val(3),'pola') 
      new_val(4) = conv_au_cgs(old_val(4),'char')
      new_val(5) = conv_au_cgs(old_val(5),'dipo') 
    ELSE IF (units .EQ. 2) THEN !au
      new_val(3) = conv_lit_cgs(old_val(3),'pola') 
      new_val(4) = conv_lit_cgs(old_val(4),'char')
      new_val(5) = conv_lit_cgs(old_val(5),'dipo') 
    END IF


    WRITE(*,*) "-------------------------------------"
    IF (units .EQ. -1) THEN
      WRITE(*,*) "You have chosen direct units"
      WRITE(*,*) "Quantity            units"
      WRITE(*,*) "Mass            :    none"
      WRITE(*,*) "Temperature     :    none"
      WRITE(*,*) "Polarizability  :    none"
      WRITE(*,*) "Charge          :    none"
      WRITE(*,*) "Dipole Moment   :    none"
    ELSE IF (units .EQ. 0) THEN
      WRITE(*,*) "You have chosen cgs units - DON'T DO THIS"
      WRITE(*,*) "Quantity            units"
      WRITE(*,*) "Mass            :     gfm"
      WRITE(*,*) "Temperature     :       K"
      WRITE(*,*) "Polarizability  :    cm^3"
      WRITE(*,*) "Charge          :   q*???"
      WRITE(*,*) "Dipole Moment   :    D???"
    ELSE IF (units .EQ. 1) THEN
      WRITE(*,*) "You have chosen atomic units"
      WRITE(*,*) "Quantity            units"
      WRITE(*,*) "Mass            :     gfm"
      WRITE(*,*) "Temperature     :       K"
      WRITE(*,*) "Polarizability  :  bohr^3"
      WRITE(*,*) "Charge          :       Z"
      WRITE(*,*) "Dipole Moment   :    a.u."
    ELSE IF (units .EQ. 2) THEN
      WRITE(*,*) "You have chosen literature units"
      WRITE(*,*) "Quantity            units"
      WRITE(*,*) "Mass            :     gfm"
      WRITE(*,*) "Temperature     :       K"
      WRITE(*,*) "Polarizability  :     Å^3"
      WRITE(*,*) "Charge          :       Z"
      WRITE(*,*) "Dipole Moment   :       D"
    END IF
    WRITE(*,*) "-------------------------------------"
    WRITE(*,*) "Values used in calculation" 
    WRITE(*,*) "massA+  ",new_val(0)
    WRITE(*,*) "massB   ",new_val(1)
    WRITE(*,*) "temp    ",new_val(2)
    WRITE(*,*) "aB      ",new_val(3)
    WRITE(*,*) "charge  ",new_val(4)
    WRITE(*,*) "dipole  ",new_val(5)

    OPEN(file='vals',unit=101,status='replace')
      WRITE(101,*) new_val
    CLOSE(unit=101,status='keep')

  END SUBROUTINE unit_trans
!---------------------------------------------------------------------

END MODULE parser


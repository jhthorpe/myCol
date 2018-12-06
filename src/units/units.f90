MODULE units
  IMPLICIT NONE

  CONTAINS
!---------------------------------------------------------------------
!	conv_SI_cgs
!		James H. Thorpe
!		Dec 1, 2018
!	-converts values in SI to cgs units
!---------------------------------------------------------------------
  !Variables
  ! SI		: real*8, property in SI units
  ! prop	: char*4, property name, first 4 letters

  REAL(KIND=8) FUNCTION conv_SI_cgs(SI,prop)
    IMPLICIT NONE
    !INOUT
    CHARACTER(LEN=4), INTENT(IN) :: prop
    REAL(KIND=8), INTENT(IN) :: SI 
    !INTERNAL
    REAL(KIND=8) :: val

    val = SI

    IF (prop .EQ. 'mass') THEN !kg -> g
      val = val*1.0E-3
    ELSE IF (prop .EQ. 'leng') THEN !m -> cm
      val = val*1.0D-2
    ELSE IF (prop .EQ. 'time') THEN !s -> s
      val = val 
    ELSE IF (prop .EQ. 'ener') THEN !J -> erg
      val = val*1.0D-7
    ELSE IF (prop .EQ. 'velo') THEN !m/s -> cm/s
      val = val*1.0D-2
    ELSE IF (prop .EQ. 'temp') THEN !K -> K
      val = val
    ELSE 
      WRITE(*,*) "conv_SI_cgs : the following property was not recognized"
      WRITE(*,*) prop
      val = 0.0D0
    END IF    

    conv_SI_cgs = val

  END FUNCTION conv_SI_cgs

!---------------------------------------------------------------------
!	conv_au_cgs
!		James H. Thorpe
!		Dec 5, 2018
!	-converts values in au to cgs units
!---------------------------------------------------------------------
  ! au		: real*8, property in a.u. 
  ! prop	: char*4, first 4 letters of property name
  REAL(KIND=8) FUNCTION conv_au_cgs(au,prop)
    IMPLICIT NONE
    REAL(KIND=8), PARAMETER :: A2B=1.8897161646320724D0,&
                           ec=4.80320427D-10
    CHARACTER(LEN=4), INTENT(IN) :: prop
    REAL(KIND=8), INTENT(IN) :: au
    REAL(KIND=8) :: val

    val = au

    IF (prop .EQ. 'dipo') THEN !a.u. -> esu*cm
      val = val/0.393430307 !au -> debye
      val = val*1D-18 !debye -> esu*cm
    ELSE IF (prop .EQ. 'pola') THEN !a.u. -> cm^3
      val = val/(A2B**3.0D0) !borh -> Å
      val = val*((1.0D-8)**3.0) !Å -> cm
    ELSE IF (prop .EQ. 'char') THEN !Z -> q_esu
      val = val*ec
    ELSE
      WRITE(*,*) "conv_au_cgs : the following property was not recognized"
      WRITE(*,*) prop
      val = 0.0D0
    END IF

    conv_au_cgs = val

  END FUNCTION conv_au_cgs
!---------------------------------------------------------------------
!	conv_lit_cgs
!		James H. Thorpe
!	-converts literature values to cgs units
!	-polar: Å^3
!	-dipol: D
!---------------------------------------------------------------------
  ! lit		: real*8, property in a.u. 
  ! prop	: char*4, first 4 letters of property name
  REAL(KIND=8) FUNCTION conv_lit_cgs(lit,prop)
    IMPLICIT NONE
    REAL(KIND=8), PARAMETER :: A2B=1.8897161646320724D0,&
                           ec=4.80320427D-10
    CHARACTER(LEN=4), INTENT(IN) :: prop
    REAL(KIND=8), INTENT(IN) :: lit
    REAL(KIND=8) :: val

    val = lit

    IF (prop .EQ. 'dipo') THEN !D -> 10^-18 esu*cm
      val = val*2.541580252938067D-18
    ELSE IF (prop .EQ. 'pola') THEN !Å^3 -> cm^3
      val = val*((1.0D-8)**3.0)
    ELSE IF (prop .EQ. 'char') THEN !charge -> q_esu (g^.5 cm^1.5 s^-1)
      val = val*ec
    ELSE
      WRITE(*,*) "conv_lit_cgs : the following property was not recognized"
      WRITE(*,*) prop
      val = 0.0D0
    END IF

    conv_lit_cgs = val
  END FUNCTION conv_lit_cgs
!---------------------------------------------------------------------

END MODULE units

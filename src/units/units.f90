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

END MODULE units

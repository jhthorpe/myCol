MODULE langevin
  IMPLICIT NONE

  CONTAINS
!---------------------------------------------------------------------
!	calc_lgv
!		James H. Thorpe
!		Dec 1, 2018
!	- calculate Langevin coefs and constants
!---------------------------------------------------------------------
  !ma,b		: real*8, mass of A,B
  !uab		: real*8, reduced mass
  !qa		: real*8, charge on A
  !T		: real*8, temperature
  !alb		: real*8, polarization of B
  SUBROUTINE calc_lgv()
    IMPLICIT NONE
    REAL(KIND=8), PARAMETER :: pi=3.1415926535897932,&
                               kb=1.38064852D-16
    REAL(KIND=8), DIMENSION(0:4) :: vals
    REAL(KIND=8) :: ma,mb,uab,qa,T,alb

    !vals = [massA+, massB, temp, aB, q]
    WRITE(*,*)
    WRITE(*,*) "		Starting Langevin Calculations"

    OPEN(unit=100,file='Lgv_vals',status='old')
    READ(100,*) vals
    CLOSE(unit=100)
    ma = vals(0)
    mb = vals(1)
    T = vals(2)
    alB = vals(3)
    qa = vals(4)

    uab = ma*mb/(ma+mb)
 
    WRITE(*,*) 
    WRITE(*,*) "k_col (cm^3/s) = ", SQRT(4*(pi*qa)**2.0D0*alb/uab)
     
  END SUBROUTINE calc_lgv
!---------------------------------------------------------------------

END MODULE langevin

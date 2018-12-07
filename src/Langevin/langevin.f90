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
    REAL(KIND=8), DIMENSION(0:5) :: vals
    REAL(KIND=8) :: ma,mb,uab,qa,T,alb,v_rms,v_avg,v_mp
    REAL(KIND=8) :: s_rms,s_avg,s_mp

    !vals = [massA+, massB, temp, aB, q, u]
    WRITE(*,*)
    WRITE(*,*) "		Starting Langevin Calculations"
    WRITE(*,*)

    OPEN(unit=100,file='vals',status='old')
    READ(100,*) vals
    CLOSE(unit=100)
    ma = vals(0)
    mb = vals(1)
    T = vals(2)
    alB = vals(3)
    qa = vals(4)

    uab = ma*mb/(ma+mb)
    v_rms = SQRT(3*kb*T/uab)
    v_avg = SQRT(8*kb*T/(pi*uab))
    v_mp  = SQRT(2*kb*T/uab)
    s_rms = eval_sigma(v_rms,qa,uab,alb)
    s_avg = eval_sigma(v_avg,qa,uab,alb)
    s_mp  = eval_sigma(v_mp,qa,uab,alb)

    WRITE(*,*) "v RMS" 
    WRITE(*,*) "v_rms     (Å/ps)    = ", v_rms*1.0D-4
    WRITE(*,*) "σ_rms (Å^2)     = ", s_rms*1.0D16
    WRITE(*,*) "bc_rms    (Å)       = ", SQRT(s_rms*1.0D16/pi)
    WRITE(*,*) "rc_rms    (Å)       = ", SQRT(s_rms*1.0D16/pi)/SQRT(2.0)
    WRITE(*,*) "---------------------"
    WRITE(*,*) 
    WRITE(*,*) "<v>" 
    WRITE(*,*) "v_avg     (Å/ps)    = ", v_avg*1.0D-4
    WRITE(*,*) "σ_avg (Å^2)     = ", s_avg*1.0D16
    WRITE(*,*) "bc_avg    (Å)       = ", SQRT(s_avg*1.0D16/pi)
    WRITE(*,*) "rc_avg    (Å)       = ", SQRT(s_avg*1.0D16/pi)/SQRT(2.0)
    WRITE(*,*) "---------------------"
    WRITE(*,*) 
    WRITE(*,*) "v*" 
    WRITE(*,*) "v_mp     (Å/ps)     = ", v_mp*1.0D-4
    WRITE(*,*) "σ_mp (Å^2)      = ", s_mp*1.0D16
    WRITE(*,*) "bc_mp    (Å)        = ", SQRT(s_mp*1.0D16/pi)
    WRITE(*,*) "rc_mp    (Å)        = ", SQRT(s_mp*1.0D16/pi)/SQRT(2.0)
    WRITE(*,*) "---------------------"
    WRITE(*,*) 
    WRITE(*,*) "Rate Coefficients x10^9"
    WRITE(*,*) "k_col    (cm^3/s)   = ", SQRT(4*(pi*qa)**2.0D0*alb/uab)*1.0D9
    WRITE(*,*) "====================================================================="
     
  END SUBROUTINE calc_lgv
!---------------------------------------------------------------------
!	eval_sigma
!		James H. Thorpe
!	-evaluates sigma for a given velocity
!---------------------------------------------------------------------
  !v		: real*8, velocity
  !q		: real*8, charge
  !uab		: real*8, reduced mass
  !ab		: real*8, polarizibility
  REAL(KIND=8) FUNCTION eval_sigma(v,q,uab,ab)
    IMPLICIT NONE 
    REAL(KIND=8), PARAMETER :: pi=3.1415926535897932,&
                               kb=1.38064852D-16
    REAL(KIND=8), INTENT(IN) :: v,q,uab,ab
    REAL(KIND=8) :: s
    s = pi*q*SQRT(2.0D0*ab/(0.5D0*uab*v**2.0D0))   
    eval_sigma = s
  END FUNCTION eval_sigma 
!---------------------------------------------------------------------

END MODULE langevin

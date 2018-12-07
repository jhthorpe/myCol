MODULE ado
  IMPLICIT NONE

  CONTAINS
!---------------------------------------------------------------------
!	calc_ado
!		James H. Thorpe
!		Dec 1, 2018
!	- calculate Locked Dipole Approximation quantities
!---------------------------------------------------------------------
  !ma,b		: real*8, mass of A,B
  !uab		: real*8, reduced mass
  !qa		: real*8, charge on A
  !T		: real*8, temperature
  !alb		: real*8, polarization of B
  !dpl		: real*8, dipole moment
  SUBROUTINE calc_ado()
    IMPLICIT NONE
    REAL(KIND=8), PARAMETER :: pi=3.1415926535897932,&
                               kb=1.38064852D-16
    REAL(KIND=8), DIMENSION(0:5) :: vals
    REAL(KIND=8) :: ma,mb,uab,qa,T,alb,v_rms,v_avg,v_mp
    REAL(KIND=8) :: s_rms,s_avg,s_mp,dpl

    !vals = [massA+, massB, temp, aB, q, u]
    WRITE(*,*)
    WRITE(*,*) "         Starting Average Dipole Orientation Calculations"
    WRITE(*,*) "                        using <cosθ>"
    WRITE(*,*)

    OPEN(unit=100,file='vals',status='old')
    READ(100,*) vals
    CLOSE(unit=100)
    ma = vals(0)
    mb = vals(1)
    T = vals(2)
    alB = vals(3)
    qa = vals(4)
    dpl = vals(5)

    uab = ma*mb/(ma+mb)
    v_rms = SQRT(3*kb*T/uab)
    v_avg = SQRT(8*kb*T/(pi*uab))
    v_mp  = SQRT(2*kb*T/uab)
    s_rms = eval_sigma_ado(v_rms,qa,uab,alb,dpl)
    s_avg = eval_sigma_ado(v_avg,qa,uab,alb,dpl)
    s_mp  = eval_sigma_ado(v_mp,qa,uab,alb,dpl)

    WRITE(*,*) "v RMS" 
    WRITE(*,*) "v_rms     (Å/ps)    = ", v_rms*1.0D-4
    WRITE(*,*) "<σ>_rms   (Å^2)     = ", s_rms*1.0D16
    WRITE(*,*) "bc_rms    (Å)       = ", SQRT(s_rms*1.0D16/pi)
    WRITE(*,*) "---------------------"
    WRITE(*,*) 
    WRITE(*,*) "<v>" 
    WRITE(*,*) "v_avg     (Å/ps)    = ", v_avg*1.0D-4
    WRITE(*,*) "<σ>_avg   (Å^2)     = ", s_avg*1.0D16
    WRITE(*,*) "bc_avg    (Å)       = ", SQRT(s_avg*1.0D16/pi)
    WRITE(*,*) "---------------------"
    WRITE(*,*) 
    WRITE(*,*) "v*" 
    WRITE(*,*) "v_mp      (Å/ps)    = ", v_mp*1.0D-4
    WRITE(*,*) "<σ>_mp    (Å^2)     = ", s_mp*1.0D16
    WRITE(*,*) "bc_mp     (Å)       = ", SQRT(s_mp*1.0D16/pi)
    WRITE(*,*) "---------------------"
    WRITE(*,*) 
    WRITE(*,*) "Rate Coefficients x10^9"
    WRITE(*,*) "k(v_rms)  (cm^3/s)  = ", v_rms*s_rms*1.0D9 
    WRITE(*,*) "k(v_avg)  (cm^3/s)  = ", v_avg*s_avg*1.0D9 
    WRITE(*,*) "k(v_mp)   (cm^3/s)  = ", v_mp*s_mp*1.0D9 
    WRITE(*,*) "<k> @T    (cm^3/s)  = not coded yet"
    WRITE(*,*) "====================================================================="
     
  END SUBROUTINE calc_ado
!---------------------------------------------------------------------
!	eval_sigma_ado
!		James H. Thorpe
!	-evaluates sigma for a given velocity in LDA 
!---------------------------------------------------------------------
  !v		: real*8, velocity
  !q		: real*8, charge
  !uab		: real*8, reduced mass
  !ab		: real*8, polarizibility
  !dpl		: real*8, dipole moment
  REAL(KIND=8) FUNCTION eval_sigma_ado(v,q,uab,ab,dpl)
    IMPLICIT NONE 
    REAL(KIND=8), PARAMETER :: pi=3.1415926535897932,&
                               kb=1.38064852D-16
    REAL(KIND=8), INTENT(IN) :: v,q,uab,ab,dpl
    REAL(KIND=8) :: s,Er,ec
    Er = 0.5D0*uab*v**2.0D0
    ec = dpl**2.0D0/(2*ab)
    s = pi*dpl*q/(4*Er)*(1 - Er/ec)+SQRT(ab*q**2.0D0/(2*Er)) + ab*q/dpl
    
    eval_sigma_ado = s
  END FUNCTION eval_sigma_ado 
!---------------------------------------------------------------------
!	eval_rate_ado
!		James H. Thorpe
!	-evaluates FDA <rate coefficeint>
!---------------------------------------------------------------------
  !q		: real*8, charge
  !uab		: real*8, reduced mass
  !ab		: real*8, polarizibility
  !dpl		: real*8, dipole moment
  !T		: real*8, temperature
  REAL(KIND=8) FUNCTION eval_rate_ado(q,uab,ab,dpl,T)
    IMPLICIT NONE 
    REAL(KIND=8), PARAMETER :: pi=3.1415926535897932,&
                               kb=1.38064852D-16
    REAL(KIND=8), INTENT(IN) :: q,uab,ab,dpl,T
    REAL(KIND=8) :: k,Er,ec
    !Er = 0.5D0*uab*v**2.0D0
    !ec = dpl**2.0D0/(2*ab)
    STOP "Thermal average rate depends on velocity??" 

    eval_rate_ado = k
  END FUNCTION eval_rate_ado
!---------------------------------------------------------------------
!	eval_rcrit_ado
!		James H. Thorpe
!		Dec 6, 2018
!	-evaluates the rc distance for LDA
!---------------------------------------------------------------------
  !v		: real*8, velocity
  !q		: real*8, charge
  !uab		: real*8, reduced mass
  !ab		: real*8, polarizability
  !dpl		: real*8, dipole moment
  REAL(KIND=8) FUNCTION eval_rcrit_ado(v,q,uab,ab,dpl)
    IMPLICIT NONE
    REAL(KIND=8), PARAMETER :: pi=3.1415926535897932,&
                               kb=1.38064852D-16
    REAL(KIND=8), INTENT(IN) :: v,q,uab,ab,dpl 
    REAL(KIND=8) :: rc
    !rc = 2.0D0*ab*q**2.0D0/(uab*v**2.0D0*)
    WRITE(*,*) "Not finished yet"
    STOP

  END FUNCTION eval_rcrit_ado
!---------------------------------------------------------------------

END MODULE ado

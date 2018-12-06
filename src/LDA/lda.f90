MODULE lda
  IMPLICIT NONE

  CONTAINS
!---------------------------------------------------------------------
!	calc_lda
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
  SUBROUTINE calc_lda()
    IMPLICIT NONE
    REAL(KIND=8), PARAMETER :: pi=3.1415926535897932,&
                               kb=1.38064852D-16
    REAL(KIND=8), DIMENSION(0:5) :: vals
    REAL(KIND=8) :: ma,mb,uab,qa,T,alb,v_rms,v_avg,v_mp
    REAL(KIND=8) :: s_rms,s_avg,s_mp,dpl

    !vals = [massA+, massB, temp, aB, q, u]
    WRITE(*,*)
    WRITE(*,*) "         Starting Locked Dipole Approximation Calculations"
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
    s_rms = eval_sigma_lda(v_rms,qa,uab,alb,dpl)
    s_avg = eval_sigma_lda(v_avg,qa,uab,alb,dpl)
    s_mp  = eval_sigma_lda(v_mp,qa,uab,alb,dpl)

    WRITE(*,*) "v RMS" 
    WRITE(*,*) "v_rms     (Å/ps)    = ", v_rms*1.0D-4
    WRITE(*,*) "sigma_rms (Å^2)     = ", s_rms*1.0D16
    WRITE(*,*) "bc_rms    (Å)       = ", SQRT(s_rms*1.0D16/pi)
    WRITE(*,*) "---------------------"
    WRITE(*,*) 
    WRITE(*,*) "<v>" 
    WRITE(*,*) "v_avg     (Å/ps)    = ", v_avg*1.0D-4
    WRITE(*,*) "sigma_avg (Å^2)     = ", s_avg*1.0D16
    WRITE(*,*) "bc_avg    (Å)       = ", SQRT(s_avg*1.0D16/pi)
    WRITE(*,*) "---------------------"
    WRITE(*,*) 
    WRITE(*,*) "v*" 
    WRITE(*,*) "v_mp      (Å/ps)    = ", v_mp*1.0D-4
    WRITE(*,*) "sigma_mp  (Å^2)     = ", s_mp*1.0D16
    WRITE(*,*) "bc_mp     (Å)       = ", SQRT(s_mp*1.0D16/pi)
    WRITE(*,*) "---------------------"
    WRITE(*,*) 
    WRITE(*,*) "Rate Coefficients x10^9"
    WRITE(*,*) "k(v_rms)  (cm^3/s)  = ", eval_rate_lda(v_rms,qa,uab,alB,dpl)*1.0D9 
    WRITE(*,*) "k(v_avg)  (cm^3/s)  = ", eval_rate_lda(v_avg,qa,uab,alB,dpl)*1.0D9 
    WRITE(*,*) "k(v_mp)   (cm^3/s)  = ", eval_rate_lda(v_mp,qa,uab,alB,dpl)*1.0D9 
    WRITE(*,*) "<k> @T    (cm^3/s)  = ", 2*pi*qa/SQRT(uab)*(SQRT(alB)+dpl*&
                                            SQRT(2.0D0/(pi*kb*T)))*1.0D9
    WRITE(*,*) "====================================================================="
    WRITE(*,*) "in cgs..."
    WRITE(*,*) "testing, v =, k=, ", v_avg/10.0D0, &
    eval_rate_lda(v_avg/10.0D0,qa,uab,alB,dpl)*1.0D9
    WRITE(*,*) "testing, v =, k=, ", v_avg/100.0D0, &
    eval_rate_lda(v_avg/100.0D0,qa,uab,alB,dpl)*1.0D9
    WRITE(*,*) "testing, v =, k=, ", v_avg/1000.0D0, &
    eval_rate_lda(v_avg/1000.0D0,qa,uab,alB,dpl)*1.0D9
    WRITE(*,*) "testing, v =, k=, ", v_avg/10000.0D0, &
    eval_rate_lda(v_avg/10000.0D0,qa,uab,alB,dpl)*1.0D9
    WRITE(*,*) "testing, v =, k=, ", v_avg/100000.0D0, &
    eval_rate_lda(v_avg/100000.0D0,qa,uab,alB,dpl)*1.0D9
    WRITE(*,*) "testing, v =, k=, ", v_avg/1000000.0D0, &
    eval_rate_lda(v_avg/1000000.0D0,qa,uab,alB,dpl)*1.0D9
     
  END SUBROUTINE calc_lda
!---------------------------------------------------------------------
!	eval_sigma_lda
!		James H. Thorpe
!	-evaluates sigma for a given velocity in LDA 
!---------------------------------------------------------------------
  !v		: real*8, velocity
  !q		: real*8, charge
  !uab		: real*8, reduced mass
  !ab		: real*8, polarizibility
  !dpl		: real*8, dipole moment
  REAL(KIND=8) FUNCTION eval_sigma_lda(v,q,uab,ab,dpl)
    IMPLICIT NONE 
    REAL(KIND=8), PARAMETER :: pi=3.1415926535897932,&
                               kb=1.38064852D-16
    REAL(KIND=8), INTENT(IN) :: v,q,uab,ab,dpl
    REAL(KIND=8) :: s
    s = 2*pi*q/(SQRT(uab)*v)*(SQRT(ab) + dpl/v) 
    eval_sigma_lda = s
  END FUNCTION eval_sigma_lda 
!---------------------------------------------------------------------
!	eval_k_lda
!		James H. Thorpe
!	-evaluates LDA rate coefficeint for given velocity
!---------------------------------------------------------------------
  !v		: real*8, velocity
  !q		: real*8, charge
  !uab		: real*8, reduced mass
  !ab		: real*8, polarizibility
  !dpl		: real*8, dipole moment
  REAL(KIND=8) FUNCTION eval_rate_lda(v,q,uab,ab,dpl)
    IMPLICIT NONE 
    REAL(KIND=8), PARAMETER :: pi=3.1415926535897932,&
                               kb=1.38064852D-16
    REAL(KIND=8), INTENT(IN) :: v,q,uab,ab,dpl
    REAL(KIND=8) :: k
    k = 2*pi*q/SQRT(uab)*(SQRT(ab)+dpl/v) 
    eval_rate_lda = k
  END FUNCTION eval_rate_lda
!---------------------------------------------------------------------

END MODULE lda

MODULE LINEAR_SOLVERS
CONTAINS

SUBROUTINE TDMA ( n, a, b, c, d, x )
!---------------------------------------------------------------------------------!
!                                                                                 !
!  Thomas Algorithm for solving linear systems involving diagonally dominant      !
!  triadiagonal matrices. This is generally more efficient than general LU or     !
!  Cholesky decomposition for more fuller matrices.                               !
!                                                                                 !
!  As a warning, TDMA is not suited for PDE matrix operators for problems with    !
!  periodic boundary conditions, use another method, such as the variant of this  !
!  one, the Sherman-Morrison(-Woodbury) Formula which can be used in conjunction  !
!  with this one.                                                                 !
!                                                                                 !  
!  With this algorithm, a(1) = 0 and c(n) = 0 from the matrix setup given below   !
!                                                                                 !
!  ------------------                                                             !
!  |b1 c1 ....... 0 |                                                             !
!  |a2 b2 c2  .   . |                                                             !
!  | . a3 b3  .   . |                                                             !
!  | .  .  .  . cn-1|                                                             !
!  |0 ...... an  bn |                                                             !
!  ------------------                                                             !
!                                                                                 !
!---------------------------------------------------------------------------------!
	IMPLICIT NONE
	INTEGER(KIND=4)                :: i
	INTEGER(KIND=4), INTENT(IN)    :: n
	REAL(KIND=8),    INTENT(IN)    :: a(n),c(n)
	REAL(KIND=8),    INTENT(INOUT) :: b(n),d(n)
	REAL(KIND=8),    INTENT(OUT)   :: x(n)

	DO i = 2,n
		b(i) = b(i) - a(i)*c(i-1)/b(i-1)
		d(i) = d(i) - a(i)*d(i-1)/b(i-1)
	ENDDO

	x(n) = d(n)/b(n)
	DO i = n-1,1,-1
		x(i) = (d(i) - c(i)*x(i+1)) / b(i)
	ENDDO
	RETURN
END SUBROUTINE TDMA


SUBROUTINE SHERMAN_MORRISON ( n, a, b, c, d, x )
!---------------------------------------------------------------------------------!
!                                                                                 !
!  Sherman-Morrison formula for solving linear systems with Thomas algorithm. The !
!  solver calls the TDMA solver twice to solve the double linear system we have   !
!  for this particular system of periodic boundary conditions, where we have,     !
!                                                                                 !
!        A*x = d,                                                                 !
!        A*z = u,                                                                 !
!                                                                                 !
!  where A is our original matrix composed of a,b,c vectors without periodic BC   !
!  elements, d is the vector on the RHS of the original linear system, u is a     !
!  a vector holding the boundary conditions as u = (alph, 0, ..., 0, gamm/beta)   !
!                                                                                 !  
!  With this algorithm, alph=c(n) and beta=a(1) from the matrix setup given below !
!                                                                                 !
!  ------------------                                                             !
!  |b1 c1 .......a1 |  alph = cn                                                  !
!  |a2 b2 c2  .   . |  beta = a1                                                  !
!  | . a3 b3  .   . |                                                             !
!  | .  .  .  . cn-1|                                                             !
!  |cn ..... an  bn |                                                             !
!  ------------------                                                             !
!                                                                                 !
!---------------------------------------------------------------------------------!
	IMPLICIT NONE
	INTEGER(KIND=4)                :: i
	INTEGER(KIND=4), INTENT(IN)    :: n
	REAL(KIND=8),    INTENT(IN)    :: a(n),b(n),c(n)
	REAL(KIND=8),    INTENT(INOUT) :: d(n)
	REAL(KIND=8),    INTENT(OUT)   :: x(n)
	REAL(KIND=8)                   :: u(n),bb(n),z(n),gamm,fact,alph,beta

	alph = c(n)
	beta = a(1)
	gamm = -b(1)

	u    = 0.d0
	u(1) = gamm
	u(n) = alph
	
	bb    = b
	bb(1) = b(1) - gamm
	bb(n) = b(n) - alph*beta/gamm

	CALL TDMA ( n, a, bb, c, d, x )

	CALL TDMA ( n, a, bb, c, u, z )

	fact = (x(1) + beta*x(n)/gamm) / (1.d0+z(1) + beta*z(n)/gamm)
	DO i = 1,n
		x(i) = x(i) - fact*z(i)
	ENDDO
	RETURN
END SUBROUTINE SHERMAN_MORRISON



END MODULE LINEAR_SOLVERS

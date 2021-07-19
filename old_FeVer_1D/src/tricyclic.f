C
C REAL TRIDIAGONAL SOLUTION BY POWER-OF-THREE CYCLIC REDUCTION
C
C       CALL TRICYC(A,B,C,D,N)
C
C       SOLVES TX=D WHERE TRIDIAGONAL MATRIX HAS THE FORM
C
C       |  B0  C0                           |
C       |  A1  B1  C1                       |
C       |      A2  B2  C2                   |
C       |          A3  B3  C3               |
C       |                                   |
C       |                                   |
C       |                                   |
C       |                                   |
C       |                         AN-1 BN-1 |
C
C
C       ANSWER OVERWRITES "D".  ALL INPUTS WILL BE OVERWRITTEN
C       DURING CALCULATION. A0 AND CN-1 WILL BE MODIFIED.
C       A AND C MAY NOT BE EQUIVALENCED.
C
C AUTHOR: STEWART A. LEVIN   MOBIL RESEARCH AND DEVELOPMENT CORP.
C DATE:   JULY 28, 1989
C
C
       SUBROUTINE TRICYC(A,B,C,D,N)
       IMPLICIT NONE
       INTEGER N
       REAL A(0:N-1)  ! SUBDIAGONAL COEFFICIENTS
       REAL B(0:N-1)  ! DIAGONAL COEFFICIENTS
       REAL C(0:N-1)  ! SUPERDIAGONAL COEFFICIENTS
       REAL D(0:N-1)  ! RIGHT-HAND SIDE VECTOR
C
C LOCAL VARIABLES (STACK)
C
       INTEGER MAXSTK      ! MAXIMUM STACK DEPTH 
       PARAMETER (MAXSTK=20)
       INTEGER ISHORT      ! LENGTH AT WHICH TO SWITCH TO SCALAR SOLVER
       PARAMETER (ISHORT=25) !
       EXTERNAL SCTRIL
       INTEGER ISTK  ! STACK POINTER
       INTEGER NM1   ! CURRENT VALUE OF N-1
       INTEGER ND3   ! CURRENT VALUE OF (N-1)/3
       INTEGER NREM(MAXSTK) ! CURRENT AND PRIOR VALUES OF (N-1) MOD 3
       INTEGER I,J         ! STRIDE AND LOOP INCREMENT
       REAL TA110,TA210        ! LOOP TEMPORARIES
       REAL TA130,TA230        ! ""
       REAL TA150,TA250        ! ""
       REAL TA020,TC020        ! ""
       REAL TA040,TC040        ! ""
       REAL TA060,TC060        ! ""
       REAL TB110,TB210        ! ""
       REAL TB130,TB230        ! ""
       REAL TB150,TB250        ! ""
       REAL TA061              ! ""
       REAL TC110,TC210        ! ""
       REAL TC130,TC230        ! ""
       REAL TC150,TC250        ! ""
       REAL TD110,TD210        ! ""
       REAL TD130,TD230        ! ""
       REAL TD150,TD250        ! ""
       REAL TEMP10,TEMP30,TEMP50 ! ""
       REAL TEMP31      ! ""
C
C INITIALIZE REDUCTION
C
       ISTK=1
       NM1=N-1
       I=1          ! INITIALLY COMPACT VECTORS
       A(0)=0.0     ! CONVENIENCE
       C(NM1)=0.0   ! ""
C
C      DO WHILE(NM1.GT.ISHORT)
 101   CONTINUE
       IF(NM1.LE.ISHORT)GOTO 102
           ND3=NM1/3
           NREM(ISTK)=NM1-3*ND3
           IF(NREM(ISTK).EQ.2) THEN
CDIR$ IVDEP
C$DIR NO_RECURRENCE
               DO 10 J=0,ND3
                   TB110=B((3*J+1)*I)
                   TB210=B((3*J+2)*I)
                   TA210=A((3*J+2)*I)
                   TC110=C((3*J+1)*I)
                   TEMP10=1.0/(TB110*TB210-TC110*TA210)
                   TB110=TEMP10*TB110
                   TB210=TEMP10*TB210
                   TA210=TEMP10*TA210
                   TC110=TEMP10*TC110
                   TD110=D((3*J+1)*I)
                   TD210=D((3*J+2)*I)
                   D((3*J+1)*I)=TD110*TB210-TC110*TD210
                   D((3*J+2)*I)=TD210*TB110-TA210*TD110
                   TA110=A((3*J+1)*I)
                   A((3*J+2)*I)=TA110*TA210
                   A((3*J+1)*I)=TA110*TB210
                   TC210=C((3*J+2)*I)
                   C((3*J+1)*I)=TC110*TC210
                   C((3*J+2)*I)=TC210*TB110
 10            CONTINUE
C
               B(0)=B(0)-C(0)*A(I)
               D(0)=D(0)-C(0)*D(I)
               C(0)=C(0)*C(I)
CDIR$ IVDEP
C$DIR NO_RECURRENCE
               DO 20 J=1,ND3
                   TC020=C((3*J)*I)
                   TA020=A((3*J)*I)
                   B((3*J)*I)=B((3*J)*I)-TC020*A((3*J+1)*I)
     1                                  -TA020*C((3*J-1)*I)
                   D((3*J)*I)=D((3*J)*I)-TC020*D((3*J+1)*I)
     1                                  -TA020*D((3*J-1)*I)
                   A((3*J)*I)=TA020*A((3*J-1)*I)
                   C((3*J)*I)=TC020*C((3*J+1)*I)
 20            CONTINUE
           ENDIF
           IF(NREM(ISTK).EQ.1) THEN
CDIR$ IVDEP
C$DIR NO_RECURRENCE
               DO 30 J=0,ND3-1
                   TB130=B((3*J+1)*I)
                   TB230=B((3*J+2)*I)
                   TA230=A((3*J+2)*I)
                   TC130=C((3*J+1)*I)
                   TEMP30=1.0/(TB130*TB230-TC130*TA230)
                   TB130=TEMP30*TB130
                   TB230=TEMP30*TB230
                   TA230=TEMP30*TA230
                   TC130=TEMP30*TC130
                   TD130=D((3*J+1)*I)
                   TD230=D((3*J+2)*I)
                   D((3*J+1)*I)=TD130*TB230-TC130*TD230
                   D((3*J+2)*I)=TD230*TB130-TA230*TD130
                   TA130=A((3*J+1)*I)
                   A((3*J+2)*I)=TA130*TA230
                   A((3*J+1)*I)=TA130*TB230
                   TC230=C((3*J+2)*I)
                   C((3*J+1)*I)=TC130*TC230
                   C((3*J+2)*I)=TC230*TB130
 30            CONTINUE
               TEMP31=1.0/B(NM1*I)
               A(NM1*I)=TEMP31*A(NM1*I)
               D(NM1*I)=TEMP31*D(NM1*I)
C
               B(0)=B(0)-C(0)*A(I)
               D(0)=D(0)-C(0)*D(I)
               C(0)=C(0)*C(I)
CDIR$ IVDEP
C$DIR NO_RECURRENCE
               DO 40 J=1,ND3
                   TC040=C((3*J)*I)
                   TA040=A((3*J)*I)
                   B((3*J)*I)=B((3*J)*I)-TC040*A((3*J+1)*I)
     1                                  -TA040*C((3*J-1)*I)
                   D((3*J)*I)=D((3*J)*I)-TC040*D((3*J+1)*I)
     1                                  -TA040*D((3*J-1)*I)
                   A((3*J)*I)=TA040*A((3*J-1)*I)
                   C((3*J)*I)=TC040*C((3*J+1)*I)
 40            CONTINUE
           ENDIF
           IF(NREM(ISTK).EQ.0) THEN
CDIR$ IVDEP
C$DIR NO_RECURRENCE
               DO 50 J=0,ND3-1
                   TB150=B((3*J+1)*I)
                   TB250=B((3*J+2)*I)
                   TA250=A((3*J+2)*I)
                   TC150=C((3*J+1)*I)
                   TEMP50=1.0/(TB150*TB250-TC150*TA250)
                   TB150=TEMP50*TB150
                   TB250=TEMP50*TB250
                   TA250=TEMP50*TA250
                   TC150=TEMP50*TC150
                   TD150=D((3*J+1)*I)
                   TD250=D((3*J+2)*I)
                   D((3*J+1)*I)=TD150*TB250-TC150*TD250
                   D((3*J+2)*I)=TD250*TB150-TA250*TD150
                   TA150=A((3*J+1)*I)
                   A((3*J+2)*I)=TA150*TA250
                   A((3*J+1)*I)=TA150*TB250
                   TC250=C((3*J+2)*I)
                   C((3*J+1)*I)=TC150*TC250
                   C((3*J+2)*I)=TC250*TB150
 50            CONTINUE
C
               B(0)=B(0)-C(0)*A(I)
               D(0)=D(0)-C(0)*D(I)
               C(0)=C(0)*C(I)
CDIR$ IVDEP
C$DIR NO_RECURRENCE
               DO 60 J=1,ND3-1
                   TC060=C((3*J)*I)
                   TA060=A((3*J)*I)
                   B((3*J)*I)=B((3*J)*I)-TC060*A((3*J+1)*I)
     1                                  -TA060*C((3*J-1)*I)
                   D((3*J)*I)=D((3*J)*I)-TC060*D((3*J+1)*I)
     1                                  -TA060*D((3*J-1)*I)
                   A((3*J)*I)=TA060*A((3*J-1)*I)
                   C((3*J)*I)=TC060*C((3*J+1)*I)
 60            CONTINUE
               TA061=A(NM1*I)
               B(NM1*I)=B(NM1*I)-TA061*C((NM1-1)*I)
               D(NM1*I)=D(NM1*I)-TA061*D((NM1-1)*I)
               A(NM1*I)=TA061*A((NM1-1)*I)
           ENDIF
           NM1=ND3
           ISTK=ISTK+1
           I=I*3
       GOTO 101
 102   CONTINUE
C
C SOLVE SMALL SYSTEM
C
       CALL SCTRIC(A,B,C,D,I,NM1)
C
C END REDUCE PHASE
C
C BACK SUBSTITUTE
C
C      DO WHILE(ISTK.GT.1)
 201   CONTINUE
       IF(ISTK.LE.1) GOTO 202
           ISTK=ISTK-1
           I=I/3
           ND3=NM1
           NM1=3*ND3+NREM(ISTK)
           IF(NREM(ISTK).EQ.2) THEN
CDIR$ IVDEP
C$DIR NO_RECURRENCE
               DO 70 J=0,ND3-1
                   D((3*J+1)*I)=D((3*J+1)*I)
     1                  -A((3*J+1)*I)*D((3*J  )*I)            
     2                  +C((3*J+1)*I)*D((3*J+3)*I)
                   D((3*J+2)*I)=D((3*J+2)*I)
     1                  +A((3*J+2)*I)*D((3*J  )*I)            
     2                  -C((3*J+2)*I)*D((3*J+3)*I)
 70            CONTINUE
               D((NM1-1)*I)=D((NM1-1)*I)-A((NM1-1)*I)*D((NM1-2)*I)
               D(NM1*I)=D(NM1*I)+A(NM1*I)*D((NM1-2)*I)
           ENDIF
           IF(NREM(ISTK).EQ.1) THEN
CDIR$ IVDEP
C$DIR NO_RECURRENCE
               DO 80 J=0,ND3-1
                   D((3*J+1)*I)=D((3*J+1)*I)
     1                  -A((3*J+1)*I)*D((3*J  )*I)            
     2                  +C((3*J+1)*I)*D((3*J+3)*I)
                   D((3*J+2)*I)=D((3*J+2)*I)
     1                  +A((3*J+2)*I)*D((3*J  )*I)            
     2                  -C((3*J+2)*I)*D((3*J+3)*I)
 80            CONTINUE
               D(NM1*I)=D(NM1*I)-A(NM1*I)*D((NM1-1)*I)
           ENDIF
           IF(NREM(ISTK).EQ.0) THEN
CDIR$ IVDEP
C$DIR NO_RECURRENCE
               DO 90 J=0,ND3-1
                   D((3*J+1)*I)=D((3*J+1)*I)
     1                  -A((3*J+1)*I)*D((3*J  )*I)            
     2                  +C((3*J+1)*I)*D((3*J+3)*I)
                   D((3*J+2)*I)=D((3*J+2)*I)
     1                  +A((3*J+2)*I)*D((3*J  )*I)            
     2                  -C((3*J+2)*I)*D((3*J+3)*I)
 90            CONTINUE
           ENDIF
       GOTO 201
 202   CONTINUE
C
C END BACK SUBSTITUTION PHASE
C
       RETURN
       END
C
C SCALAR TRIDIAGONAL SOLVER WITH ARBITRARY INTER-ELEMENT SPACING
C
       SUBROUTINE SCTRIC(A,B,C,D,I,NM1)
       IMPLICIT NONE
       INTEGER I,NM1
       REAL A(I,0:NM1)
       REAL B(I,0:NM1)
       REAL C(I,0:NM1)
       REAL D(I,0:NM1)
       INTEGER J
       REAL TEMP,TAMP
       TAMP=1.0/B(1,0)
       D(1,0)=TAMP*D(1,0)
       IF(NM1.EQ.0) RETURN
       C(1,0)=TAMP*C(1,0)
C
CDIR$ NOVECTOR
C$DIR SCALAR
       DO 10 J=1,NM1-1,1
           TEMP=1.0/(B(1,J)-A(1,J)*C(1,J-1))
           D(1,J)=TEMP*(D(1,J)-A(1,J)*D(1,J-1))
           C(1,J)=TEMP*C(1,J)
 10    CONTINUE
       TAMP=1.0/(B(1,NM1)-A(1,NM1)*C(1,NM1-1))
       D(1,NM1)=TAMP*(D(1,NM1)-A(1,NM1)*D(1,NM1-1))
C
CDIR$ NOVECTOR
C$DIR SCALAR
       DO 20 J=NM1-1,0,-1
           D(1,J)=D(1,J)-C(1,J)*D(1,J+1)
 20    CONTINUE
       RETURN
       END


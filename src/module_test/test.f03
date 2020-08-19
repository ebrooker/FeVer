MODULE utils
USE iso_fortran_env, only : int32, int64, real64
IMPLICIT NONE
REAL(real64) :: t1,t2
integer(int64) :: count_max, count_rate, count_1, count_2

CONTAINS

SUBROUTINE tic()
      CALL SYSTEM_CLOCK(count_1,count_max=count_max, count_rate=count_rate)
      t1 = real(count_1,real64)/ count_rate
END SUBROUTINE tic

SUBROUTINE toc()
      CALL SYSTEM_CLOCK(count_2, count_max=count_max, count_rate=count_rate)
      t2 = real(count_2,real64)/ count_rate
END SUBROUTINE toc

END MODULE utils

PROGRAM TEST
USE utils
USE class_gridVector
      IMPLICIT NONE
      TYPE(gridVector) :: x
      real,allocatable :: y(:)
      integer :: i,j
      real :: mag, norms(4)


      WRITE(*,*) ""

      OPEN(UNIT=20,FILE='timing.dat')
      DO i = 1,26
            CALL tic()
            CALL x%init(2**i,0.0,1.0)

            mag = x%nodes%mag()
            norms(1) = x%nodes%norm('1')
            norms(2) = x%nodes%norm('2')
            norms(3) = x%nodes%norm('3')
            norms(4) = x%nodes%norm('Inf')
            CALL toc()
            WRITE(*,'(A,ES8.2,A,I8)') " TIME TAKE --> ", t2-t1, " | ", 2**i
            WRITE(20,*) t2-t1, '   ', 2**i
            CALL x%del
      ENDDO
      CLOSE(20)
      CALL SYSTEM("python plot_timing.py")
      WRITE(*,*) ""
END PROGRAM TEST


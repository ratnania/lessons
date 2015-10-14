SUBROUTINE DO_FAST(x, n, m)
  IMPLICIT NONE
  INTEGER, DIMENSION(n,m), INTENT(INOUT) :: x
  INTEGER, INTENT(IN)    :: n
  INTEGER, INTENT(IN)    :: m 
  ! LOCAL
  INTEGER :: i, j
  
  x = 0
  DO j=1, m 
     DO i=1, n 
        x(i,j) = i+j
     END DO
  END DO

END SUBROUTINE DO_FAST
  
SUBROUTINE DO_SLOW(x, n, m)
  IMPLICIT NONE
  INTEGER, DIMENSION(n,m), INTENT(INOUT) :: x
  INTEGER, INTENT(IN)    :: n
  INTEGER, INTENT(IN)    :: m 
  ! LOCAL
  INTEGER :: i, j
  
  x = 0
  DO i=1, n 
     DO j=1, m 
        x(i,j) = i+j
     END DO
  END DO
 
END SUBROUTINE DO_SLOW

PROGRAM EXAMPLE
  IMPLICIT NONE
  INTEGER, PARAMETER :: n_loops = 10 
  INTEGER :: n
  INTEGER :: m
  INTEGER :: i 
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: x
  REAL    :: start, finish

  n = 100000 ; m = 1000 
  ALLOCATE(x(n,m))

  CALL CPU_TIME(start)
  DO i=1, n_loops 
     CALL DO_FAST(x, n, m)
  END DO
  CALL CPU_TIME(finish)
  print *, "> Done with fast."
  print '("Time = ",f6.3," seconds.")',(finish-start)/n_loops

  CALL CPU_TIME(start)
  DO i=1, n_loops 
     CALL DO_slow(x, n, m)
  END DO
  CALL CPU_TIME(finish)
  print *, "> Done with slow."
  print '("Time = ",f6.3," seconds.")',(finish-start)/n_loops

END PROGRAM EXAMPLE

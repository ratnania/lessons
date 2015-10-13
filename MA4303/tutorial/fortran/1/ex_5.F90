! ...................................................
PROGRAM EX_5
IMPLICIT NONE
   INTEGER, PARAMETER :: M = 5
   INTEGER, PARAMETER :: N = 5
   REAL(KIND=8), DIMENSION(M,N) :: Mat 
   REAL(KIND=8), DIMENSION(N)   :: x 
   REAL(KIND=8), DIMENSION(M)   :: y 
   INTEGER :: i
   INTEGER :: j 

   ! ...
   Mat = 0.0
   DO j = 1, N
      DO i = 1, M
         Mat(i,j) = (i+j) * 1.0
      END DO
   END DO
   ! ...

   ! ...
   x = 1.0
   ! ...

   ! ...
   CALL MATRIX_PRODUCT_VECTOR_DENSE(Mat, x, y)
   ! ...

   PRINT *, "y = M x : ", y

CONTAINS
   ! ...................................................
   SUBROUTINE MATRIX_PRODUCT_VECTOR_DENSE(A, x, y)
   IMPLICIT NONE
      REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: A
      REAL(KIND=8), DIMENSION(:), INTENT(IN)      :: x 
      REAL(KIND=8), DIMENSION(:), INTENT(INOUT)   :: y
      ! LOCAL
      INTEGER :: M
      INTEGER :: N 
      INTEGER :: i
      INTEGER :: j 

      M = SIZE(A,1)
      N = SIZE(A,2)

      ! ...
      y = 0.0
      DO i = 1, M
         DO j = 1, N
            y(i) = y(i) + A(i,j) * x(j) 
         END DO
      END DO
      ! ...

   END SUBROUTINE MATRIX_PRODUCT_VECTOR_DENSE
   ! ...................................................
END PROGRAM EX_5 
! ...................................................

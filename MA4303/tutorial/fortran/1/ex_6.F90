! ...................................................
PROGRAM EX_6
IMPLICIT NONE
   INTEGER, PARAMETER              :: N = 4 
   INTEGER, PARAMETER              :: NNZ = 9
   REAL(KIND=8)   , DIMENSION(N)   :: x 
   REAL(KIND=8)   , DIMENSION(N)   :: y
   REAL(KIND=8)   , DIMENSION(NNZ) :: A
   INTEGER(KIND=8), DIMENSION(NNZ) :: JA 
   INTEGER(KIND=4), DIMENSION(N+1) :: IA
  
   ! ...
   JA(1) = 1      ; A(1) = 1.0   
   JA(2) = 3      ; A(2) = 2.0 
   JA(3) = 4      ; A(3) = 3.0 
   JA(4) = 2      ; A(4) = 4.0 
   JA(5) = 2      ; A(5) = 5.0 
   JA(6) = 3      ; A(6) = 6.0 
   JA(7) = 2      ; A(7) = 7.0 
   JA(8) = 3      ; A(8) = 8.0 
   JA(9) = 4      ; A(9) = 9.0 
   ! ...
  
   ! ...
   IA(1) = 1 
   IA(2) = 4 
   IA(3) = 5
   IA(4) = 7
   IA(5) = 10
   ! ...

   ! ...
   x = 1.0
   ! ...

   ! ...
   CALL MATRIX_PRODUCT_VECTOR_CSR(IA, JA, A, x, y)
   ! ...

   PRINT *, "y = M x : ", y

CONTAINS
   ! ...................................................
   SUBROUTINE MATRIX_PRODUCT_VECTOR_CSR(IA, JA, A, x, y)
   IMPLICIT NONE
      REAL(KIND=8)   , DIMENSION(:), INTENT(IN) :: A
      INTEGER(KIND=8), DIMENSION(:), INTENT(IN) :: JA 
      INTEGER(KIND=4), DIMENSION(:), INTENT(IN) :: IA
      REAL(KIND=8), DIMENSION(:), INTENT(IN)      :: x 
      REAL(KIND=8), DIMENSION(:), INTENT(INOUT)   :: y
      ! LOCAL
      INTEGER :: N 
      INTEGER :: i
      INTEGER :: k_max 
      INTEGER :: k_min 

      N = SIZE(IA,1) - 1

      ! ...
      y = 0.0
      DO i = 1, N
         k_min = IA(i)
         k_max = IA(i+1) - 1 
         PRINT *, i, k_min, k_max
         y(i) = DOT_PRODUCT(A(k_min:k_max), X(JA(k_min:k_max))) 
      END DO
      ! ...
   END SUBROUTINE MATRIX_PRODUCT_VECTOR_CSR
   ! ...................................................

END PROGRAM EX_6 
! ...................................................

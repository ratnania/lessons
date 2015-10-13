! ...................................................
MODULE MATRIX_UTILITIES
IMPLICIT NONE
   PRIVATE
   PUBLIC :: MATRIX, &
           & MATRIX_CREATE_DENSE, MATRIX_CREATE_CSR,   & 
           & MATRIX_FREE_DENSE   , MATRIX_FREE_CSR,    & 
           & MATRIX_PRODUCT_VECTOR

   TYPE MATRIX
      INTEGER :: matrix_type
      INTEGER :: N 
      INTEGER :: NNZ 
      REAL(KIND=8)   , DIMENSION(:,:), ALLOCATABLE :: dense_A
      REAL(KIND=8)   , DIMENSION(:)  , ALLOCATABLE :: csr_A 
      INTEGER(KIND=4), DIMENSION(:)  , ALLOCATABLE :: csr_IA 
      INTEGER(KIND=8), DIMENSION(:)  , ALLOCATABLE :: csr_JA 
   END TYPE MATRIX

CONTAINS
   ! ...................................................
   SUBROUTINE MATRIX_CREATE_DENSE(self, Mat)
   IMPLICIT NONE
      TYPE(MATRIX), INTENT(INOUT) :: self
      REAL(KIND=8), DIMENSION(:,:), INTENT(IN)      :: Mat 
      ! LOCAL

      IF (SIZE(Mat, 1) .NE. SIZE(Mat, 2)) THEN
          STOP 'Only square matrices can be used'
      END IF

      self % N = SIZE(Mat,1)

      ALLOCATE(self % dense_A(self % N, self % N))

      self % dense_A = Mat

      self % matrix_type = 0 
      
   END SUBROUTINE MATRIX_CREATE_DENSE
   ! ...................................................

   ! ...................................................
   SUBROUTINE MATRIX_FREE_DENSE(self)
   IMPLICIT NONE
      TYPE(MATRIX), INTENT(INOUT) :: self
      ! LOCAL

      DEALLOCATE(self % dense_A)
      
   END SUBROUTINE MATRIX_FREE_DENSE
   ! ...................................................

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

   ! ...................................................
   SUBROUTINE MATRIX_CREATE_CSR(self, IA, JA, A)
   IMPLICIT NONE
      TYPE(MATRIX), INTENT(INOUT) :: self
      INTEGER(KIND=8), DIMENSION(:) :: JA 
      INTEGER(KIND=4), DIMENSION(:) :: IA      
      REAL(KIND=8)   , DIMENSION(:) :: A
      ! LOCAL

      self % N   = SIZE(IA,1) - 1
      self % NNZ = SIZE(JA,1)

      ALLOCATE(self % csr_IA(self % N + 1))
      ALLOCATE(self % csr_JA(self % NNZ))
      ALLOCATE(self % csr_A (self % NNZ))

      self % csr_IA = IA
      self % csr_JA = JA
      self % csr_A  = A

      self % matrix_type = 1 
      
   END SUBROUTINE MATRIX_CREATE_CSR
   ! ...................................................

   ! ...................................................
   SUBROUTINE MATRIX_FREE_CSR(self)
   IMPLICIT NONE
      TYPE(MATRIX), INTENT(INOUT) :: self
      ! LOCAL

      DEALLOCATE(self % csr_IA)
      DEALLOCATE(self % csr_JA)
      DEALLOCATE(self % csr_A )
      
   END SUBROUTINE MATRIX_FREE_CSR
   ! ...................................................

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
         y(i) = DOT_PRODUCT(A(k_min:k_max), X(JA(k_min:k_max))) 
      END DO
      ! ...
   END SUBROUTINE MATRIX_PRODUCT_VECTOR_CSR
   ! ...................................................

   ! ...................................................
   SUBROUTINE MATRIX_PRODUCT_VECTOR(self, x, y)
   IMPLICIT NONE
      TYPE(MATRIX), INTENT(IN) :: self
      REAL(KIND=8), DIMENSION(:), INTENT(IN)      :: x 
      REAL(KIND=8), DIMENSION(:), INTENT(INOUT)   :: y
      ! LOCAL

      SELECT CASE(self % matrix_type)
         CASE(0)
            CALL MATRIX_PRODUCT_VECTOR_DENSE(self % dense_A, x, y)
         CASE(1)
            CALL MATRIX_PRODUCT_VECTOR_CSR(self % csr_IA, self % csr_JA, self % csr_A, x, y)
         CASE DEFAULT
            PRINT *, "MATRIX_PRODUCT_VECTOR: matrix-type not yet implemented"
      END SELECT

   END SUBROUTINE MATRIX_PRODUCT_VECTOR
   ! ...................................................

END MODULE MATRIX_UTILITIES 
! ...................................................

! ...................................................
PROGRAM EX_8
USE MATRIX_UTILITIES
IMPLICIT NONE
   INTEGER, PARAMETER              :: M = 5
   INTEGER, PARAMETER              :: N = 4 
   INTEGER, PARAMETER              :: NNZ = 9
   REAL(KIND=8)   , DIMENSION(NNZ) :: A
   INTEGER(KIND=8), DIMENSION(NNZ) :: JA 
   INTEGER(KIND=4), DIMENSION(N+1) :: IA
   REAL(KIND=8), DIMENSION(M,M)    :: Mat 
   REAL(KIND=8), DIMENSION(N)      :: x1 
   REAL(KIND=8), DIMENSION(N)      :: y1 
   REAL(KIND=8), DIMENSION(M)      :: x2 
   REAL(KIND=8), DIMENSION(M)      :: y2 
   INTEGER :: i
   INTEGER :: j 
   TYPE(MATRIX) :: csr
   TYPE(MATRIX) :: dense 
  
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
   Mat = 0.0
   DO j = 1, N
      DO i = 1, M
         Mat(i,j) = (i+j) * 1.0
      END DO
   END DO
   ! ...

   ! ...
   x1 = 1.0 ; x2 = 1.0
   ! ...

   ! ...
   CALL MATRIX_CREATE_CSR(csr, IA, JA, A)
   CALL MATRIX_PRODUCT_VECTOR(csr, x1, y1)
   CALL MATRIX_FREE_CSR(csr)
   ! ...

   ! ...
   CALL MATRIX_CREATE_DENSE(dense, Mat)
   CALL MATRIX_PRODUCT_VECTOR(dense, x2, y2)
   CALL MATRIX_FREE_DENSE(dense)
   ! ...

   PRINT *, "y2 = M2 x2 : ", y2
   PRINT *, "y1 = M1 x1 : ", y1

END PROGRAM EX_8 
! ...................................................


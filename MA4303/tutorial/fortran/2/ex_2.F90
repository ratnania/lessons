! ...................................................
MODULE MATRIX_UTILITIES
IMPLICIT NONE
   PRIVATE
   PUBLIC :: MATRIX, MATRIX_DENSE, MATRIX_CSR,         & 
           & MATRIX_CREATE_DENSE, MATRIX_CREATE_CSR

   TYPE MATRIX
      INTEGER :: N 
   END TYPE MATRIX

   TYPE, EXTENDS(MATRIX) :: MATRIX_DENSE
      REAL(KIND=8)   , DIMENSION(:,:), ALLOCATABLE :: coefficients 
   CONTAINS
      PROCEDURE :: CREATE => MATRIX_CREATE_DENSE
      PROCEDURE :: FREE   => MATRIX_FREE_DENSE
      PROCEDURE :: DOT    => MATRIX_PRODUCT_VECTOR_DENSE
   END TYPE MATRIX_DENSE

   TYPE, EXTENDS(MATRIX) :: MATRIX_CSR
      INTEGER :: NNZ 
      INTEGER(KIND=4), DIMENSION(:), ALLOCATABLE :: IA 
      INTEGER(KIND=8), DIMENSION(:), ALLOCATABLE :: JA 
      REAL(KIND=8)   , DIMENSION(:), ALLOCATABLE :: A 
   CONTAINS
      PROCEDURE :: CREATE => MATRIX_CREATE_CSR
      PROCEDURE :: FREE   => MATRIX_FREE_CSR
      PROCEDURE :: DOT    => MATRIX_PRODUCT_VECTOR_CSR
   END TYPE MATRIX_CSR
CONTAINS
   ! ...................................................
   SUBROUTINE MATRIX_CREATE_DENSE(self, Mat)
   IMPLICIT NONE
      CLASS(MATRIX_DENSE), INTENT(INOUT) :: self
      REAL(KIND=8), DIMENSION(:,:), INTENT(IN)      :: Mat 
      ! LOCAL

      IF (SIZE(Mat, 1) .NE. SIZE(Mat, 2)) THEN
          STOP 'Only square matrices can be used'
      END IF

      self % N = SIZE(Mat,1)

      ALLOCATE(self % coefficients(self % N, self % N))

      self % coefficients = Mat

   END SUBROUTINE MATRIX_CREATE_DENSE
   ! ...................................................

   ! ...................................................
   SUBROUTINE MATRIX_FREE_DENSE(self)
   IMPLICIT NONE
      CLASS(MATRIX_DENSE), INTENT(INOUT) :: self
      ! LOCAL

      DEALLOCATE(self % coefficients)
      
   END SUBROUTINE MATRIX_FREE_DENSE
   ! ...................................................

   ! ...................................................
   SUBROUTINE MATRIX_PRODUCT_VECTOR_DENSE(self, x, y)
   IMPLICIT NONE
      CLASS(MATRIX_DENSE), INTENT(IN) :: self
      REAL(KIND=8), DIMENSION(:), INTENT(IN)      :: x 
      REAL(KIND=8), DIMENSION(:), INTENT(INOUT)   :: y
      ! LOCAL
      INTEGER :: i
      INTEGER :: j 

      ! ...
      y = 0.0
      DO i = 1, self % N
         DO j = 1, self % N
            y(i) = y(i) + self % coefficients(i,j) * x(j) 
         END DO
      END DO
      ! ...

   END SUBROUTINE MATRIX_PRODUCT_VECTOR_DENSE
   ! ...................................................

   ! ...................................................
   SUBROUTINE MATRIX_CREATE_CSR(self, IA, JA, A)
   IMPLICIT NONE
      CLASS(MATRIX_CSR), INTENT(INOUT) :: self
      INTEGER(KIND=8), DIMENSION(:) :: JA 
      INTEGER(KIND=4), DIMENSION(:) :: IA      
      REAL(KIND=8)   , DIMENSION(:) :: A
      ! LOCAL

      self % N   = SIZE(IA,1) - 1
      self % NNZ = SIZE(JA,1)

      ALLOCATE(self % IA(self % N + 1))
      ALLOCATE(self % JA(self % NNZ))
      ALLOCATE(self % A (self % NNZ))

      self % IA = IA
      self % JA = JA
      self % A  = A
      
   END SUBROUTINE MATRIX_CREATE_CSR
   ! ...................................................

   ! ...................................................
   SUBROUTINE MATRIX_FREE_CSR(self)
   IMPLICIT NONE
      CLASS(MATRIX_CSR), INTENT(INOUT) :: self
      ! LOCAL

      DEALLOCATE(self % IA)
      DEALLOCATE(self % JA)
      DEALLOCATE(self % A )
      
   END SUBROUTINE MATRIX_FREE_CSR
   ! ...................................................

   ! ...................................................
   SUBROUTINE MATRIX_PRODUCT_VECTOR_CSR(self, x, y)
   IMPLICIT NONE
      CLASS(MATRIX_CSR), INTENT(IN) :: self
      REAL(KIND=8), DIMENSION(:), INTENT(IN)      :: x 
      REAL(KIND=8), DIMENSION(:), INTENT(INOUT)   :: y
      ! LOCAL
      INTEGER :: i
      INTEGER :: k_max 
      INTEGER :: k_min 

      ! ...
      y = 0.0
      DO i = 1, self % N
         k_min = self % IA(i)
         k_max = self % IA(i+1) - 1 
         y(i) = DOT_PRODUCT(self % A(k_min:k_max), X(self % JA(k_min:k_max))) 
      END DO
      ! ...
   END SUBROUTINE MATRIX_PRODUCT_VECTOR_CSR
   ! ...................................................

END MODULE MATRIX_UTILITIES 
! ...................................................

! ...................................................
PROGRAM EX_1
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
   TYPE(MATRIX_DENSE), TARGET :: dense 
   TYPE(MATRIX_CSR), TARGET :: csr
   CLASS(MATRIX), POINTER :: ptr_matrix
    
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

   ptr_matrix => dense 
   ptr_matrix => csr

   ! ...
   CALL csr % CREATE(IA, JA, A)
   CALL csr % DOT (x1, y1)
   CALL csr % FREE()
   ! ...

   ! ...
   CALL dense % CREATE(Mat)
   CALL dense % DOT(x2, y2)
   CALL dense % FREE()
   ! ...

END PROGRAM EX_1 
! ...................................................

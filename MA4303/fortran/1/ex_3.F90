PROGRAM EX_3 
IMPLICIT NONE
   CALL MY_PRINT_1()
   CALL MY_PRINT_2()
CONTAINS
   ! ...................................................
   SUBROUTINE MY_PRINT_1()
   IMPLICIT NONE
      INTEGER :: i
      INTEGER, PARAMETER :: J = 1 
      REAL(KIND=8) :: x
      REAL(KIND=4) :: y

      PRINT *, ">>> Enter MY_PRINT_1"

      i = J + 1
      x = 2.0 * i
      y = 2.0 * i

      PRINT *, "J : ", J
      PRINT *, "I : ", i
      PRINT *, "X : ", x
      PRINT *, "Y : ", y

      PRINT *, "<<< Leave MY_PRINT_1"

   END SUBROUTINE MY_PRINT_1
   ! ...................................................

   ! ...................................................
   SUBROUTINE MY_PRINT_2()
   IMPLICIT NONE
      INTEGER, PARAMETER :: N = 5 
      INTEGER :: i
      INTEGER, DIMENSION(5) :: arr_i 
      REAL(KIND=8), DIMENSION(N) :: arr_x
      REAL(KIND=8), DIMENSION(N) :: arr_y

      PRINT *, ">>> Enter MY_PRINT_2"

      arr_i = 1
      
      DO i = 1, N
         arr_x(i) = i * 1.0d0 / N
         arr_y(i) = i * 1.0 / N
      END DO

      PRINT *, "ARR_I : ", arr_i
      PRINT *, "ARR_X : ", arr_x
      PRINT *, "ARR_Y : ", arr_y

      PRINT *, "<<< Leave MY_PRINT_2"

   END SUBROUTINE MY_PRINT_2
   ! ...................................................

END PROGRAM EX_3 

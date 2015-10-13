! ...................................................
PROGRAM EX_7
IMPLICIT NONE
! Example that generate A Laplacian with Dirichlet BC and FD 
!
! The laplacian looks like :
!         >    2  -1            ... 0
!         >   -1   2  -1   0    ... 0
!         >    0  -1   2  -1  0 ... 0
! 1/dx^2  >          ....
!         >          ....
!         >    0     ...    -1  2  -1
!         >    0     ...       -1   2
!
   INTEGER, PARAMETER :: N = 5
   REAL(KIND=8), DIMENSION(N,N) :: Mat 
   INTEGER :: i
   REAL(KIND=8) :: dx

   ! ...
   dx = 1./N
   ! ...

   ! ...
   Mat = 0.0
   DO i = 1, N 
      IF (i == 1) THEN
         Mat(i,  i) =  2.0  
         Mat(i,i+1) = -1.0  
      ELSEIF (i == N) THEN
         Mat(i,i-1) = -1.0  
         Mat(i,  i) =  2.0  
      ELSE
         Mat(i,i-1) = -1.0  
         Mat(i,  i) =  2.0  
         Mat(i,i+1) = -1.0  
      END IF
   END DO
   Mat = (1./dx**2) * Mat
   ! ...

END PROGRAM EX_7 
! ...................................................

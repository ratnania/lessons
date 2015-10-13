This directory contains the following files:

- fem1d_lagrange.F90
  contains all functions/subroutines for the 1d lagrange fem
- main.F90
  contains the main program and some tests

To compile these files:

gfortran -c fem1d_lagrange.F90
gfortran main.F90 fem1d_lagrange.o -o fem.exe

to run it:

./fem.exe


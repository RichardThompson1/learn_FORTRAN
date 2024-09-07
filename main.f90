! program program_name
!   use some_modules        #like imports?
!   implicit none           #?
!   variable, derived type and interface declarations     #specification section
!   executable statements           #
! contains
!   procedure definitions
! end program

program demo
    implicit none

    integer :: x, y, z, i, j
    integer, parameter :: inches_per_foot = 12      !to make a const
    integer :: squares(10), small_matrix(2,2)
    integer :: num, unit_1, unit_2, iostat
    logical :: file_exists

    x = 3
    y = 7
    z = (sin(0.5-0.5)*x+fib(8)*y-2**4)/5

    squares = 42
    print *, squares

    do i = 1, 10
        squares(i) = i**2
    end do
    print *, squares

    i = 0
    do while (i > 1)
        print *, squares(i)
        i = i-1
    end do

    print *, squares(0)
    print *, squares(1)
    print *, squares(-1)

    print *, "small_matrix = ", small_matrix
    i = 0
    j = 0
    do 
        do
            small_matrix(i,j) = i**j
            if (j >= 2) then
                j = 0
                exit
            end if
            print *, "i = ", i, ", j = ", j
            j = j+1
        end do
        if (i >= 2) then
            exit
        end if
        i = i+1
    end do

    print *, small_matrix

    inquire(file="../in.txt",exist=file_exists)
    if (file_exists) then
        ! Open the input file
        open(newunit=unit_1, file="../in.txt", status="old", action="read")

        ! Open or create the output file
        open(newunit=unit_2, file="../out.txt", status="replace", action="write")

        ! Read from file, perform some operation and write the result
        do
            read(unit_1, *, iostat=iostat) num
            if (iostat /= 0) exit   ! Exit the loop if read fails (end of file or error)

            num = num + 2 

            ! Write result to output file
            write(unit_2, '(I10)') num
        end do

        ! Close both files
        close(unit_1)
        close(unit_2)
    else
        print *, "Input file does not exist.........................."
    end if

contains
    pure recursive function fib(n) result(fib_)
        integer, intent(in) :: n
        integer :: fib_

        if (n == 1 .or. n == 2) then
            fib_ = 1
        else 
            fib_ = fib(n-1) + fib(n-2)
        end if
    end function
end program 
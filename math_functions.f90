

module math_functions
    implicit none

    real, public :: pi = 3.141592653589793
    integer, public :: call_count = 0

contains
    pure function square(n) result(res)
        integer, intent(in) :: n
        integer :: res
        res = n * n
    end function square

    pure function cube(n) result(res)
        integer, intent(in) :: n
        integer res
        res = n * n * n
    end function cube

    pure recursive function fib(n) result(fib_)
        integer, intent(in) :: n
        integer :: fib_ 

        if (n <= 0) then 
            fib_ = 0
        else if (n == 1 .or. n == 2) then
            fib_ = 1
        else 
            fib_ = fib(n-1) + fib(n-2)
        end if
    end function fib

    subroutine reset_call_count()
        call_count = 0
    end subroutine reset_call_count

end module math_functions
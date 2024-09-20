

module point_cartesian
    implicit none

    type :: Point
        real :: x = 0.0
        real :: y = 0.0
    contains
        procedure :: set_coordinates
        procedure :: get_coordinates
        procedure :: distance_to
    end type Point

contains

    subroutine set_coordinates(this, x_val, y_val)
        class(Point), intent(inout) :: this
        real, intent(in) :: x_val, y_val
        this%x = x_val
        this%y = y_val
    end subroutine set_coordinates

    subroutine get_coordinates(this, x_val, y_val)
        class(Point), intent(in) :: this
        real, intent(out) :: x_val, y_val
        x_val = this%x
        y_val = this%y
    end subroutine get_coordinates

    pure function distance_to(this, other) result(dist)
        class(Point), intent(in) :: this
        class(Point), intent(in) :: other
        real :: dist
        dist = sqrt((this%x - other%x)**2 + (this%y - other%y)**2)
    end function distance_to

end module point_cartesian

module mini_test

    private

    public :: check

    interface
        !> Version: experimental
        !> 
        !> `warn == .true.`(default): if `condition == .false.` then error stop `msg`;
        !> `warn == .false.`        : if `condition == .false.` then print      `msg`.
        module subroutine check(condition, msg, warn)
            logical,          intent(in)            :: condition
            character(len=*), intent(in)            :: msg
            logical,          intent(in), optional  :: warn
        end subroutine check
    end interface

end module mini_test
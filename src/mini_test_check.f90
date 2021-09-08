!> This submodule draws on the `check` subroutine of `fortran-lang/stdlib`!
submodule(mini_test) mini_test_check

contains

    !> Checks `condition` is `.true.` or not.
    module subroutine check(condition, msg, warn)

        logical,          intent(in)            :: condition
        character(len=*), intent(in)            :: msg
        logical,          intent(in), optional  :: warn
        
        logical                                 :: warn_

        warn_ = optval(warn, .false.)
            
        if     (     warn_ .and. .not.condition) then;  print *,   msg
        elseif (.not.warn_ .and. .not.condition) then;  error stop msg
        end if

    end subroutine check

end submodule mini_test_check
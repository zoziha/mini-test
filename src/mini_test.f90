module mini_test

    use iso_fortran_env, only: sp => real32, dp => real64, qp => real128
    private

    public :: check, is_close

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

    !> Version: experimental
    !>
    !> Determines whether the values of `a` and `b` are close.
    interface is_close
        elemental module function is_close_rsp(a, b, rtol, atol) result(bool)
            real(sp),       intent(in)            :: a, b
            real(sp), intent(in), optional  :: rtol, atol
            logical                             :: bool
        end function is_close_rsp
        elemental module function is_close_rdp(a, b, rtol, atol) result(bool)
            real(dp),       intent(in)            :: a, b
            real(dp), intent(in), optional  :: rtol, atol
            logical                             :: bool
        end function is_close_rdp
        elemental module function is_close_rqp(a, b, rtol, atol) result(bool)
            real(qp),       intent(in)            :: a, b
            real(qp), intent(in), optional  :: rtol, atol
            logical                             :: bool
        end function is_close_rqp
        elemental module function is_close_csp(a, b, rtol, atol) result(bool)
            complex(sp),       intent(in)            :: a, b
            real(sp), intent(in), optional  :: rtol, atol
            logical                             :: bool
        end function is_close_csp
        elemental module function is_close_cdp(a, b, rtol, atol) result(bool)
            complex(dp),       intent(in)            :: a, b
            real(dp), intent(in), optional  :: rtol, atol
            logical                             :: bool
        end function is_close_cdp
        elemental module function is_close_cqp(a, b, rtol, atol) result(bool)
            complex(qp),       intent(in)            :: a, b
            real(qp), intent(in), optional  :: rtol, atol
            logical                             :: bool
        end function is_close_cqp
    end interface is_close

    !> Version: experimental
    !>
    !> if `x` is presented, then `x_ = x`;
    !> if not,              then `x_ = default`.
    interface optval
        elemental module function optval_rsp(x, default) result(x_)
            ! import
            real(sp), intent(in), optional    :: x
            real(sp), intent(in)              :: default
            real(sp)                          :: x_
        end function optval_rsp
        elemental module function optval_rdp(x, default) result(x_)
            ! import
            real(dp), intent(in), optional    :: x
            real(dp), intent(in)              :: default
            real(dp)                          :: x_
        end function optval_rdp
        elemental module function optval_rqp(x, default) result(x_)
            ! import
            real(qp), intent(in), optional    :: x
            real(qp), intent(in)              :: default
            real(qp)                          :: x_
        end function optval_rqp
        elemental module function optval_ll1(x, default) result(x_)
            ! import
            logical, intent(in), optional    :: x
            logical, intent(in)              :: default
            logical                          :: x_
        end function optval_ll1
    end interface optval

end module mini_test
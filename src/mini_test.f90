module mini_test

    use mini_test_kinds
    private

    public :: check, is_close, disp
    public :: optval, to_string

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
            real(sp), intent(in), optional            :: x
            real(sp), intent(in)                      :: default
            real(sp)                                  :: x_
        end function optval_rsp
        elemental module function optval_rdp(x, default) result(x_)
            real(dp), intent(in), optional            :: x
            real(dp), intent(in)                      :: default
            real(dp)                                  :: x_
        end function optval_rdp
        elemental module function optval_rqp(x, default) result(x_)
            real(qp), intent(in), optional            :: x
            real(qp), intent(in)                      :: default
            real(qp)                                  :: x_
        end function optval_rqp
        elemental module function optval_ll1(x, default) result(x_)
            logical, intent(in), optional            :: x
            logical, intent(in)                      :: default
            logical                                  :: x_
        end function optval_ll1
        pure module function optval_character(x, default) result(x_)
            character(len=*), intent(in), optional  :: x
            character(len=*), intent(in)            :: default
            character(len=:), allocatable           :: x_
        end function optval_character
    end interface optval

    !> version: experimental
    !>
    !> Display a scalar, vector or matrix.
    interface disp
        module subroutine disp_0_rsp(value, header, brief)
            real(sp), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_rsp
        module subroutine disp_1_rsp(value, header, brief)
            real(sp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_rsp
        module subroutine disp_2_rsp(value, header, brief)
            real(sp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_rsp
        module subroutine disp_0_rdp(value, header, brief)
            real(dp), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_rdp
        module subroutine disp_1_rdp(value, header, brief)
            real(dp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_rdp
        module subroutine disp_2_rdp(value, header, brief)
            real(dp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_rdp
        module subroutine disp_0_rqp(value, header, brief)
            real(qp), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_rqp
        module subroutine disp_1_rqp(value, header, brief)
            real(qp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_rqp
        module subroutine disp_2_rqp(value, header, brief)
            real(qp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_rqp
        module subroutine disp_0_iint8(value, header, brief)
            integer(int8), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_iint8
        module subroutine disp_1_iint8(value, header, brief)
            integer(int8), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_iint8
        module subroutine disp_2_iint8(value, header, brief)
            integer(int8), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_iint8
        module subroutine disp_0_iint16(value, header, brief)
            integer(int16), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_iint16
        module subroutine disp_1_iint16(value, header, brief)
            integer(int16), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_iint16
        module subroutine disp_2_iint16(value, header, brief)
            integer(int16), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_iint16
        module subroutine disp_0_iint32(value, header, brief)
            integer(int32), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_iint32
        module subroutine disp_1_iint32(value, header, brief)
            integer(int32), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_iint32
        module subroutine disp_2_iint32(value, header, brief)
            integer(int32), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_iint32
        module subroutine disp_0_iint64(value, header, brief)
            integer(int64), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_iint64
        module subroutine disp_1_iint64(value, header, brief)
            integer(int64), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_iint64
        module subroutine disp_2_iint64(value, header, brief)
            integer(int64), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_iint64
        module subroutine disp_0_csp(value, header, brief)
            complex(sp), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_csp
        module subroutine disp_1_csp(value, header, brief)
            complex(sp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_csp
        module subroutine disp_2_csp(value, header, brief)
            complex(sp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_csp
        module subroutine disp_0_cdp(value, header, brief)
            complex(dp), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_cdp
        module subroutine disp_1_cdp(value, header, brief)
            complex(dp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_cdp
        module subroutine disp_2_cdp(value, header, brief)
            complex(dp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_cdp
        module subroutine disp_0_cqp(value, header, brief)
            complex(qp), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_cqp
        module subroutine disp_1_cqp(value, header, brief)
            complex(qp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_cqp
        module subroutine disp_2_cqp(value, header, brief)
            complex(qp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_cqp
        module subroutine disp_0_llk(value, header, brief)
            logical(lk), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_llk
        module subroutine disp_1_llk(value, header, brief)
            logical(lk), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_llk
        module subroutine disp_2_llk(value, header, brief)
            logical(lk), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_llk
        module subroutine disp_0_lc_bool(value, header, brief)
            logical(c_bool), intent(in) :: value
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_0_lc_bool
        module subroutine disp_1_lc_bool(value, header, brief)
            logical(c_bool), intent(in) :: value(:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_1_lc_bool
        module subroutine disp_2_lc_bool(value, header, brief)
            logical(c_bool), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: header
            logical, intent(in), optional :: brief
        end subroutine disp_2_lc_bool
        module subroutine disp_character(value, header, brief)
            character(len=*), intent(in), optional :: value
            character(len=*), intent(in), optional :: header
            logical,          intent(in), optional :: brief
        end subroutine disp_character
    end interface disp

    !> Version: experimental
    !>
    !> Format or transfer other types as a string.
    interface to_string
        pure module function to_string_r_sp(value, format) result(string)
            real(sp), intent(in) :: value
            character(len=*), intent(in), optional :: format
            character(len=:), allocatable          :: string
        end function to_string_r_sp
        pure module function to_string_r_dp(value, format) result(string)
            real(dp), intent(in) :: value
            character(len=*), intent(in), optional :: format
            character(len=:), allocatable          :: string
        end function to_string_r_dp
        pure module function to_string_r_qp(value, format) result(string)
            real(qp), intent(in) :: value
            character(len=*), intent(in), optional :: format
            character(len=:), allocatable          :: string
        end function to_string_r_qp
        pure module function to_string_c_sp(value, format) result(string)
            complex(sp), intent(in) :: value
            character(len=*), intent(in), optional :: format
            character(len=:), allocatable          :: string
        end function to_string_c_sp
        pure module function to_string_c_dp(value, format) result(string)
            complex(dp), intent(in) :: value
            character(len=*), intent(in), optional :: format
            character(len=:), allocatable          :: string
        end function to_string_c_dp
        pure module function to_string_c_qp(value, format) result(string)
            complex(qp), intent(in) :: value
            character(len=*), intent(in), optional :: format
            character(len=:), allocatable          :: string
        end function to_string_c_qp
        pure module function to_string_1_i_int8(value) result(string)
            integer(int8), intent(in) :: value
            character(len=:), allocatable :: string 
        end function to_string_1_i_int8
        pure module function to_string_2_i_int8(value, format) result(string)
            integer(int8), intent(in) :: value
            character(len=*), intent(in)  :: format
            character(len=:), allocatable :: string
        end function to_string_2_i_int8
        pure module function to_string_1_i_int16(value) result(string)
            integer(int16), intent(in) :: value
            character(len=:), allocatable :: string 
        end function to_string_1_i_int16
        pure module function to_string_2_i_int16(value, format) result(string)
            integer(int16), intent(in) :: value
            character(len=*), intent(in)  :: format
            character(len=:), allocatable :: string
        end function to_string_2_i_int16
        pure module function to_string_1_i_int32(value) result(string)
            integer(int32), intent(in) :: value
            character(len=:), allocatable :: string 
        end function to_string_1_i_int32
        pure module function to_string_2_i_int32(value, format) result(string)
            integer(int32), intent(in) :: value
            character(len=*), intent(in)  :: format
            character(len=:), allocatable :: string
        end function to_string_2_i_int32
        pure module function to_string_1_i_int64(value) result(string)
            integer(int64), intent(in) :: value
            character(len=:), allocatable :: string 
        end function to_string_1_i_int64
        pure module function to_string_2_i_int64(value, format) result(string)
            integer(int64), intent(in) :: value
            character(len=*), intent(in)  :: format
            character(len=:), allocatable :: string
        end function to_string_2_i_int64
        pure module function to_string_1_l_lk(value) result(string)
            logical(lk), intent(in) :: value
            character(len=1) :: string 
        end function to_string_1_l_lk
        pure module function to_string_2_l_lk(value, format) result(string)
            logical(lk), intent(in) :: value
            character(len=*), intent(in)  :: format
            character(len=:), allocatable :: string
        end function to_string_2_l_lk
        pure module function to_string_1_l_c_bool(value) result(string)
            logical(c_bool), intent(in) :: value
            character(len=1) :: string 
        end function to_string_1_l_c_bool
        pure module function to_string_2_l_c_bool(value, format) result(string)
            logical(c_bool), intent(in) :: value
            character(len=*), intent(in)  :: format
            character(len=:), allocatable :: string
        end function to_string_2_l_c_bool
    end interface to_string

end module mini_test
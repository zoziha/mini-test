submodule(mini_test) mini_test_optval

contains

    elemental module function optval_rsp(x, default) result(x_)
        real(sp), intent(in), optional    :: x
        real(sp), intent(in)              :: default
        real(sp)                          :: x_

        if (present(x)) then;   x_ = x
        else;                   x_ = default
        end if

    end function optval_rsp
    elemental module function optval_rdp(x, default) result(x_)
        real(dp), intent(in), optional    :: x
        real(dp), intent(in)              :: default
        real(dp)                          :: x_

        if (present(x)) then;   x_ = x
        else;                   x_ = default
        end if

    end function optval_rdp
    elemental module function optval_rqp(x, default) result(x_)
        real(qp), intent(in), optional    :: x
        real(qp), intent(in)              :: default
        real(qp)                          :: x_

        if (present(x)) then;   x_ = x
        else;                   x_ = default
        end if

    end function optval_rqp
    elemental module function optval_ll1(x, default) result(x_)
        logical, intent(in), optional    :: x
        logical, intent(in)              :: default
        logical                          :: x_

        if (present(x)) then;   x_ = x
        else;                   x_ = default
        end if

    end function optval_ll1
    
end submodule mini_test_optval
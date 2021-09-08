!> This submodule draws on the `check` subroutine of `keurfonluu/Forlab`!
submodule(mini_test) mini_test_disp

    character(len=*), parameter :: fmt_r       = '(*(g12.4, 1x))'
    character(len=*), parameter :: fmt_c       = '(*(g25.0, 1x))'
    character(len=*), parameter :: format_     = 'g0.4'
    integer,          parameter :: brief_col   = 5
    integer,          parameter :: brief_row   = 5
    integer,          parameter :: default_col = 10
    integer,          parameter :: default_row = 50

contains

    module procedure disp_0_rsp

        if (present(header)) print *, header
        print fmt_r, value

    end procedure disp_0_rsp

    module procedure disp_1_rsp
        logical :: brief_
        integer :: m, col
        
        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'

        if (brief_ .and. m > col) then; print fmt_r, value(1:col-2), '...', value(m)
        else;                           print fmt_r, value(:)
        end if

    end procedure disp_1_rsp

    module procedure disp_2_rsp
        logical :: brief_
        integer :: i, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, row-2;    print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
                print fmt_r, colon(1:col)
                print fmt_r, value(m,1:col-2), '...', value(m,n)
            elseif (m > col .and. n <= row) then
                do i = 1, 3;        print fmt_r, value(i,:);                            end do
                print fmt_r, colon(1:n)
                print fmt_r, value(m,:)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_r, value(i,:);                            end do
        end if

    end procedure disp_2_rsp
    module procedure disp_0_rdp

        if (present(header)) print *, header
        print fmt_r, value

    end procedure disp_0_rdp

    module procedure disp_1_rdp
        logical :: brief_
        integer :: m, col
        
        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'

        if (brief_ .and. m > col) then; print fmt_r, value(1:col-2), '...', value(m)
        else;                           print fmt_r, value(:)
        end if

    end procedure disp_1_rdp

    module procedure disp_2_rdp
        logical :: brief_
        integer :: i, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, row-2;    print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
                print fmt_r, colon(1:col)
                print fmt_r, value(m,1:col-2), '...', value(m,n)
            elseif (m > col .and. n <= row) then
                do i = 1, 3;        print fmt_r, value(i,:);                            end do
                print fmt_r, colon(1:n)
                print fmt_r, value(m,:)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_r, value(i,:);                            end do
        end if

    end procedure disp_2_rdp
    module procedure disp_0_rqp

        if (present(header)) print *, header
        print fmt_r, value

    end procedure disp_0_rqp

    module procedure disp_1_rqp
        logical :: brief_
        integer :: m, col
        
        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'

        if (brief_ .and. m > col) then; print fmt_r, value(1:col-2), '...', value(m)
        else;                           print fmt_r, value(:)
        end if

    end procedure disp_1_rqp

    module procedure disp_2_rqp
        logical :: brief_
        integer :: i, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, row-2;    print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
                print fmt_r, colon(1:col)
                print fmt_r, value(m,1:col-2), '...', value(m,n)
            elseif (m > col .and. n <= row) then
                do i = 1, 3;        print fmt_r, value(i,:);                            end do
                print fmt_r, colon(1:n)
                print fmt_r, value(m,:)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_r, value(i,:);                            end do
        end if

    end procedure disp_2_rqp
    module procedure disp_0_iint8

        if (present(header)) print *, header
        print fmt_r, value

    end procedure disp_0_iint8

    module procedure disp_1_iint8
        logical :: brief_
        integer :: m, col
        
        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'

        if (brief_ .and. m > col) then; print fmt_r, value(1:col-2), '...', value(m)
        else;                           print fmt_r, value(:)
        end if

    end procedure disp_1_iint8

    module procedure disp_2_iint8
        logical :: brief_
        integer :: i, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, row-2;    print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
                print fmt_r, colon(1:col)
                print fmt_r, value(m,1:col-2), '...', value(m,n)
            elseif (m > col .and. n <= row) then
                do i = 1, 3;        print fmt_r, value(i,:);                            end do
                print fmt_r, colon(1:n)
                print fmt_r, value(m,:)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_r, value(i,:);                            end do
        end if

    end procedure disp_2_iint8
    module procedure disp_0_iint16

        if (present(header)) print *, header
        print fmt_r, value

    end procedure disp_0_iint16

    module procedure disp_1_iint16
        logical :: brief_
        integer :: m, col
        
        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'

        if (brief_ .and. m > col) then; print fmt_r, value(1:col-2), '...', value(m)
        else;                           print fmt_r, value(:)
        end if

    end procedure disp_1_iint16

    module procedure disp_2_iint16
        logical :: brief_
        integer :: i, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, row-2;    print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
                print fmt_r, colon(1:col)
                print fmt_r, value(m,1:col-2), '...', value(m,n)
            elseif (m > col .and. n <= row) then
                do i = 1, 3;        print fmt_r, value(i,:);                            end do
                print fmt_r, colon(1:n)
                print fmt_r, value(m,:)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_r, value(i,:);                            end do
        end if

    end procedure disp_2_iint16
    module procedure disp_0_iint32

        if (present(header)) print *, header
        print fmt_r, value

    end procedure disp_0_iint32

    module procedure disp_1_iint32
        logical :: brief_
        integer :: m, col
        
        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'

        if (brief_ .and. m > col) then; print fmt_r, value(1:col-2), '...', value(m)
        else;                           print fmt_r, value(:)
        end if

    end procedure disp_1_iint32

    module procedure disp_2_iint32
        logical :: brief_
        integer :: i, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, row-2;    print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
                print fmt_r, colon(1:col)
                print fmt_r, value(m,1:col-2), '...', value(m,n)
            elseif (m > col .and. n <= row) then
                do i = 1, 3;        print fmt_r, value(i,:);                            end do
                print fmt_r, colon(1:n)
                print fmt_r, value(m,:)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_r, value(i,:);                            end do
        end if

    end procedure disp_2_iint32
    module procedure disp_0_iint64

        if (present(header)) print *, header
        print fmt_r, value

    end procedure disp_0_iint64

    module procedure disp_1_iint64
        logical :: brief_
        integer :: m, col
        
        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'

        if (brief_ .and. m > col) then; print fmt_r, value(1:col-2), '...', value(m)
        else;                           print fmt_r, value(:)
        end if

    end procedure disp_1_iint64

    module procedure disp_2_iint64
        logical :: brief_
        integer :: i, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, row-2;    print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
                print fmt_r, colon(1:col)
                print fmt_r, value(m,1:col-2), '...', value(m,n)
            elseif (m > col .and. n <= row) then
                do i = 1, 3;        print fmt_r, value(i,:);                            end do
                print fmt_r, colon(1:n)
                print fmt_r, value(m,:)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_r, value(i,:);                            end do
        end if

    end procedure disp_2_iint64
    module procedure disp_0_llk

        if (present(header)) print *, header
        print fmt_r, value

    end procedure disp_0_llk

    module procedure disp_1_llk
        logical :: brief_
        integer :: m, col
        
        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'

        if (brief_ .and. m > col) then; print fmt_r, value(1:col-2), '...', value(m)
        else;                           print fmt_r, value(:)
        end if

    end procedure disp_1_llk

    module procedure disp_2_llk
        logical :: brief_
        integer :: i, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, row-2;    print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
                print fmt_r, colon(1:col)
                print fmt_r, value(m,1:col-2), '...', value(m,n)
            elseif (m > col .and. n <= row) then
                do i = 1, 3;        print fmt_r, value(i,:);                            end do
                print fmt_r, colon(1:n)
                print fmt_r, value(m,:)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_r, value(i,:);                            end do
        end if

    end procedure disp_2_llk
    module procedure disp_0_lc_bool

        if (present(header)) print *, header
        print fmt_r, value

    end procedure disp_0_lc_bool

    module procedure disp_1_lc_bool
        logical :: brief_
        integer :: m, col
        
        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'

        if (brief_ .and. m > col) then; print fmt_r, value(1:col-2), '...', value(m)
        else;                           print fmt_r, value(:)
        end if

    end procedure disp_1_lc_bool

    module procedure disp_2_lc_bool
        logical :: brief_
        integer :: i, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, row-2;    print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
                print fmt_r, colon(1:col)
                print fmt_r, value(m,1:col-2), '...', value(m,n)
            elseif (m > col .and. n <= row) then
                do i = 1, 3;        print fmt_r, value(i,:);                            end do
                print fmt_r, colon(1:n)
                print fmt_r, value(m,:)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_r, value(i,1:col-2), '...', value(i,n);   end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_r, value(i,:);                            end do
        end if

    end procedure disp_2_lc_bool

    module procedure disp_0_csp

        if (present(header)) print *, header
        print fmt_c, to_string(value, format_)

    end procedure disp_0_csp

    module procedure disp_1_csp
        logical :: brief_
        integer :: i, m, col

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'
        if (brief_ .and. m > col) then; print fmt_c, (to_string(value(i), format_), i=1, col-2), &
                                               '...', to_string(value(m), format_)
        else;                           print fmt_c, (to_string(value(i), format_), i=1, m)
        end if

    end procedure disp_1_csp

    module procedure disp_2_csp
        logical :: brief_
        integer :: i, j, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, col-2;    print fmt_c, (to_string(value(i,j), format_), j=1, col-2), &
                                           '...', to_string(value(i,n), format_);           end do
                print fmt_c, colon(1:col)
                print fmt_c, (to_string(value(m,j), format_), j=1, col-2), &
                       '...', to_string(value(m,n), format_)
            elseif (m > col .and. n <= row) then
                do i = 1, col-2;    print fmt_c, (to_string(value(i,j), format_), j=1, n);  end do
                print fmt_c, colon(1:n)
                print fmt_c, (to_string(value(m,j), format_), j=1, n)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_c, (to_string(value(m,j), format_), j=1, col-2), &
                                           '...', to_string(value(m,n), format_);           end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_c, (to_string(value(i,j), format_), j=1, n);  end do
        end if

    end procedure disp_2_csp
    module procedure disp_0_cdp

        if (present(header)) print *, header
        print fmt_c, to_string(value, format_)

    end procedure disp_0_cdp

    module procedure disp_1_cdp
        logical :: brief_
        integer :: i, m, col

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'
        if (brief_ .and. m > col) then; print fmt_c, (to_string(value(i), format_), i=1, col-2), &
                                               '...', to_string(value(m), format_)
        else;                           print fmt_c, (to_string(value(i), format_), i=1, m)
        end if

    end procedure disp_1_cdp

    module procedure disp_2_cdp
        logical :: brief_
        integer :: i, j, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, col-2;    print fmt_c, (to_string(value(i,j), format_), j=1, col-2), &
                                           '...', to_string(value(i,n), format_);           end do
                print fmt_c, colon(1:col)
                print fmt_c, (to_string(value(m,j), format_), j=1, col-2), &
                       '...', to_string(value(m,n), format_)
            elseif (m > col .and. n <= row) then
                do i = 1, col-2;    print fmt_c, (to_string(value(i,j), format_), j=1, n);  end do
                print fmt_c, colon(1:n)
                print fmt_c, (to_string(value(m,j), format_), j=1, n)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_c, (to_string(value(m,j), format_), j=1, col-2), &
                                           '...', to_string(value(m,n), format_);           end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_c, (to_string(value(i,j), format_), j=1, n);  end do
        end if

    end procedure disp_2_cdp
    module procedure disp_0_cqp

        if (present(header)) print *, header
        print fmt_c, to_string(value, format_)

    end procedure disp_0_cqp

    module procedure disp_1_cqp
        logical :: brief_
        integer :: i, m, col

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        m      = size(value, 1)

        if (present(header)) print *, header
        print *, '[vector size: ' // to_string(m) // ']'
        if (brief_ .and. m > col) then; print fmt_c, (to_string(value(i), format_), i=1, col-2), &
                                               '...', to_string(value(m), format_)
        else;                           print fmt_c, (to_string(value(i), format_), i=1, m)
        end if

    end procedure disp_1_cqp

    module procedure disp_2_cqp
        logical :: brief_
        integer :: i, j, m, n
        integer :: col, row
        character(len=1) :: colon(default_col)

        brief_ = optval(brief, .true.)
        col    = merge(brief_col, default_col, present(brief) .and. brief_)
        row    = merge(brief_row, default_row, present(brief) .and. brief_)
        m      = size(value, 1)
        n      = size(value, 2)

        if (present(header)) print *, header
        print *, '[matrix size: ' // to_string(m) // '×' // to_string(n) // ']'
        if (brief_ .and. (m > col .or. n > row)) then
            !> Brief Print.
            colon = ':'
            if (m > col .and. n > row) then
                do i = 1, col-2;    print fmt_c, (to_string(value(i,j), format_), j=1, col-2), &
                                           '...', to_string(value(i,n), format_);           end do
                print fmt_c, colon(1:col)
                print fmt_c, (to_string(value(m,j), format_), j=1, col-2), &
                       '...', to_string(value(m,n), format_)
            elseif (m > col .and. n <= row) then
                do i = 1, col-2;    print fmt_c, (to_string(value(i,j), format_), j=1, n);  end do
                print fmt_c, colon(1:n)
                print fmt_c, (to_string(value(m,j), format_), j=1, n)
            elseif (m <= col .and. n > row) then
                do i = 1, m;        print fmt_c, (to_string(value(m,j), format_), j=1, col-2), &
                                           '...', to_string(value(m,n), format_);           end do
            end if
        else
            !> Full Print.
            do i = 1, m;            print fmt_c, (to_string(value(i,j), format_), j=1, n);  end do
        end if

    end procedure disp_2_cqp

    module procedure disp_character
        character(len=:), allocatable :: value_

        value_ = optval(value, '')

        if (present(header)) print *, header
        print *, value_

    end procedure disp_character

end submodule mini_test_disp
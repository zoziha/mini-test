program demo_mini_test_disp

    use mini_test, only: disp
    real    :: x(3, 3) = 1
    complex :: c(3, 3) = 1

    call disp(.true.,  "`.true.`  failed.")
    call disp(.false., "`.false.` failed.")
    call disp(.false., "`.false.` failed.")

    call disp(x, "shows `x`: ")
    call disp(c, "shows `c`: ")

end program demo_mini_test_disp
program demo_mini_test_check

    use mini_test, only: check

    call check(.true.,  msg="`.true.`  failed.")
    call check(.false., msg="`.false.` failed.", warn=.true.)
    call check(.false., msg="`.false.` failed.")

end program demo_mini_test_check
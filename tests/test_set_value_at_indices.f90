    program test_set_value_at_indices

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmsstreamflow
    use fixtures, only: config_file, status, print_1darray, isReal4EqualReal4, &
        isReal8EqualReal8, print_i_1darray, print_array, isintEqualint, print_d_1darray

    implicit none

    type (bmi_prms_streamflow) :: m
    integer :: retcode

    !test r32 gwsink_coef
    retcode = test1()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if



    contains

    ! Test getting r32 hru_area.
    function test1() result(code)
    character (len=*), parameter :: &
        var_name = "gwres_flow"
    integer, parameter :: rank =1
    integer, parameter :: size = 7
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    integer, parameter, dimension(size) :: &
        indices = (/ 2, 4, 6, 8, 10, 12, 14 /)

    real, parameter, dimension(shape(1)) :: &
         expected = (/ 0.0025d0, 0.0025d0, 0.0025d0, &
            0.0025d0, 0.0025d0, 0.0025d0, 0.0025d0 /)
    real :: val(size), gval(size)
    integer :: i, code

    code = m%initialize(config_file)
    code = m%update()
    
    code = m%get_value_at_indices(var_name, val, indices)
    val = 0.0025d0
    code = m%set_value_at_indices(var_name, indices, val)
    code = m%get_value_at_indices(var_name, gval, indices)

    code = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
    write(*,*) "Expected"
    call print_1darray(expected, shape)
    write(*,*) "Set Value"
    call print_1darray(gval, shape)

    code = BMI_SUCCESS
    do i = 1, size
        if (gval(i).ne.expected(i)) then
            code = BMI_FAILURE
        end if
    end do
    end function test1

    end program test_set_value_at_indices

program test_get_value_at_indices

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmsstreamflow
    use fixtures, only: config_file, status, print_1darray, isReal4EqualReal4, &
        isReal8EqualReal8, print_i_1darray, print_array, isintEqualint, print_d_1darray

    implicit none

    type (bmi_prms_streamflow) :: m
    integer :: retcode

    !test r32 gwres_flow
    retcode = test1()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    !r64 by nhru gwres_stor_ante.
    retcode = test2()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if
        
    contains

! Test getting r32 hru_type.
function test1() result(status)
    character (len=*), parameter :: &
         var_name = "gwres_flow"
    integer, parameter :: rank = 1
    integer, parameter :: size = 7
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    integer, parameter, dimension(7) :: &
       indices = (/ 2, 4, 6, 8, 10, 12, 14 /)
    real, parameter, dimension(shape(1)) :: &
         expected = (/ 0.00000000, &
            0.00000000, 0.00000000, &
            0.00000000, 0.00000000, &
            0.00000000, 0.00000000 /)
    real :: tval(size)
    integer :: i, status
    
    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value_at_indices(var_name, tval, indices)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
        
    write(*,*) "get Value"
    call print_1darray(tval, shape)
        
    write(*,*) "Expected"
    call print_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test1
  
! Test getting r64 by nhru
    function test2() result(status)
    character (len=*), parameter :: &
         var_name = "seg_ssflow"
    integer, parameter :: rank =1
    integer, parameter :: size = 3
    integer, parameter, dimension(rank) :: shape = (/ 3 /)
    integer, parameter, dimension(size) :: &
       indices = (/ 2, 4, 6 /)
    double precision, parameter, dimension(shape(1)) :: &
         expected = (/ 0.00000000, &
            0.00000000, 0.00000000 /)
    double precision :: val(size)
    integer :: i, status
    double precision :: endtime

    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value_at_indices(var_name, val, indices)
    status = m%finalize()

    
    ! Visual inspection.
    write(*,*) "Test 2"
    
    write(*,*) "get Value"
    call print_d_1darray(val, shape)
    
    write(*,*) "Expected"
    call print_d_1darray(expected, shape)
    
    status = BMI_SUCCESS
    do i = 1, size
       if (val(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test2

end program test_get_value_at_indices

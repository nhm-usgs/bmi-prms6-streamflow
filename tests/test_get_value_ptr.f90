    program test_get_value_ptr

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
  
    ! Test getting r32 gwres_flow.
    function test1() result(status)
    character (len=*), parameter :: &
         var_name = "hru_outflow"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    double precision, parameter, dimension(shape(1)) :: &
         expected = (/ 0.000000000, 0.000000000, &
            0.000000000, 0.000000000, 0.000000000, 0.000000000, &
            0.000000000, 0.000000000, 0.000000000, 0.000000000, &
            0.000000000, 0.000000000, 0.000000000, 0.000000000 /)
    double precision, pointer :: tptr(:)
    integer :: i, status

    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value_ptr(var_name, tptr)
    
    ! Visual inspection.
    write(*,*) "Test 1"
    
    write(*,*) "Get Value Ptr"
    call print_d_1darray(tptr, shape)
    
    write(*,*) "Expected"
    call print_d_1darray(expected, shape)
    
    status = BMI_SUCCESS
    do i = 1, shape(1)
        if (tptr(i).ne.expected(i)) then
            status = BMI_FAILURE
            exit
        end if
    end do

    status = m%finalize()

    end function test1
  ! Test r64 by nhru.
  function test2() result(status)
    character (len=*), parameter :: &
         var_name = "seg_ssflow"
    integer, parameter :: rank = 1
    integer, parameter :: size = 7
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    double precision, parameter, dimension(shape(1)) :: &
         expected = (/ 0.000000000, 0.000000000, &
            0.000000000, 0.000000000, 0.000000000, 0.000000000, &
            0.000000000 /)
    double precision, pointer :: tptr(:)

    integer :: i, status
    
    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value_ptr(var_name, tptr)
    
    ! Visual inspection.
    write(*,*) "Test 2"
    
    write(*,*) "Get Value Ptr"
    call print_d_1darray(tptr, shape)
    
    write(*,*) "Expected"
    call print_d_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tptr(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
    status = m%finalize()
  end function test2

end program test_get_value_ptr

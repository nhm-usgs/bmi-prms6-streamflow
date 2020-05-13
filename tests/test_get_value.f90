program test_get_value

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

    !int by 1 has_gwstor_minarea
  retcode = test3()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

    !test nowtime i(6)
  retcode = test4()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

contains


  ! Test getting r32 gwres_flow.
  function test1() result(status)
    character (len=*), parameter :: &
         var_name = "flow_out"
    integer, parameter :: rank = 1
    integer, parameter :: size = 1
    integer, parameter, dimension(rank) :: shape = (/ 1 /)
    double precision, parameter, dimension(shape(1)) :: &
         expected = [0.0000000000d0] !expected = (/ 1.0 /)
    double precision :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    do i = 1,int(180)
        status = m%update()
        if(i == endtime) then
            status = m%get_value(var_name, tval)
        endif
    enddo
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
    
    write(*,*) "get Value"
    call print_d_1darray(tval, shape)
    
    write(*,*) "Expected"
    call print_d_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test1

  ! Test r64 by nhru.
  function test2() result(status)
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
    double precision :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    status = m%update()
    status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 2"
    
    write(*,*) "get Value"
    call print_d_1darray(tval, shape)
    
    write(*,*) "Expected"
    call print_d_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test2

  ! Test int by 1
  function test3() result(status)
    character (len=*), parameter :: &
         var_name = "seg_ssflow"
    integer, parameter :: rank = 1
    integer, parameter :: size = 7
    integer, parameter, dimension(rank) :: shape = (/7/)
    double precision, parameter, dimension(shape(1)) :: &
         expected = (/ 0.000000000, 0.000000000, &
            0.000000000, 0.000000000, 0.000000000, 0.000000000, &
            0.000000000 /)
    double precision :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    status = m%update()
    status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 3"

    write(*,*) "get Value"
    call print_d_1darray(tval, shape)
    
    write(*,*) "Expected"
    call print_d_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test3

  !test nowtime
  function test4() result(status)
    character (len=*), parameter :: &
         var_name = "nowtime"
    integer, parameter :: rank = 1
    integer, parameter :: size = 6
    integer, parameter, dimension(rank) :: shape = (/ 6 /)
    integer, parameter, dimension(shape(1)) :: &
         expected = (/ 2016, 1, 31, 0, 0, 0 /)
    integer :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    do i = 1,int(endtime)
        status = m%update()
        if(i == endtime) then
            status = m%get_value(var_name, tval)
        endif
    enddo
    !status = m%get_value(var_name, tval)
    status = m%finalize()
  
    ! Visual inspection.
    write(*,*) "Test 4"

    write(*,*) "get Value"
    call print_i_1darray(tval, shape)
    
    write(*,*) "Expected"
    call print_i_1darray(expected, shape)
  
    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test4
  
  end program test_get_value

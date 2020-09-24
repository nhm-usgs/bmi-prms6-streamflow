program test_get_output_item_count

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmsstreamflow
  use fixtures, only: status

  implicit none

  integer, parameter :: expected = 22
  type (bmi_prms_streamflow) :: m
  integer :: count

  status = m%get_output_item_count(count)
  
  if (count /= expected) then
     stop BMI_FAILURE
  end if
end program test_get_output_item_count

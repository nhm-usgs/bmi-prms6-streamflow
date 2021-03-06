program test_get_component_name

  use bmif_2_0, only: BMI_FAILURE, BMI_MAX_COMPONENT_NAME
  use bmiprmsstreamflow
  use fixtures, only: status

  implicit none

  character (len=BMI_MAX_COMPONENT_NAME), parameter :: &
       expected = "prms6-streamflow-BMI"
  type (bmi_prms_streamflow) :: m
  character (len=BMI_MAX_COMPONENT_NAME), pointer :: name

  status = m%get_component_name(name)

  if (name.ne.expected) then
     stop BMI_FAILURE
  end if
end program test_get_component_name

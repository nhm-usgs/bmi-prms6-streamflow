program test_get_grid_edge_nodes

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmsstreamflow
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: rank = 1
  integer, dimension(rank), parameter :: expected = [-1]

  type (bmi_prms_streamflow) :: m
  integer, dimension(rank) :: edge_nodes
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_edge_nodes(grid_id, edge_nodes)
  status = m%finalize()

  do i = 1, rank
     if (edge_nodes(i) /= expected(i)) then
        write(*,*) edge_nodes
        stop BMI_FAILURE
     end if
  end do
end program test_get_grid_edge_nodes

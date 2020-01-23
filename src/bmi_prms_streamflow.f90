    module bmiprmsstreamflow

    use m_prms_streamflow
    use bmif_2_0
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
    implicit none

    type, extends (bmi) :: bmi_prms_streamflow
        private
        type (prms_streamflow_model) :: model
    contains
    procedure :: get_component_name => prms_component_name
    procedure :: get_input_item_count => prms_input_item_count
    procedure :: get_output_item_count => prms_output_item_count
    procedure :: get_input_var_names => prms_input_var_names
    procedure :: get_output_var_names => prms_output_var_names
    procedure :: initialize => prms_initialize
    procedure :: finalize => prms_finalize
    procedure :: get_start_time => prms_start_time
    procedure :: get_end_time => prms_end_time
    procedure :: get_current_time => prms_current_time
    procedure :: get_time_step => prms_time_step
    procedure :: get_time_units => prms_time_units
    procedure :: update => prms_update
    procedure :: update_until => prms_update_until
    procedure :: get_var_grid => prms_var_grid
    procedure :: get_grid_type => prms_grid_type
    procedure :: get_grid_rank => prms_grid_rank
    !procedure :: get_grid_shape => prms_grid_shape
    procedure :: get_grid_size => prms_grid_size
    !procedure :: get_grid_spacing => prms_grid_spacing
    !procedure :: get_grid_origin => prms_grid_origin
    procedure :: get_grid_x => prms_grid_x
    procedure :: get_grid_y => prms_grid_y
    procedure :: get_grid_z => prms_grid_z
    procedure :: get_var_type => prms_var_type
    procedure :: get_var_units => prms_var_units
    procedure :: get_var_itemsize => prms_var_itemsize
    procedure :: get_var_nbytes => prms_var_nbytes
    procedure :: get_var_location => prms_var_location
    procedure :: get_value_int => prms_get_int
    procedure :: get_value_float => prms_get_float
    procedure :: get_value_double => prms_get_double
    generic :: get_value => &
         get_value_int, &
         get_value_float, &
         get_value_double
    procedure :: get_value_ptr_int => prms_get_ptr_int
    procedure :: get_value_ptr_float => prms_get_ptr_float
    procedure :: get_value_ptr_double => prms_get_ptr_double
    generic :: get_value_ptr => &
         get_value_ptr_int, &
         get_value_ptr_float, &
         get_value_ptr_double
    procedure :: get_value_at_indices_int => prms_get_at_indices_int
    procedure :: get_value_at_indices_float => prms_get_at_indices_float
    procedure :: get_value_at_indices_double => prms_get_at_indices_double
    generic :: get_value_at_indices => &
         get_value_at_indices_int, &
         get_value_at_indices_float, &
         get_value_at_indices_double
    procedure :: set_value_int => prms_set_int
    procedure :: set_value_float => prms_set_float
    procedure :: set_value_double => prms_set_double
    generic :: set_value => &
         set_value_int, &
         set_value_float, &
         set_value_double
    !procedure :: set_value_at_indices_int => prms_set_at_indices_int
    !procedure :: set_value_at_indices_float => prms_set_at_indices_float
    !procedure :: set_value_at_indices_double => prms_set_at_indices_double
    !generic :: set_value_at_indices => &
    !     set_value_at_indices_int, &
    !     set_value_at_indices_float, &
    !     set_value_at_indices_double
    !procedure :: print_model_info
    end type bmi_prms_streamflow

    private
    public :: bmi_prms_streamflow

    character (len=BMI_MAX_COMPONENT_NAME), target :: &
        component_name = "prms6-BMI"

    ! Exchange items
    integer, parameter :: input_item_count = 6
    integer, parameter :: output_item_count = 16
    character (len=BMI_MAX_VAR_NAME), target, &
        dimension(input_item_count) :: input_items = (/ &
            
    ! input vars from surface soil and groundwater required to run
    ! stream flow
    ! from potet
    'potet', &
    ! from solrad
    'swrad', &
    ! from groundwater
    'gwres_flow', &
    ! from soil
    'ssres_flow', &
    ! from runoff
    'sroff', &
    'strm_seg_in' &
    /)

    character (len=BMI_MAX_VAR_NAME), target, &
        dimension(output_item_count) :: output_items  = (/ &
        ! A list of potential ouputs needs more work once other streamflow 
        ! modules are added to prms6
    'flow_out', & !r64 by nhru
    'hru_outflow', & !r64 by nhru
    'seg_gwflow', & !r64 by nsegment
    'seg_sroff', & !r64 by nsegment
    'seg_ssflow', & !r64 by nsegment
    'seg_lateral_inflow', &  !r64 by nsegment
    'segment_order', & ! i32 by nsegment
    'segment_up', & !i32 by nsegment
    'seg_inflow', & !r64 by nsegment
    'seg_outflow', & !r64 by nsegment
    'seg_upstream_inflow', & !r64 by nsegment
    'seginc_gwflow', &  !r64 by nsegment
    'seginc_sroff', & !r64 by nsegment
    'seginc_ssflow', & !r64 by nsegment
    'seginc_swrad', & !r64 by nsegment
    'segment_delta_flow' & !r64 by nsegment
    /)
    


    contains

    ! Get the name of the model.
    function prms_component_name(this, name) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
    end function prms_component_name

    ! Count the input variables.
    function prms_input_item_count(this, count) result (bmi_status)
        class (bmi_prms_streamflow), intent(in) :: this
        integer, intent(out) :: count
        integer :: bmi_status

        count = input_item_count
        bmi_status = BMI_SUCCESS
     end function prms_input_item_count

    ! Count the output variables.
    function prms_output_item_count(this, count) result (bmi_status)
        class (bmi_prms_streamflow), intent(in) :: this
        integer, intent(out) :: count
        integer :: bmi_status

        count = output_item_count
        bmi_status = BMI_SUCCESS
    end function prms_output_item_count

    ! List input variables.
    function prms_input_var_names(this, names) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status
    names => input_items
    bmi_status = BMI_SUCCESS
    end function prms_input_var_names

    ! List output variables.
    function prms_output_var_names(this, names) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status
    ! vars by nhru                
    names => output_items
    bmi_status = BMI_SUCCESS
    end function prms_output_var_names

    ! BMI initializer.
    function prms_initialize(this, config_file) result (bmi_status)
    class (bmi_prms_streamflow), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    if (len(config_file) > 0) then
        call initialize_from_file(this%model, config_file)
    else
        !call initialize_from_defaults(this%model)
    end if
    bmi_status = BMI_SUCCESS
    end function prms_initialize

    ! BMI finalizer.
    function prms_finalize(this) result (bmi_status)
    class (bmi_prms_streamflow), intent(inout) :: this
    integer :: bmi_status

    call cleanup(this%model)
    bmi_status = BMI_SUCCESS
    end function prms_finalize

    ! Model start time.
    function prms_start_time(this, time) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = 0.d0
    !time = this%model%model_simulation%model_time%Timestep
    bmi_status = BMI_SUCCESS
    end function prms_start_time

    ! Model end time.
    function prms_end_time(this, time) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%model_simulation%model_time%Number_timesteps)
    bmi_status = BMI_SUCCESS
    end function prms_end_time

    ! Model current time.
    function prms_current_time(this, time) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%model_simulation%model_time%Timestep)
    bmi_status = BMI_SUCCESS
    end function prms_current_time

    ! Model time step.
    function prms_time_step(this, time_step) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(this%model%model_simulation%model_time%Timestep_seconds)
    bmi_status = BMI_SUCCESS
    end function prms_time_step

    ! Model time units.
    function prms_time_units(this, units) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
    end function prms_time_units

    ! Advance model by one time step.
    function prms_update(this) result (bmi_status)
    class (bmi_prms_streamflow), intent(inout) :: this
    integer :: bmi_status

    call advance_in_time(this%model)
    bmi_status = BMI_SUCCESS
    end function prms_update

    ! Advance the model until the given time.
    function prms_update_until(this, time) result (bmi_status)
    class (bmi_prms_streamflow), intent(inout) :: this
    double precision, intent(in) :: time
    double precision :: current_time, end_time, dt
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s
    s = this%get_current_time(current_time)
    s = this%get_end_time(end_time)
    s = this%get_time_step(dt)
    if (time > current_time) then
        n_steps_real = (time - current_time)
        n_steps = floor(n_steps_real)
        do i = 1, n_steps
            s = this%update()
        end do
        !s = this%update_frac(n_steps_real - dble(n_steps))
    end if
    bmi_status = BMI_SUCCESS
    end function prms_update_until

    ! Get the grid id for a particular variable.
    function prms_var_grid(this, name, grid) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: grid
    integer :: bmi_status

    select case(name)
    case('potet','swrad', 'gwres_flow', 'ssres_flow', 'sroff', &
        'hru_outflow')
        grid = 0
        bmi_status = BMI_SUCCESS
    case('strm_seg_in', 'seg_gwflow', 'seg_sroff', 'seg_ssflow', &
        'seg_lateral_inflow', 'seg_inflow', 'seg_outflow', &
        'seg_upstream_inflow', 'seginc_gwflow', 'seginc_sroff', &
        'seginc_ssflow', 'seginc_swrad', 'segment_delta_flow')
        grid = 1
        bmi_status = BMI_SUCCESS
    case('flow_out')
        grid = 2
    case default
        grid = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_grid

    ! The type of a variable's grid.
    function prms_grid_type(this, grid, type) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(0)
        type = "vector"
        bmi_status = BMI_SUCCESS
    case(1)
        type = "vector"
        bmi_status = BMI_SUCCESS
    case(2)
        type = 'scalar'
        bmi_status = BMI_SUCCESS
    case default
        type = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_type

    ! The number of dimensions of a grid.
    function prms_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status

    select case(grid)
    case(0)
        rank = 1
        bmi_status = BMI_SUCCESS
    case(1)
        rank = 1
        bmi_status = BMI_SUCCESS
    case(2)
        rank = 1
        bmi_status = BMI_SUCCESS
    case default
        rank = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_rank

    !! The dimensions of a grid.
    !function prms_grid_shape(this, type, grid_shape) result (bmi_status)
    !  class (bmi_prms_streamflow), intent(in) :: this
    !  integer, intent(in) :: type
    !  integer, dimension(:), intent(out) :: grid_shape
    !  integer :: bmi_status
    !
    !  select case(type)
    !  case(0)
    !     grid_shape = this%model%control_data%nhru%value
    !     bmi_status = BMI_SUCCESS
    !  case default
    !     grid_shape = [-1]
    !     bmi_status = BMI_FAILURE
    !  end select
    !end function prms_grid_shape
    !
    ! The total number of elements in a grid.
    function prms_grid_size(this, grid, size) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status

    select case(grid)
    case(0)
        size = this%model%model_simulation%model_basin%nhru
        bmi_status = BMI_SUCCESS
    case(1)
        size = this%model%model_simulation%model_basin%nsegment
        bmi_status = BMI_SUCCESS
    case(2)
        size = 1
        bmi_status = BMI_SUCCESS
    case default
        size = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_size

    !! The distance between nodes of a grid.
    !function prms_grid_spacing(this, type, grid_spacing) result (bmi_status)
    !  class (bmi_prms_streamflow), intent(in) :: this
    !  integer, intent(in) :: type
    !  real, dimension(:), intent(out) :: grid_spacing
    !  integer :: bmi_status
    !
    !  select case(type)
    !  case(0)
    !     grid_spacing = [this%model%dy, this%model%dx]
    !     bmi_status = BMI_SUCCESS
    !  case default
    !     grid_spacing = -1
    !     bmi_status = BMI_FAILURE
    !  end select
    !end function prms_grid_spacing
    !
    !! Coordinates of grid origin.
    !function prms_grid_origin(this, type, grid_origin) result (bmi_status)
    !  class (bmi_prms_streamflow), intent(in) :: this
    !  integer, intent(in) :: type
    !  real, dimension(:), intent(out) :: grid_origin
    !  integer :: bmi_status
    !
    !  select case(type)
    !  case(0)
    !     grid_origin = [0.0, 0.0]
    !     bmi_status = BMI_SUCCESS
    !  case default
    !     grid_origin = [-1.0]
    !     bmi_status = BMI_FAILURE
    !  end select
    !end function prms_grid_origin
    !
    ! X-coordinates of grid nodes.
    function prms_grid_x(this, grid, x) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    integer :: bmi_status

    select case(grid)
    case(0)
        x = this%model%model_simulation%model_basin%hru_x
        bmi_status = BMI_SUCCESS
    case default
        x = [-1.0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_x

    ! Y-coordinates of grid nodes.
    function prms_grid_y(this, grid, y) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    integer :: bmi_status

    select case(grid)
    case(0)
        y = this%model%model_simulation%model_basin%hru_y
        bmi_status = BMI_SUCCESS
    case default
        y = [-1.0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_y

    ! Z-coordinates of grid nodes.
    function prms_grid_z(this, grid, z) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status

    select case(grid)
    case(0)
        z = this%model%model_simulation%model_basin%hru_elev
        bmi_status = BMI_SUCCESS
    case default
        z = [-1.0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_z
    !
    ! The data type of the variable, as a string.
    function prms_var_type(this, name, type) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(name)
    case('potet','swrad', 'gwres_flow', 'ssres_flow', 'sroff')
        type = "real"
        bmi_status = BMI_SUCCESS
    case('flow_out', 'hru_outflow', 'seg_gwflow', 'seg_sroff', 'seg_ssflow', 'seg_lateral_inflow', &
            'seg_inflow','seg_outflow', 'seg_upstream_inflow', 'seginc_gwflow', 'seginc_sroff', &
            'seginc_ssflow', 'seginc_swrad', 'sement_delta_flow')
        type = "double"
        bmi_status = BMI_SUCCESS
    case('segment_order', 'segment_up')
        type='integer'
        bmi_status = BMI_SUCCESS
    case default
        type = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_type

    ! The units of the given variable.
    function prms_var_units(this, name, units) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status

    select case(name)
    case('potet', 'swrad', 'gwres_flow', 'ssres_flow', 'sroff', &
        'seg_gwflow', 'seg_sroff', 'seg_ssflow')
        units = "in"
        bmi_status = BMI_SUCCESS
    case('strm_seg_in', 'flow_out', &
        'hru_outflow', 'seg_lateral_inflow', 'seg_inflow', 'seg_outflow', 'seg_upstream_inflow', &
        'seginc_gwflow', 'seginc_sroff', 'seginc_ssflow', 'segment_delta_flow')
        units = "ft3 s-1"
        bmi_status = BMI_SUCCESS
    case('seginc')
        units = 'Ly'
        bmi_status = BMI_SUCCESS
    case default
        units = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_units

    ! Memory use per array element.
    function prms_var_itemsize(this, name, size) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: size
    integer :: bmi_status

    select case(name)
    !case("nlake")
    !    size = sizeof(this%model%model_simulation%model_basin%nlake)
    !    bmi_status = BMI_SUCCESS
    case('potet')
        size = sizeof(this%model%model_simulation%potet%potet)
        bmi_status = BMI_SUCCESS
    case('swrad')
        size = sizeof(this%model%model_simulation%solrad%swrad)
        bmi_status = BMI_SUCCESS
    case('gwres_flow')
        size = sizeof(this%model%model_simulation%groundwater%gwres_flow)
        bmi_status = BMI_SUCCESS
    case('ssres_flow')
        size = sizeof(this%model%model_simulation%soil%ssres_flow)
        bmi_status = BMI_SUCCESS
    case('sroff')
        size = sizeof(this%model%model_simulation%runoff%sroff)
        bmi_status = BMI_SUCCESS
    case('strm_seg_in')
        size = sizeof(this%model%model_simulation%runoff%strm_seg_in)
        bmi_status = BMI_SUCCESS
    case('hru_outflow')
        size = sizeof(this%model%model_simulation%model_streamflow%hru_outflow)
        bmi_status = BMI_SUCCESS
    case('seg_gwflow')
        size = sizeof(this%model%model_simulation%model_streamflow%seg_gwflow)
        bmi_status = BMI_SUCCESS
    case('seg_sroff')
        size = sizeof(this%model%model_simulation%model_streamflow%seg_sroff)
        bmi_status = BMI_SUCCESS
    case('seg_ssflow')
        size = sizeof(this%model%model_simulation%model_streamflow%seg_ssflow)
        bmi_status = BMI_SUCCESS
    case('seg_lateral_inflow')
        size = sizeof(this%model%model_simulation%model_streamflow%seg_lateral_inflow)
        bmi_status = BMI_SUCCESS
    case('flow_out')
        size = sizeof(this%model%model_simulation%model_streamflow%flow_out)
        bmi_status = BMI_SUCCESS
    case('segment_order')
        size = sizeof(this%model%model_simulation%model_streamflow%segment_order)
        bmi_status = BMI_SUCCESS
    case('segment_up')
        size = sizeof(this%model%model_simulation%model_streamflow%segment_up)
        bmi_status = BMI_SUCCESS
    case('seg_inflow')
        size = sizeof(this%model%model_simulation%model_streamflow%seg_inflow)
        bmi_status = BMI_SUCCESS
    case('seg_outflow')
        size = sizeof(this%model%model_simulation%model_streamflow%seg_outflow)
        bmi_status = BMI_SUCCESS
    case('seg_upstream_inflow')
        size = sizeof(this%model%model_simulation%model_streamflow%seg_upstream_inflow)
        bmi_status = BMI_SUCCESS
    case('seginc_gwflow')
        size = sizeof(this%model%model_simulation%model_streamflow%seginc_gwflow)
        bmi_status = BMI_SUCCESS
    case('seginc_sroff')
        size = sizeof(this%model%model_simulation%model_streamflow%seginc_sroff)
        bmi_status = BMI_SUCCESS
    case('seginc_ssflow')
        size = sizeof(this%model%model_simulation%model_streamflow%seginc_ssflow)
        bmi_status = BMI_SUCCESS
    case('seginc_swrad')
        size = sizeof(this%model%model_simulation%model_streamflow%seginc_swrad)
        bmi_status = BMI_SUCCESS
    case('segment_delta_flow' )
        size = sizeof(this%model%model_simulation%model_streamflow%segment_delta_flow)
        bmi_status = BMI_SUCCESS

    case default
        size = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_itemsize
    
    ! The size of the given variable.
    function prms_var_nbytes(this, name, nbytes) result (bmi_status)
      class (bmi_prms_streamflow), intent(in) :: this
      character (len=*), intent(in) :: name
      integer, intent(out) :: nbytes
      integer :: bmi_status
      integer :: s1, s2, s3, type, grid_size, item_size
    
      s1 = this%get_var_grid(name, type)
      s2 = this%get_grid_size(type, grid_size)
      s3 = this%get_var_itemsize(name, item_size)
    
      if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
         nbytes = item_size * grid_size
         bmi_status = BMI_SUCCESS
      else
         nbytes = -1
         bmi_status = BMI_FAILURE
      end if
    end function prms_var_nbytes
    
  ! The location (node, face, edge) of the given variable.
    function prms_var_location(this, name, location) result (bmi_status)
        class (bmi_prms_streamflow), intent(in) :: this
        character (len=*), intent(in) :: name
        character (len=*), intent(out) :: location
        integer :: bmi_status

        select case(name)
        case default
           location = "face"
           bmi_status = BMI_SUCCESS
        end select
    end function prms_var_location
    
    ! Get a copy of a integer variable's values, flattened.
    function prms_get_int(this, name, dest) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case('segment_order')
        dest = [this%model%model_simulation%model_streamflow%segment_order]
        bmi_status = BMI_SUCCESS
    case('segment_up')
        dest = [this%model%model_simulation%model_streamflow%segment_up]
        bmi_status = BMI_SUCCESS
    case default
        dest = [-1]
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_int
    
    ! Get a copy of a real variable's values, flattened.
    function prms_get_float(this, name, dest) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
        !case("plate_surface__temperature")
        !   ! This would be safe, but subject to indexing errors.
        !   ! do j = 1, this%model%n_y
        !   !    do i = 1, this%model%n_x
        !   !       k = j + this%model%n_y*(i-1)
        !   !       dest(k) = this%model%temperature(j,i)
        !   !    end do
        !   ! end do
        !
        !   ! This is an equivalent, elementwise copy into `dest`.
        !   ! See https://stackoverflow.com/a/11800068/1563298
        !   dest = reshape(this%model%temperature, [this%model%n_x*this%model%n_y])
        !   bmi_status = BMI_SUCCESS
        !case("plate_surface__thermal_diffusivity")
        !   dest = [this%model%alpha]
        !   bmi_status = BMI_SUCCESS
    !case('hru_ppt')
    !    dest = [this%model%model_simulation%model_precip%hru_ppt]
    !    bmi_status = BMI_SUCCESS

    case default
        dest = [-1.0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_float

    ! Get a copy of a double variable's values, flattened.
    function prms_get_double(this, name, dest) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)

    case('hru_outflow')
        dest = [this%model%model_simulation%model_streamflow%hru_outflow]
        bmi_status = BMI_SUCCESS
    case('seg_gwflow')
        dest = [this%model%model_simulation%model_streamflow%seg_gwflow]
        bmi_status = BMI_SUCCESS
    case('seg_sroff')
        dest = [this%model%model_simulation%model_streamflow%seg_sroff]
        bmi_status = BMI_SUCCESS
    case('seg_ssflow')
        dest = [this%model%model_simulation%model_streamflow%seg_ssflow]
        bmi_status = BMI_SUCCESS
    case('seg_lateral_inflow')
        dest = [this%model%model_simulation%model_streamflow%seg_lateral_inflow]
        bmi_status = BMI_SUCCESS
    case('flow_out')
        dest = [this%model%model_simulation%model_streamflow%flow_out]
        bmi_status = BMI_SUCCESS
    case('seg_inflow')
        dest = [this%model%model_simulation%model_streamflow%seg_inflow]
        bmi_status = BMI_SUCCESS
    case('seg_outflow')
        dest = [this%model%model_simulation%model_streamflow%seg_outflow]
        bmi_status = BMI_SUCCESS
    case('seg_upstream_inflow')
        dest = [this%model%model_simulation%model_streamflow%seg_upstream_inflow]
        bmi_status = BMI_SUCCESS
    case('seginc_gwflow')
        dest = [this%model%model_simulation%model_streamflow%seginc_gwflow]
        bmi_status = BMI_SUCCESS
    case('seginc_sroff')
        dest = [this%model%model_simulation%model_streamflow%seginc_sroff]
        bmi_status = BMI_SUCCESS
    case('seginc_ssflow')
        dest = [this%model%model_simulation%model_streamflow%seginc_ssflow]
        bmi_status = BMI_SUCCESS
    case('seginc_swrad')
        dest = [this%model%model_simulation%model_streamflow%seginc_swrad]
        bmi_status = BMI_SUCCESS
    case('segment_delta_flow') 
        dest = [this%model%model_simulation%model_streamflow%segment_delta_flow]
        bmi_status = BMI_SUCCESS

    case default
        dest = [-1.d0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_double

    ! Get a reference to an integer-valued variable, flattened.
    function prms_get_ptr_int(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status, status
    type (c_ptr) :: src
    integer :: n_elements, gridid

    select case(name)
    !case("nlake")
    !    src = c_loc(this%model%model_simulation%model_basin%nlake)
    !    status = this%get_var_grid(name,gridid)
    !    status = this%get_grid_size(gridid, n_elements)
    !    call c_f_pointer(src, dest_ptr, [n_elements])
    !    bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE
    end select

    end function prms_get_ptr_int

    ! Get a reference to a real-valued variable, flattened.
    function prms_get_ptr_float(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements, gridid, status

    select case(name)
        !case("plate_surface__temperature")
        !   src = c_loc(this%model%temperature(1,1))
        !   n_elements = this%model%n_y * this%model%n_x
        !   call c_f_pointer(src, dest, [n_elements])
        !   bmi_status = BMI_SUCCESS
    !case('hru_ppt')
    !    src = c_loc(this%model%model_simulation%model_precip%hru_ppt(1))
    !    status = this%get_var_grid(name,gridid)
    !    status = this%get_grid_size(gridid, n_elements)
    !    call c_f_pointer(src, dest_ptr, [n_elements])
    !    bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_ptr_float

    ! Get a reference to an double-valued variable, flattened.
    function prms_get_ptr_double(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements, status, gridid
        
    status = this%get_var_grid(name,gridid)
        
    status = this%get_grid_size(gridid, n_elements)

    select case(name)
    case('hru_outflow')
        src = c_loc(this%model%model_simulation%model_streamflow%hru_outflow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('seg_gwflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_gwflow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('seg_sroff')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_sroff(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('seg_ssflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_ssflow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('seg_lateral_inflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_lateral_inflow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('seg_inflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_inflow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('seg_outflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_outflow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('seg_upstream_inflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_upstream_inflow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('seginc_gwflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seginc_gwflow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('seginc_sroff')
        src = c_loc(this%model%model_simulation%model_streamflow%seginc_sroff(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('seginc_ssflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seginc_ssflow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('seginc_swrad')
        src = c_loc(this%model%model_simulation%model_streamflow%seginc_swrad(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('segment_delta_flow') 
        src = c_loc(this%model%model_simulation%model_streamflow%segment_delta_flow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_ptr_double

    ! Get values of an integer variable at the given locations.
    function prms_get_at_indices_int(this, name, dest, inds) &
        result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    integer, pointer :: src_flattened(:)
    integer :: i, n_elements, status, gridid

    select case(name)
    !case('cov_type')
    !    src = c_loc(this%model%model_simulation%model_basin%cov_type(1))
    !    status = this%get_var_grid(name,gridid)
    !    status = this%get_grid_size(gridid, n_elements)
    !    call c_f_pointer(src, src_flattened, [n_elements])
    !    do i = 1,  size(inds)
    !        dest(i) = src_flattened(inds(i))
    !    end do
    !    bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_at_indices_int

    ! Get values of a real variable at the given locations.
    function prms_get_at_indices_float(this, name, dest, inds) &
        result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    real, pointer :: src_flattened(:)
    integer :: i, n_elements, status, gridid

    select case(name)
        !case("plate_surface__temperature")
        !   src = c_loc(this%model%temperature(1,1))
        !   call c_f_pointer(src, src_flattened, [this%model%n_y * this%model%n_x])
        !   n_elements = size(indices)
        !   do i = 1, n_elements
        !      dest(i) = src_flattened(indices(i))
        !   end do
        !   bmi_status = BMI_SUCCESS
    !case('hru_ppt')
    !    src = c_loc(this%model%model_simulation%model_precip%hru_ppt(1))
    !    status = this%get_var_grid(name,gridid)
    !    status = this%get_grid_size(gridid, n_elements)
    !    call c_f_pointer(src, src_flattened, [n_elements])
    !    do i = 1,  size(inds)
    !        dest(i) = src_flattened(inds(i))
    !    end do
    !    bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_at_indices_float

    ! Get values of a double variable at the given locations.
    function prms_get_at_indices_double(this, name, dest, inds) &
        result (bmi_status)
    class (bmi_prms_streamflow), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    double precision, pointer :: src_flattened(:)
    integer :: i, n_elements, status, gridid
        
    status = this%get_var_grid(name,gridid)
    status = this%get_grid_size(gridid, n_elements)

    select case(name)
    case('hru_outflow')
        src = c_loc(this%model%model_simulation%model_streamflow%hru_outflow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('seg_gwflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_gwflow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('seg_sroff')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_sroff(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('seg_ssflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_ssflow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('seg_lateral_inflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_lateral_inflow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('seg_inflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_inflow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('seg_outflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_outflow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('seg_upstream_inflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seg_upstream_inflow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('seginc_gwflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seginc_gwflow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('seginc_sroff')
        src = c_loc(this%model%model_simulation%model_streamflow%seginc_sroff(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('seginc_ssflow')
        src = c_loc(this%model%model_simulation%model_streamflow%seginc_ssflow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('seginc_swrad')
        src = c_loc(this%model%model_simulation%model_streamflow%seginc_swrad(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('segment_delta_flow') 
        src = c_loc(this%model%model_simulation%model_streamflow%segment_delta_flow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_at_indices_double

    ! Set new integer values.
    function prms_set_int(this, name, src) result (bmi_status)
    class (bmi_prms_streamflow), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
      !case('srunoff_updated_soil')
      !    this%model%model_simulation%runoff%srunoff_updated_soil = src(1)
      !    bmi_status = BMI_SUCCESS
        case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_set_int

    ! Set new real values.
    function prms_set_float(this, name, src) result (bmi_status)
    class (bmi_prms_streamflow), intent(inout) :: this
    character (len=*), intent(in) :: name
    real, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    !case('dprst_evap_hru')
    !    this%model%model_simulation%runoff%dprst_evap_hru = src
    !    bmi_status = BMI_SUCCESS
    case('potet')
        this%model%model_simulation%potet%potet = src
        bmi_status = BMI_SUCCESS
    case('swrad')
        this%model%model_simulation%solrad%swrad = src
        bmi_status = BMI_SUCCESS
    case('gwres_flow')
        this%model%model_simulation%groundwater%gwres_flow = src
        bmi_status = BMI_SUCCESS
    case('ssres_flow')
        this%model%model_simulation%soil%ssres_flow = src
        bmi_status = BMI_SUCCESS
    case('sroff')
        this%model%model_simulation%runoff%sroff = src
        bmi_status = BMI_SUCCESS

    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_set_float

    ! Set new double values.
    function prms_set_double(this, name, src) result (bmi_status)
    class (bmi_prms_streamflow), intent(inout) :: this
    character (len=*), intent(in) :: name
    double precision, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case('strm_seg_in')
        this%model%model_simulation%runoff%strm_seg_in = src
        bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_set_double
    !
    ! Set integer values at particular locations.
    !function prms_set_at_indices_int(this, name, indices, src) &
    !     result (bmi_status)
    !  class (bmi_prms_streamflow), intent(inout) :: this
    !  character (len=*), intent(in) :: name
    !  integer, intent(in) :: indices(:)
    !  integer, intent(in) :: src(:)
    !  integer :: bmi_status
    !  type (c_ptr) dest
    !  integer, pointer :: dest_flattened(:)
    !  integer :: i
    !
    !  select case(name)
    !  case default
    !     bmi_status = BMI_FAILURE
    !  end select
    !end function prms_set_at_indices_int
    !
    !! Set real values at particular locations.
    !function prms_set_at_indices_float(this, name, indices, src) &
    !     result (bmi_status)
    !  class (bmi_prms_streamflow), intent(inout) :: this
    !  character (len=*), intent(in) :: name
    !  integer, intent(in) :: indices(:)
    !  real, intent(in) :: src(:)
    !  integer :: bmi_status
    !  type (c_ptr) dest
    !  real, pointer :: dest_flattened(:)
    !  integer :: i
    !
    !  select case(name)
    !  case("potet")
    !     dest = c_loc(this%model%model_simulation%potet%potet(1))
    !     call c_f_pointer(dest, dest_flattened, [this%model%model_simulation%model_basin%nhru])
    !     do i = 1, size(indices)
    !        dest_flattened(indices(i)) = src(i)
    !     end do
    !     bmi_status = BMI_SUCCESS
    !  case default
    !     bmi_status = BMI_FAILURE
    !  end select
    !end function prms_set_at_indices_float
    !
    !! Set double values at particular locations.
    !function prms_set_at_indices_double(this, name, indices, src) &
    !     result (bmi_status)
    !  class (bmi_prms_streamflow), intent(inout) :: this
    !  character (len=*), intent(in) :: name
    !  integer, intent(in) :: indices(:)
    !  double precision, intent(in) :: src(:)
    !  integer :: bmi_status
    !  type (c_ptr) dest
    !  double precision, pointer :: dest_flattened(:)
    !  integer :: i
    !
    !  select case(name)
    !  case default
    !     bmi_status = BMI_FAILURE
    !  end select
    !end function prms_set_at_indices_double
    
    ! A non-BMI procedure for model introspection.
    !subroutine print_model_info(this)
    !  class (bmi_prms_streamflow), intent(in) :: this
    !
    !  call print_info(this%model)
    !end subroutine print_model_info

    end module bmiprmsstreamflow

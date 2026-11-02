module wpsim_field_mod
  use wpsim_constants
  use wpsim_projection

  implicit none
  private
  public :: wpsim_field, wpsim_record, wpsim_record_link

  type :: wpsim_field
    integer                        :: version
    integer                        :: nx
    integer                        :: ny
    real                           :: xfcst
    real                           :: xlvl
    character(len=9)               :: field
    character(len=24)              :: hdate
    character(len=25)              :: units
    character(len=32)              :: map_source
    character(len=46)              :: desc
    real, allocatable              :: slab(:, :)
  end type wpsim_field

  type :: wpsim_record
    class(base_proj), allocatable  :: proj
    type(wpsim_field)              :: field
  contains
    procedure, public, pass(self)  :: init  => init_record
    procedure, public, pass(self)  :: read  => read_record
    procedure, public, pass(self)  :: write => write_record
    generic, public                :: write(formatted) => write_formatted
    procedure, private, pass(self) :: write_formatted
  end type wpsim_record

  type wpsim_record_link
    type(wpsim_record)               :: record
    type(wpsim_record_link), pointer :: next
  end type wpsim_record_link

  interface wpsim_field
    module procedure constructor
  end interface wpsim_field

contains

  function constructor(field, hdate, slab, xlvl, units, map_source, desc) result(self)

    implicit none
    character(len=*), intent(in)           :: field
    character(len=*), intent(in)           :: hdate
    real, intent(in)                       :: slab(:, :)
    real, intent(in), optional             :: xlvl
    character(len=*), intent(in), optional :: units
    character(len=*), intent(in), optional :: map_source
    character(len=*), intent(in), optional :: desc

    integer                                :: nx
    integer                                :: ny
    type(wpsim_field)                      :: self

    nx = size(slab, 1)
    ny = size(slab, 2)

    self%version = VERSION
    self%nx      = nx
    self%ny      = ny
    self%xfcst   = 0.0
    self%field   = field
    self%hdate   = hdate

    if (present(map_source)) then
      self%map_source = map_source
    else
      self%map_source = "WPSIM_PROCESSOR"
    end if

    if (present(xlvl) .and. present(units) .and. present(desc)) then
      self%xlvl = xlvl
      self%units = units
      self%desc  = desc
    else
      if (trim(field) == "PSFC") then
        self%units = "Pa"
        self%desc  = "Surface pressure"
        self%xlvl  = 200100
      else if (trim(field) == "PMSL") then
        self%units = "Pa"
        self%desc  = "Mean sea-level pressure"
        self%xlvl  = 201300
      else if (trim(field) == "SKINTEMP") then
        self%units = "K"
        self%desc  = "Skin temperature"
        self%xlvl  = 200100
      else if (trim(field) == "SOILHGT") then
        self%units = "m"
        self%desc  = "Soil height"
        self%xlvl  = 200100
      else if (trim(field) == "TT") then
        self%units = "K"
        self%desc  = "2-meter air temperature"
        self%xlvl  = 200100
      else if (trim(field) == "RH") then
        self%units = "%"
        self%desc  = "2-meter relative humidity"
        self%xlvl  = 200100
      else if (trim(field) == "SPECHUMD") then
        self%units = "kg kg-1"
        self%desc  = "2-meter specific humidity"
        self%xlvl  = 200100
      else if (trim(field) == "UU") then
        self%units = "m s-1"
        self%desc  = "10-meter wind u-component"
        self%xlvl  = 200100
      else if (trim(field) == "VV") then
        self%units = "m s-1"
        self%desc  = "10-meter wind v-component"
        self%xlvl  = 200100
      else if (trim(field) == "LANDSEA") then
        self%units = "fraction"
        self%desc  = "Land sea mask"
        self%xlvl  = 200100
      else if (trim(field) == "SST") then
        self%units = "K"
        self%desc  = "Sea surface temperature"
        self%xlvl  = 201300
      else if (trim(field) == "SEAICE") then
        self%units = "fraction"
        self%desc  = "Sea-ice-fraction"
        self%xlvl  = 200100
      else if (trim(field) == "SNOW") then
        self%units = "kg m-2"
        self%desc  = "Water equivalent of Accum snow depth"
        self%xlvl  = 200100
      else if (trim(field) == "TAVGSFC") then
        self%units = "K"
        self%desc  = "Daily mean of surface air temperature"
        self%xlvl  = 200100
      end if
    end if

  end function constructor

  subroutine init_record(self, proj, field)

    implicit none
    class(wpsim_record), intent(inout) :: self
    class(base_proj), intent(in)       :: proj
    type(wpsim_field), intent(in)      :: field

    self%proj  = proj
    self%field = field

  end subroutine init_record

  subroutine read_record(self, iunit, iostat)

    implicit none
    class(wpsim_record), intent(inout) :: self
    integer, intent(in)                :: iunit
    integer, intent(out)               :: iostat

    class(base_proj), allocatable       :: proj
    type(wpsim_field)                   :: field
    type(proj_params)                   :: params

    read(unit=iunit, iostat=iostat) field%version
    if (iostat /= 0) then
      write(*, *) 'ERROR: can not read format version from WPS intermediate format file!'
      return
    end if

    if (field%version /= VERSION) then
      write(*, *) 'ERROR: unsupported WPS intermediate format version ', field%version, '!'
      iostat = 1
      return
    end if

    read(iunit, iostat=iostat) field%hdate, field%xfcst, field%map_source, field%field,     &
                               field%units, field%desc, field%xlvl, field%nx, field%ny,     &
                               params%iproj
    if (iostat /= 0) then
      write(*, *) 'ERROR: can not read metadata part 1 from WPS intermediate format file!'
      return
    end if

    if (params%iproj == IPROJ_LATLON) then ! Cylindrical equidistant (lat/lon)
      allocate(latlon_proj :: proj)
      read(iunit, iostat=iostat) params%startloc, params%startlat, params%startlon,         &
                                 params%deltalat, params%deltalon, params%earth_radius
    else if (params%iproj == IPROJ_MERCATOR) then ! Mercator
      allocate(mercator_proj :: proj)
      read(iunit, iostat=iostat) params%startloc, params%startlat, params%startlon,         &
                                 params%dx, params%dy, params%truelat1, params%earth_radius
    else if (params%iproj == IPROJ_LAMBERT_CONFORMAL) then ! Lambert conformal
      allocate(lambert_proj :: proj)
      read(iunit, iostat=iostat) params%startloc, params%startlat, params%startlon,         &
                                 params%dx, params%dy, params%xlonc, params%truelat1,       &
                                 params%truelat2, params%earth_radius
    else if (params%iproj == IPROJ_GAUSSIAN) then ! Gaussian
      allocate(gaussian_proj :: proj)
      read(iunit, iostat=iostat) params%startloc, params%startlat, params%startlon,         &
                                 params%nlats, params%deltalon, params%earth_radius
    else if (params%iproj == IPROJ_POLAR_STEREOGRAPHIC) then ! Polar stereographic
      allocate(stereographic_proj :: proj)
      read(iunit, iostat=iostat) params%startloc, params%startlat, params%startlon,         &
                                 params%xlonc, params%truelat1, params%earth_radius
    end if
    if (iostat /= 0) then
      write(*, *) 'ERROR: can not read metadata part 2 from WPS intermediate format file!'
      return
    end if

    read(iunit, iostat=iostat) params%is_wind_grid_rel
    if (iostat /= 0) then
      write(*, *) 'ERROR: can not read wind rotation flag from WPS intermediate format file!'
      return
    end if

    call proj%init(params)

    if (.not. allocated(field%slab)) then
      allocate(field%slab(field%nx, field%ny))
    end if

    ! Read 2-d array of data
    read(iunit, iostat=iostat) field%slab
    if (iostat /= 0) then
      write(*, *) 'ERROR: can not read slab data from WPS intermediate format file!'
      return
    end if

    call self%init(proj, field)

  end subroutine read_record

  subroutine write_record(self, iunit, iostat)

    implicit none
    class(wpsim_record), intent(in)   :: self
    integer, intent(in)               :: iunit
    integer, intent(out)              :: iostat

    class(base_proj), allocatable     :: proj
    type(wpsim_field)                 :: field

    proj  = self%proj
    field = self%field

    write(unit=iunit, iostat=iostat) field%version
    if (iostat /= 0) then
      write(*, *) 'ERROR: can not write format version to WPS intermediate format file!'
      return
    endif

    write(iunit, iostat=iostat) field%hdate, field%xfcst, field%map_source, field%field, &
                                field%units, field%desc, field%xlvl, field%nx, field%ny, &
                                proj%iproj
    if (iostat /= 0) then
      write(*, *) 'ERROR: can not write metadata part 1 to WPS intermediate format file!'
      return
    end if

    select type (proj)
    type is (latlon_proj) ! Cylindrical equidistant (lat/lon)
      write(iunit, iostat=iostat) proj%startloc, proj%startlat, proj%startlon,       &
                                  proj%deltalat, proj%deltalon, proj%earth_radius
    type is (mercator_proj) ! Mercator
      write(iunit, iostat=iostat) proj%startloc, proj%startlat, proj%startlon,       &
                                  proj%dx, proj%dy, proj%truelat1, proj%earth_radius
    type is (lambert_proj) ! Lambert conformal
      write(iunit, iostat=iostat) proj%startloc, proj%startlat, proj%startlon,       &
                                  proj%dx, proj%dy, proj%xlonc, proj%truelat1,       &
                                  proj%truelat2, proj%earth_radius
    type is (gaussian_proj) ! Gaussian
      write(iunit, iostat=iostat) proj%startloc, proj%startlat, proj%startlon,       &
                                  proj%nlats, proj%deltalon, proj%earth_radius
    type is (stereographic_proj) ! Polar stereographic
      write(iunit, iostat=iostat) proj%startloc, proj%startlat, proj%startlon,       &
                                  proj%xlonc, proj%truelat1, proj%earth_radius
    end select
    if (iostat /= 0) then
      write(*, *) 'ERROR: can not write metadata part 2 to WPS intermediate format file!'
      return
    end if

    write(iunit, iostat=iostat) proj%is_wind_grid_rel
    if (iostat /= 0) then
      write(*, *) 'ERROR: can not write wind rotation flag to WPS intermediate format file!'
      return
    end if

    ! Write 2-d array of data
    write(iunit, iostat=iostat) field%slab
    if (iostat /= 0) then
      write(*, *) 'ERROR: can not write slab data to WPS intermediate format file!'
      return
    end if

  end subroutine write_record

  subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)

    implicit none
    class(wpsim_record), intent(in) :: self      !< The string.
    integer, intent(in)             :: unit      !< Logical unit.
    character(len=*), intent(in)    :: iotype    !< Edit descriptor.
    integer, intent(in)             :: v_list(:) !< Edit descriptor list.
    integer, intent(out)            :: iostat    !< IO status code.
    character(len=*), intent(inout) :: iomsg     !< IO status message.

    class(base_proj), allocatable   :: proj
    type(wpsim_field)               :: field

    proj  = self%proj
    field = self%field

    write(unit, *, iostat=iostat) '================================================', new_line('A')
    write(unit, *, iostat=iostat) ' FIELD = ', trim(field%field), new_line('A')
    write(unit, *, iostat=iostat) ' UNITS = ', trim(field%units), ' DESCRIPTION = ', trim(field%desc), new_line('A')
    write(unit, *, iostat=iostat) ' DATE = ', trim(field%hdate), ' FCST = ', field%xfcst, new_line('A')
    write(unit, *, iostat=iostat) ' SOURCE = ', trim(field%map_source), new_line('A')
    write(unit, *, iostat=iostat) ' LEVEL = ', field%xlvl, new_line('A')
    write(unit, *, iostat=iostat) ' I,J DIMS = ', field%nx, field%ny, new_line('A')
    select type (proj)
    type is (latlon_proj) ! Cylindrical equidistant (lat/lon)
      write(unit, *, iostat=iostat) ' IPROJ = ', proj%iproj, ' PROJECTION = LAT LON', new_line('A')
      write(unit, *, iostat=iostat) '   REF_X, REF_Y = ', 1.0, 1.0, new_line('A')
      write(unit, *, iostat=iostat) '   REF_LAT, REF_LON = ', proj%startlat, proj%startlon, new_line('A')
      write(unit, *, iostat=iostat) '   DLAT, DLON = ', proj%deltalat, proj%deltalon, new_line('A')
    type is (mercator_proj) ! Mercator
      write(unit, *, iostat=iostat) ' IPROJ = ', proj%iproj, ' PROJECTION = MERCATOR', new_line('A')
      write(unit, *, iostat=iostat) '   REF_X, REF_Y = ', 1.0, 1.0, new_line('A')
      write(unit, *, iostat=iostat) '   REF_LAT, REF_LON = ', proj%startlat, proj%startlon, new_line('A')
      write(unit, *, iostat=iostat) '   DX, DY = ', proj%dx, proj%dy, new_line('A')
      write(unit, *, iostat=iostat) '   TRUELAT1 = ', proj%truelat1, new_line('A')
    type is (lambert_proj) ! Lambert conformal
      write(unit, *, iostat=iostat) ' IPROJ = ', proj%iproj, ' PROJECTION = LAMBERT', new_line('A')
      write(unit, *, iostat=iostat) '   REF_X, REF_Y = ', 1.0, 1.0, new_line('A')
      write(unit, *, iostat=iostat) '   REF_LAT, REF_LON = ', proj%startlat, proj%startlon, new_line('A')
      write(unit, *, iostat=iostat) '   DX, DY = ', proj%dx, proj%dy, new_line('A')
      write(unit, *, iostat=iostat) '   XLONC = ', proj%xlonc, new_line('A')
      write(unit, *, iostat=iostat) '   TRUELAT1 = ', proj%truelat1, new_line('A')
      write(unit, *, iostat=iostat) '   TRUELAT2 = ', proj%truelat2, new_line('A')
    type is (gaussian_proj) ! Gaussian
      write(unit, *, iostat=iostat) ' IPROJ = ', proj%iproj, ' PROJECTION = GAUSSIAN', new_line('A')
      write(unit, *, iostat=iostat) '   REF_X, REF_Y = ', 1.0, 1.0, new_line('A')
      write(unit, *, iostat=iostat) '   REF_LAT, REF_LON = ', proj%startlat, proj%startlon, new_line('A')
      write(unit, *, iostat=iostat) '   NLATS = ', proj%nlats, new_line('A')
    type is (stereographic_proj) ! Polar stereographic
      write(unit, *, iostat=iostat) ' IPROJ = ', proj%iproj, ' PROJECTION = POLAR STEREOGRAPHIC', new_line('A')
      write(unit, *, iostat=iostat) '   REF_X, REF_Y = ', 1.0, 1.0, new_line('A')
      write(unit, *, iostat=iostat) '   REF_LAT, REF_LON = ', proj%startlat, proj%startlon, new_line('A')
      write(unit, *, iostat=iostat) '   XLONC = ', proj%xlonc, new_line('A')
      write(unit, *, iostat=iostat) '   TRUELAT1 = ', proj%truelat1, new_line('A')
    end select
    write(unit, *, iostat=iostat) '   EARTH_RADIUS = ', proj%earth_radius, new_line('A')
    write(unit, *, iostat=iostat) ' DATA(1,1) = ', field%slab(1, 1), new_line('A')

  end subroutine write_formatted

end module wpsim_field_mod
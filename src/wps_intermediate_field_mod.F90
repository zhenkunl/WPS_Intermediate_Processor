module wps_intermediate_field_mod

  implicit none
  private
  public :: wps_intermediate_field_t

  integer, parameter          :: IPROJ_LATLON = 0
  integer, parameter          :: IPROJ_MERCATOR = 1
  integer, parameter          :: IPROJ_LAMBERT_CONFORMAL = 3
  integer, parameter          :: IPROJ_GAUSSIAN = 4
  integer, parameter          :: IPROJ_POLAR_STEREOGRAPHIC = 5
  character(len=8), parameter :: SWCORNER = 'SWCORNER'
  character(len=8), parameter :: CENTER =   'CENTER  '
  integer, parameter          :: VERSION = 5
  real, parameter             :: EARTH_RADIUS = 6371.229 ! km
  real, parameter             :: UNDEF = 999.0

  type :: wps_var_t
    private
    character(len=9)  :: field            ! Name of the field
    character(len=24) :: hdate            ! Valid data for data YYYY:MM:DD_HH:mm:ss
    character(len=25) :: units            ! Units of data
    character(len=32) :: map_source       ! Source model / originating center
    character(len=46) :: desc             ! Short description of data
  end type wps_var_t

  type :: wps_intermediate_field_t
    private
    integer           :: version          ! Format version (must =5 for WPS format)
    integer           :: nx               ! x-dimension for 2-d array
    integer           :: ny               ! y-dimension for 2-d array
    integer           :: iproj            ! Code for projection of data in array
                                          ! = 0, cylindrical equidistant (lat/lon)
                                          ! = 1, Mercator
                                          ! = 3, Lambert conformal conic
                                          ! = 4, Gaussian (global only)
                                          ! = 5, Polar stereographic
    real              :: nlats            ! Number of latitudes north of equator
                                          ! (for Gaussian grids)
    real              :: xfcst            ! Forecast hour of data
    real              :: xlvl             ! Vertical level of data in 2-d array
    real              :: startlat         ! Latitude of point in array indicated by startloc
    real              :: startlon         ! Longitude of point in array indicated by startloc
    real              :: deltalat         ! Grid spacing, degrees
    real              :: deltalon         ! Grid spacing, degrees
    real              :: dx               ! Grid spacing, km
    real              :: dy               ! Grid spacing, km
    real              :: xlonc            ! Standard longitude of projection
    real              :: truelat1         ! True latitude 1 of projection
    real              :: truelat2         ! True latitude 2 of projection
    real              :: earth_radius     ! Earth radius, km
    real, allocatable :: slab(:, :)       ! 2-d array holding the data
    logical           :: is_wind_grid_rel ! Flag indicating whether winds are relative
                                          ! to source grid (TRUE) or relative to earth
                                          ! (FALSE)
    character(len=8)  :: startloc         ! Which point in array is given by startlat/
                                          ! startlon; set either to 'SWCORNER' or
                                          ! 'CENTER  '
    character(len=9)  :: field            ! Name of the field
    character(len=24) :: hdate            ! Valid data for data YYYY:MM:DD_HH:mm:ss
    character(len=25) :: units            ! Units of data
    character(len=32) :: map_source       ! Source model / originating center
    character(len=46) :: desc             ! Short description of data
  contains
    private
    procedure, public, pass(self)  :: read  => read_met_field
    procedure, public, pass(self)  :: write => write_met_field
    generic, public                :: write(formatted) => write_formatted
    procedure, private, pass(self) :: write_formatted
  end type wps_intermediate_field_t

  interface wps_intermediate_field_t
    module procedure constructor
  end interface wps_intermediate_field_t

contains

  function constructor(nx, ny, iproj, xfcst, xlvl, startlat, startlon, is_wind_grid_rel, &
                       startloc, field, units, map_source, desc, year, month, day, hour, &
                       nlats, deltalat, deltalon, dx, dy, xlonc, truelat1, truelat2,     &
                       minute, second) result(self)

    implicit none

    integer, intent(in)           :: nx
    integer, intent(in)           :: ny
    integer, intent(in)           :: iproj
    real, intent(in)              :: xfcst
    real, intent(in)              :: xlvl
    real, intent(in)              :: startlat
    real, intent(in)              :: startlon
    logical, intent(in)           :: is_wind_grid_rel
    character(len=*), intent(in)  :: startloc
    character(len=*), intent(in)  :: field
    character(len=*), intent(in)  :: units
    character(len=*), intent(in)  :: map_source
    character(len=*), intent(in)  :: desc
    integer, intent(in)           :: year
    integer, intent(in)           :: month
    integer, intent(in)           :: day
    integer, intent(in)           :: hour
    real, intent(in), optional    :: nlats
    real, intent(in), optional    :: deltalat
    real, intent(in), optional    :: deltalon
    real, intent(in), optional    :: dx
    real, intent(in), optional    :: dy
    real, intent(in), optional    :: xlonc
    real, intent(in), optional    :: truelat1
    real, intent(in), optional    :: truelat2
    integer, intent(in), optional :: minute
    integer, intent(in), optional :: second

    type(wps_intermediate_field_t) :: self

    self%version          = VERSION
    self%nx               = nx
    self%ny               = ny
    self%iproj            = iproj
    if (present(nlats)) then
      self%nlats          = nlats
    else
      self%nlats          = UNDEF
    end if
    self%xfcst            = xfcst
    self%xlvl             = xlvl
    self%startlat         = startlat
    self%startlon         = startlon
    if (present(deltalat)) then
      self%deltalat       = deltalat
    else
      self%deltalat       = UNDEF
    end if
    if (present(deltalon)) then
      self%deltalon       = deltalon
    else
      self%deltalon       = UNDEF
    end if
    if (present(dx)) then
      self%dx             = dx
    else  
      self%dx             = UNDEF
    end if
    if (present(dy)) then
      self%dy             = dy
    else
      self%dy             = UNDEF
    end if
    if (present(xlonc)) then
      self%xlonc          = xlonc
    else
      self%xlonc          = UNDEF
    end if
    if (present(truelat1)) then
      self%truelat1       = truelat1
    else
      self%truelat1       = UNDEF
    end if
    if (present(truelat2)) then
      self%truelat2       = truelat2
    else
      self%truelat2       = UNDEF
    end if
    self%earth_radius     = EARTH_RADIUS
    self%is_wind_grid_rel = is_wind_grid_rel
    self%startloc         = trim(startloc)
    self%field            = trim(field)
    write(self%hdate, '(i4.4, a1, i2.2, a1, i2.2, a1, i2.2)') year, '-', month, '-', day, '_', hour ! like 2025-10-27_15
    if (present(minute)) then
      write(self%hdate(14:16), '(a1, i2.2)') ':', minute
      if (present(second)) then
        write(self%hdate(17:19), '(a1, i2.2)') ':', second
      else
        ! write(self%hdate(17:19), '(a3)') ':00'
        self%hdate = trim(self%hdate) // ':00'
      end if
    else
      ! write(self%hdate(14:19), '(a6)') ':00:00'
      self%hdate = trim(self%hdate) // ':00:00'
    end if
    self%units            = trim(units)
    self%map_source       = trim(map_source)
    self%desc             = trim(desc)

  end function constructor

  subroutine read_met_field(self, iunit, status)

    implicit none

    class(wps_intermediate_field_t), intent(inout) :: self
    integer, intent(in)                            :: iunit
    integer, intent(out)                           :: status

    read(unit=iunit, iostat=status) self%version
    if (status /= 0) then
      if (status > 0) then
        write(*, *) 'ERROR: can not read format version from WPS intermediate format file!'
      end if
      return
    end if
    if (self%version /= VERSION) then
      write(*, *) 'ERROR: unsupported WPS intermediate format version ', self%version, '!'
      status = 1
      return
    end if

    read(iunit, iostat=status) self%hdate, self%xfcst, self%map_source, self%field, &
                               self%units, self%desc, self%xlvl, self%nx, self%ny,  &
                               self%iproj
    if (status /= 0) then
      write(*, *) 'ERROR: can not read metadata part 1 from WPS intermediate format file!'
      return
    end if

    if (self%iproj == IPROJ_LATLON) then ! Cylindrical equidistant (lat/lon)
      read(iunit, iostat=status) self%startloc, self%startlat, self%startlon,       &
                                 self%deltalat, self%deltalon, self%earth_radius
    else if (self%iproj == IPROJ_MERCATOR) then ! Mercator
      read(iunit, iostat=status) self%startloc, self%startlat, self%startlon,       &
                                 self%dx, self%dy, self%truelat1, self%earth_radius
    else if (self%iproj == IPROJ_LAMBERT_CONFORMAL) then ! Lambert conformal
      read(iunit, iostat=status) self%startloc, self%startlat, self%startlon,       &
                                 self%dx, self%dy, self%xlonc, self%truelat1,       &
                                 self%truelat2, self%earth_radius
    else if (self%iproj == IPROJ_GAUSSIAN) then ! Gaussian
      read(iunit, iostat=status) self%startloc, self%startlat, self%startlon,       &
                                 self%nlats, self%deltalon, self%earth_radius
    else if (self%iproj == IPROJ_POLAR_STEREOGRAPHIC) then ! Polar stereographic
      read(iunit, iostat=status) self%startloc, self%startlat, self%startlon,       &
                                 self%xlonc, self%truelat1, self%earth_radius
    end if
    if (status /= 0) then
      write(*, *) 'ERROR: can not read metadata part 2 from WPS intermediate format file!'
      return
    end if

    read(iunit, iostat=status) self%is_wind_grid_rel
    if (status /= 0) then
      write(*, *) 'ERROR: can not read wind rotation flag from WPS intermediate format file!'
      return
    end if

    if (.not. allocated(self%slab)) then
      allocate(self%slab(self%nx, self%ny))
    end if

    ! Read 2-d array of data
    read(iunit, iostat=status) self%slab
    if (status /= 0) then
      write(*, *) 'ERROR: can not read slab data from WPS intermediate format file!'
      return
    end if

  end subroutine read_met_field

  subroutine write_met_field(self, iunit, hdate, status)

    implicit none

    class(wps_intermediate_field_t), intent(in) :: self
    integer, intent(in)                         :: iunit
    character(len=*), intent(in)                :: hdate
    integer, intent(out), optional              :: status

    integer                                     :: status_

    if (self%hdate /= trim(hdate)) then
      write(*, *) "Error: WPS field date does not match WPS file date."
      if (present(status)) status = 1
      return
    end if

    write(unit=iunit, iostat=status_) self%version
    if (present(status)) status = status_
    if (status_ /= 0) then
      write(*, *) 'ERROR: can not write format version to WPS intermediate format file!'
      return
    endif

    write(iunit, iostat=status_) self%hdate, self%xfcst, self%map_source, self%field, &
                                 self%units, self%desc, self%xlvl, self%nx, self%ny,  &
                                 self%iproj
    if (present(status)) status = status_
    if (status_ /= 0) then
      write(*, *) 'ERROR: can not write metadata part 1 to WPS intermediate format file!'
      return
    end if

    if (self%iproj == IPROJ_LATLON) then ! Cylindrical equidistant (lat/lon)
      write(iunit, iostat=status_) self%startloc, self%startlat, self%startlon,       &
                                   self%deltalat, self%deltalon, self%earth_radius
    else if (self%iproj == IPROJ_MERCATOR) then ! Mercator
      write(iunit, iostat=status_) self%startloc, self%startlat, self%startlon,       &
                                   self%dx, self%dy, self%truelat1, self%earth_radius
    else if (self%iproj == IPROJ_LAMBERT_CONFORMAL) then ! Lambert conformal
      write(iunit, iostat=status_) self%startloc, self%startlat, self%startlon,       &
                                   self%dx, self%dy, self%xlonc, self%truelat1,       &
                                   self%truelat2, self%earth_radius
    else if (self%iproj == IPROJ_GAUSSIAN) then ! Gaussian
      write(iunit, iostat=status_) self%startloc, self%startlat, self%startlon,       &
                                   self%nlats, self%deltalon, self%earth_radius
    else if (self%iproj == IPROJ_POLAR_STEREOGRAPHIC) then ! Polar stereographic
      write(iunit, iostat=status_) self%startloc, self%startlat, self%startlon,       &
                                   self%xlonc, self%truelat1, self%earth_radius
    end if
    if (present(status)) status = status_
    if (status_ /= 0) then
      write(*, *) 'ERROR: can not write metadata part 2 to WPS intermediate format file!'
      return
    end if

    write(iunit, iostat=status_) self%is_wind_grid_rel
    if (present(status)) status = status_
    if (status_ /= 0) then
      write(*, *) 'ERROR: can not write wind rotation flag to WPS intermediate format file!'
      return
    end if

    ! Write 2-d array of data
    write(iunit, iostat=status_) self%slab
    if (present(status)) status = status_
    if (status_ /= 0) then
      write(*, *) 'ERROR: can not write slab data to WPS intermediate format file!'
      return
    end if

  end subroutine write_met_field

  subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)

    implicit none

    class(wps_intermediate_field_t), intent(in) :: self       !< The string.
    integer, intent(in)                         :: unit      !< Logical unit.
    character(len=*), intent(in)                :: iotype    !< Edit descriptor.
    integer, intent(in)                         :: v_list(:) !< Edit descriptor list.
    integer, intent(out)                        :: iostat    !< IO status code.
    character(len=*), intent(inout)             :: iomsg     !< IO status message.

    write(unit, *, iostat=iostat) '================================================', new_line('A')
    write(unit, *, iostat=iostat) ' FIELD = ', trim(self%field), new_line('A')
    write(unit, *, iostat=iostat) ' UNITS = ', trim(self%units), ' DESCRIPTION = ', trim(self%desc), new_line('A')
    write(unit, *, iostat=iostat) ' DATE = ', trim(self%hdate), ' FCST = ', self%xfcst, new_line('A')
    write(unit, *, iostat=iostat) ' SOURCE = ', trim(self%map_source), new_line('A')
    write(unit, *, iostat=iostat) ' LEVEL = ', self%xlvl, new_line('A')
    write(unit, *, iostat=iostat) ' I,J DIMS = ', self%nx, self%ny, new_line('A')
    if (self%iproj == IPROJ_LATLON) then ! Cylindrical equidistant (lat/lon)
      write(unit, *, iostat=iostat) ' IPROJ = ', self%iproj, ' PROJECTION = LAT LON', new_line('A')
      write(unit, *, iostat=iostat) '   REF_X, REF_Y = ', 1.0, 1.0, new_line('A')
      write(unit, *, iostat=iostat) '   REF_LAT, REF_LON = ', self%startlat, self%startlon, new_line('A')
      write(unit, *, iostat=iostat) '   DLAT, DLON = ', self%deltalat, self%deltalon, new_line('A')
    else if (self%iproj == IPROJ_MERCATOR) then ! Mercator
      write(unit, *, iostat=iostat) ' IPROJ = ', self%iproj, ' PROJECTION = MERCATOR', new_line('A')
      write(unit, *, iostat=iostat) '   REF_X, REF_Y = ', 1.0, 1.0, new_line('A')
      write(unit, *, iostat=iostat) '   REF_LAT, REF_LON = ', self%startlat, self%startlon, new_line('A')
      write(unit, *, iostat=iostat) '   DX, DY = ', self%dx, self%dy, new_line('A')
      write(unit, *, iostat=iostat) '   TRUELAT1 = ', self%truelat1, new_line('A')
    else if (self%iproj == IPROJ_LAMBERT_CONFORMAL) then ! Lambert conformal
      write(unit, *, iostat=iostat) ' IPROJ = ', self%iproj, ' PROJECTION = LAMBERT', new_line('A')
      write(unit, *, iostat=iostat) '   REF_X, REF_Y = ', 1.0, 1.0, new_line('A')
      write(unit, *, iostat=iostat) '   REF_LAT, REF_LON = ', self%startlat, self%startlon, new_line('A')
      write(unit, *, iostat=iostat) '   DX, DY = ', self%dx, self%dy, new_line('A')
      write(unit, *, iostat=iostat) '   XLONC = ', self%xlonc, new_line('A')
      write(unit, *, iostat=iostat) '   TRUELAT1 = ', self%truelat1, new_line('A')
      write(unit, *, iostat=iostat) '   TRUELAT2 = ', self%truelat2, new_line('A')
    else if (self%iproj == IPROJ_GAUSSIAN) then ! Gaussian
      write(unit, *, iostat=iostat) ' IPROJ = ', self%iproj, ' PROJECTION = GAUSSIAN', new_line('A')
      write(unit, *, iostat=iostat) '   REF_X, REF_Y = ', 1.0, 1.0, new_line('A')
      write(unit, *, iostat=iostat) '   REF_LAT, REF_LON = ', self%startlat, self%startlon, new_line('A')
      write(unit, *, iostat=iostat) '   NLATS = ', self%nlats, new_line('A')
    else if (self%iproj == IPROJ_POLAR_STEREOGRAPHIC) then ! Polar stereographic
      write(unit, *, iostat=iostat) ' IPROJ = ', self%iproj, ' PROJECTION = POLAR STEREOGRAPHIC', new_line('A')
      write(unit, *, iostat=iostat) '   REF_X, REF_Y = ', 1.0, 1.0, new_line('A')
      write(unit, *, iostat=iostat) '   REF_LAT, REF_LON = ', self%startlat, self%startlon, new_line('A')
      write(unit, *, iostat=iostat) '   XLONC = ', self%xlonc, new_line('A')
      write(unit, *, iostat=iostat) '   TRUELAT1 = ', self%truelat1, new_line('A')
    end if
    write(unit, *, iostat=iostat) '   EARTH_RADIUS = ', self%earth_radius, new_line('A')
    write(unit, *, iostat=iostat) ' DATA(1,1) = ', self%slab(1, 1), new_line('A')

  end subroutine write_formatted

end module wps_intermediate_field_mod
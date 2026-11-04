module wpsim_projection
  use wpsim_constants

  implicit none
  private
  public :: proj_params, base_proj, latlon_proj, mercator_proj, lambert_proj, gaussian_proj, stereographic_proj

  type :: proj_params
    integer          :: iproj
    real             :: nlats
    real             :: startlat
    real             :: startlon
    real             :: deltalat
    real             :: deltalon
    real             :: dx
    real             :: dy
    real             :: xlonc
    real             :: truelat1
    real             :: truelat2
    real             :: earth_radius
    logical          :: is_wind_grid_rel
    character(len=8) :: startloc
  end type proj_params

  type, abstract :: base_proj
    integer          :: iproj
    real             :: startlat
    real             :: startlon
    real             :: earth_radius
    logical          :: is_wind_grid_rel
    character(len=8) :: startloc
  contains
    procedure(init_iface), public, pass(self), deferred :: init
  end type base_proj

  abstract interface
    subroutine init_iface(self, params)
      import :: base_proj, proj_params
      class(base_proj), intent(inout) :: self
      type(proj_params), intent(in)   :: params
    end subroutine
  end interface

  type, extends(base_proj) :: latlon_proj
    real             :: deltalat
    real             :: deltalon
  contains
    procedure, public, pass(self) :: init => latlon_init
  end type latlon_proj

  type, extends(base_proj) :: mercator_proj
    real             :: dx
    real             :: dy
    real             :: truelat1
  contains
    procedure, public, pass(self) :: init => mercator_init
  end type mercator_proj

  type, extends(base_proj) :: lambert_proj
    real             :: dx
    real             :: dy
    real             :: xlonc
    real             :: truelat1
    real             :: truelat2
  contains
    procedure, public, pass(self) :: init => lambert_init
  end type lambert_proj

  type, extends(base_proj) :: gaussian_proj
    real             :: nlats
    real             :: deltalon
  contains
    procedure, public, pass(self) :: init => gaussian_init
  end type gaussian_proj

  type, extends(base_proj) :: stereographic_proj
    real             :: xlonc
    real             :: truelat1
  contains
    procedure, public, pass(self) :: init => stereographic_init
  end type stereographic_proj

  interface proj_params
    module procedure constructor
  end interface proj_params

contains

  function constructor(iproj, startlat, startlon, nlats, deltalat, deltalon, dx, dy, xlonc, truelat1, truelat2, earth_rad, is_wind_grid_rel, startloc) result(params)

    implicit none
    integer, intent(in)                    :: iproj
    real, intent(in)                       :: startlat
    real, intent(in)                       :: startlon
    real, intent(in), optional             :: nlats
    real, intent(in), optional             :: deltalat
    real, intent(in), optional             :: deltalon
    real, intent(in), optional             :: dx
    real, intent(in), optional             :: dy
    real, intent(in), optional             :: xlonc
    real, intent(in), optional             :: truelat1
    real, intent(in), optional             :: truelat2
    real, intent(in), optional             :: earth_rad
    logical, intent(in), optional          :: is_wind_grid_rel
    character(len=*), intent(in), optional :: startloc

    type(proj_params)                      :: params

    params%iproj = iproj
    params%startlat = startlat
    params%startlon = startlon

    if (present(nlats)) then
      params%nlats = nlats
    else
      params%nlats = UNDEF_INT
    end if

    if (present(deltalat)) then
      params%deltalat = deltalat
    else
      params%deltalat = UNDEF_REAL
    end if

    if (present(deltalon)) then
      params%deltalon = deltalon
    else
      params%deltalon = UNDEF_REAL
    end if

    if (present(dx)) then
      params%dx = dx
    else
      params%dx = UNDEF_REAL
    end if

    if (present(dy)) then
      params%dy = dy
    else
      params%dy = UNDEF_REAL
    end if

    if (present(xlonc)) then
      params%xlonc = xlonc
    else
      params%xlonc = UNDEF_REAL
    end if

    if (present(truelat1)) then
      params%truelat1 = truelat1
    else
      params%truelat1 = UNDEF_REAL
    end if

    if (present(truelat2)) then
      params%truelat2 = truelat2
    else
      params%truelat2 = UNDEF_REAL
    end if

    if (present(earth_rad)) then
      params%earth_radius = earth_rad
    else
      params%earth_radius = EARTH_RADIUS
    end if

    if (present(is_wind_grid_rel)) then
      params%is_wind_grid_rel = is_wind_grid_rel
    else
      params%is_wind_grid_rel = .false.
    end if

    if (present(startloc)) then
      params%startloc = startloc
    else
      params%startloc = 'SWCORNER'
    end if

  end function constructor

  subroutine latlon_init(self, params)

    implicit none
    class(latlon_proj), intent(inout) :: self
    type(proj_params), intent(in)     :: params

    self%iproj            = params%iproj
    self%startlat         = params%startlat
    self%startlon         = params%startlon
    self%deltalat         = params%deltalat
    self%deltalon         = params%deltalon
    self%earth_radius     = params%earth_radius
    self%is_wind_grid_rel = params%is_wind_grid_rel
    self%startloc         = params%startloc

  end subroutine latlon_init

  subroutine mercator_init(self, params)

    implicit none
    class(mercator_proj), intent(inout) :: self
    type(proj_params), intent(in)       :: params

    self%iproj            = params%iproj
    self%startlat         = params%startlat
    self%startlon         = params%startlon
    self%dx               = params%dx
    self%dy               = params%dy
    self%truelat1         = params%truelat1
    self%earth_radius     = params%earth_radius
    self%is_wind_grid_rel = params%is_wind_grid_rel
    self%startloc         = params%startloc

  end subroutine mercator_init

  subroutine lambert_init(self, params)

    implicit none
    class(lambert_proj), intent(inout) :: self
    type(proj_params), intent(in)      :: params

    self%iproj            = params%iproj
    self%startlat         = params%startlat
    self%startlon         = params%startlon
    self%dx               = params%dx
    self%dy               = params%dy
    self%xlonc            = params%xlonc
    self%truelat1         = params%truelat1
    self%truelat2         = params%truelat2
    self%earth_radius     = params%earth_radius
    self%is_wind_grid_rel = params%is_wind_grid_rel
    self%startloc         = params%startloc

  end subroutine lambert_init

  subroutine gaussian_init(self, params)

    implicit none
    class(gaussian_proj), intent(inout) :: self
    type(proj_params), intent(in)       :: params

    self%iproj            = params%iproj
    self%startlat         = params%startlat
    self%startlon         = params%startlon
    self%nlats            = params%nlats
    self%deltalon         = params%deltalon
    self%earth_radius     = params%earth_radius
    self%is_wind_grid_rel = params%is_wind_grid_rel
    self%startloc         = params%startloc

  end subroutine gaussian_init

  subroutine stereographic_init(self, params)

    implicit none
    class(stereographic_proj), intent(inout) :: self
    type(proj_params), intent(in)            :: params

    self%iproj            = params%iproj
    self%startlat         = params%startlat
    self%startlon         = params%startlon
    self%xlonc            = params%xlonc
    self%truelat1         = params%truelat1
    self%earth_radius     = params%earth_radius
    self%is_wind_grid_rel = params%is_wind_grid_rel
    self%startloc         = params%startloc

  end subroutine stereographic_init

end module wpsim_projection
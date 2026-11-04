module wpsim_projection
  use wpsim_constants

  implicit none
  private
  public :: wpsim_proj

  type :: wpsim_proj
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
  end type wpsim_proj

  interface wpsim_proj
    module procedure constructor
  end interface wpsim_proj

contains

  function constructor(iproj, startlat, startlon, nlats, deltalat, deltalon, dx, dy, xlonc, truelat1, truelat2, earth_rad, is_wind_grid_rel, startloc) result(proj)

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

    type(wpsim_proj)                       :: proj

    proj%iproj = iproj
    proj%startlat = startlat
    proj%startlon = startlon

    if (present(nlats)) then
      proj%nlats = nlats
    else
      proj%nlats = UNDEF_INT
    end if

    if (present(deltalat)) then
      proj%deltalat = deltalat
    else
      proj%deltalat = UNDEF_REAL
    end if

    if (present(deltalon)) then
      proj%deltalon = deltalon
    else
      proj%deltalon = UNDEF_REAL
    end if

    if (present(dx)) then
      proj%dx = dx
    else
      proj%dx = UNDEF_REAL
    end if

    if (present(dy)) then
      proj%dy = dy
    else
      proj%dy = UNDEF_REAL
    end if

    if (present(xlonc)) then
      proj%xlonc = xlonc
    else
      proj%xlonc = UNDEF_REAL
    end if

    if (present(truelat1)) then
      proj%truelat1 = truelat1
    else
      proj%truelat1 = UNDEF_REAL
    end if

    if (present(truelat2)) then
      proj%truelat2 = truelat2
    else
      proj%truelat2 = UNDEF_REAL
    end if

    if (present(earth_rad)) then
      proj%earth_radius = earth_rad
    else
      proj%earth_radius = EARTH_RADIUS
    end if

    if (present(is_wind_grid_rel)) then
      proj%is_wind_grid_rel = is_wind_grid_rel
    else
      proj%is_wind_grid_rel = .false.
    end if

    if (present(startloc)) then
      proj%startloc = startloc
    else
      proj%startloc = SWCORNER
    end if

  end function constructor

end module wpsim_projection
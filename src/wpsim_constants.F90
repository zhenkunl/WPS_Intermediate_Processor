module wpsim_constants
!-----------------------------------------------------------------------
! Constants for WPSIM
!-----------------------------------------------------------------------
  implicit none
  public

  integer, parameter          :: IPROJ_LATLON = 0
  integer, parameter          :: IPROJ_MERCATOR = 1
  integer, parameter          :: IPROJ_LAMBERT_CONFORMAL = 3
  integer, parameter          :: IPROJ_GAUSSIAN = 4
  integer, parameter          :: IPROJ_POLAR_STEREOGRAPHIC = 5
  character(len=8), parameter :: SWCORNER = 'SWCORNER'
  character(len=8), parameter :: CENTER =   'CENTER  '
  integer, parameter          :: VERSION = 5
  real, parameter             :: EARTH_RADIUS = 6371.229 ! km
  integer, parameter          :: UNDEF_INT = 999
  real, parameter             :: UNDEF_REAL = 999.0
  integer, parameter          :: MAX_FILENAME_LEN = 1024

end module wpsim_constants
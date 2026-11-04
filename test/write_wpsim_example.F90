program write_wpsim_example
    use wpsim_io
    implicit none

    integer, parameter  :: nx = 2, ny = 3, nz = 4
    type(wpsim_file)    :: im_file
    real                :: a(nx, ny), b(nx, ny, nz)
    type(proj_params)   :: params
    type(latlon_proj)   :: proj
    character(len=24)   :: hdate
    type(wpsim_field2d) :: field2d
    type(wpsim_field3d) :: field3d
    real                :: levels(nz) = [1000.0, 925.0, 850.0, 700.0]

    params = proj_params(iproj=0, startlat=90.0, startlon=0.0, deltalat=-1.0, deltalon=1.0)
    call proj%init(params)

    a = 0.1
    b = 0.2

    hdate = "2026-11-03_18"
    call im_file%open(dir='./', date=hdate)
    field2d = wpsim_field2d("UU", hdate, a)
    field3d = wpsim_field3d("GHT", hdate, b, levels)
    call im_file%write(proj, field2d)
    call im_file%write(proj, field3d)
    call im_file%close()

end program write_wpsim_example
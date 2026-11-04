program write_wpsim_example
    use wpsim_io
    implicit none

    integer, parameter       :: nx = 2, ny = 3, nz = 4, nlayer = 4
    type(wpsim_file)         :: im_file
    real                     :: a(nx, ny), b(nx, ny, nz), c(nx, ny, nlayer)
    type(wpsim_proj)         :: proj
    character(len=24)        :: date
    type(wpsim_field2d)      :: field2d
    type(wpsim_field3d)      :: field3d
    type(wpsim_field3d_soil) :: field3d_soil
    real                     :: levels(nz) = [1000.0, 925.0, 850.0, 700.0]
    character(len=6)         :: layers(nlayer) = ['000010','010040','040100','100200']

    a = 0.1
    b = 0.2
    c = 0.3
    date = "2026-11-03_18"

    proj = wpsim_proj(iproj=0, startlat=90.0, startlon=0.0, deltalat=-1.0, deltalon=1.0)
    field2d = wpsim_field2d("UU", date, a)
    field3d = wpsim_field3d("GHT", date, b, levels)
    field3d_soil = wpsim_field3d_soil("ST", date, c, layers)

    call im_file%create(dir='./', date=date)
    call im_file%write(proj, field2d)
    call im_file%write(proj, field3d)
    call im_file%write(proj, field3d_soil)
    call im_file%close()

end program write_wpsim_example
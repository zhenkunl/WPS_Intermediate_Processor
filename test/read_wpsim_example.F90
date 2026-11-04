program read_wpsim_example
    use wpsim_io
    implicit none

    type(wpsim_file)    :: im_file

    call im_file%open(file="examples/FILE:2013-08-11_00")
    call im_file%read()
    call im_file%print()
    call im_file%close()

end program read_wpsim_example
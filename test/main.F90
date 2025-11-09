program test
    use wps_intermediate_processor
    implicit none

    type(wps_intermediate_file_t)  :: wps_file
    ! type(wps_intermediate_field_t) :: wps_field
    integer                        :: status

    wps_file = wps_intermediate_file_t('/home/zhenkun/work/pgi/WPS-4.2', 2025, 10, 27, 13, 30)
    call wps_file%open()
    call wps_file%read()
    write(*, *) 'successfully read WPS intermediate format file.'
    call wps_file%close()
    
end program test
program test
    use wps_intermediate_processor
    implicit none

    type(wps_intermediate_file_t)  :: wps_file
    ! type(wps_intermediate_field_t) :: wps_field

    wps_file = wps_intermediate_file_t('examples', 2013, 08, 11, 00)
    call wps_file%open()
    call wps_file%read()
    write(*, *) 'successfully read WPS intermediate format file.'
    call wps_file%close()
    
end program test
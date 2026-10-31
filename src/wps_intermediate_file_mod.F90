module wps_intermediate_file_mod
  use wps_intermediate_field_mod

  implicit none
  private
  public :: wps_intermediate_file_t

  integer, parameter :: MAX_FILENAME_LEN = 1024
  integer, parameter :: IUNIT            = 13

  type :: wps_intermediate_file_t
    private
    character(len=24)               :: hdate
    character(len=MAX_FILENAME_LEN) :: prefix
    character(len=MAX_FILENAME_LEN) :: file
  contains
    private
    procedure, public, pass(self)   :: open  => open_wps_file
    procedure, public, pass(self)   :: close => close_wps_file
    procedure, public, pass(self)   :: read  => read_all_fields
    procedure, public, pass(self)   :: write => write_met_field
  end type wps_intermediate_file_t

  interface wps_intermediate_file_t
    module procedure constructor_from_date
    module procedure constructor_from_path
  end interface wps_intermediate_file_t

contains

  function constructor_from_date(output_dir, year, month, day, hour, minute, second, prefix) result(self)

    implicit none

    character(len=*), intent(in)           :: output_dir
    integer, intent(in)                    :: year, month, day, hour
    integer, intent(in), optional          :: minute, second
    character(len=*), intent(in), optional :: prefix
    type(wps_intermediate_file_t)          :: self

    character(len=24)                      :: file_hdate

    write(file_hdate, '(i4.4, a1, i2.2, a1, i2.2, a1, i2.2)') year, '-', month, '-', day, '_', hour ! like 2025-10-27_15
    if (present(minute)) then
      write(file_hdate(14:16), '(a1, i2.2)') ':', minute
      if (present(second)) then
        write(file_hdate(17:19), '(a1, i2.2)') ':', second
      end if
    end if
    if (present(prefix)) then
      self%prefix = trim(prefix)
    else
      self%prefix = 'FILE:'
    end if
    self%file = trim(output_dir)//"/"//trim(self%prefix)//trim(file_hdate)
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

  end function constructor_from_date

  function constructor_from_path(path) result(self)

    implicit none

    character(len=*), intent(in)    :: path
    type(wps_intermediate_file_t)   :: self

    integer                         :: pos
    character(len=MAX_FILENAME_LEN) :: filename

    self%file = path
    pos = scan(path, '/', back=.true.)
    filename = path(pos+1:)
    pos = index(filename, ':')
    if (pos /= 0) then
      self%prefix = filename(1:pos)
      self%hdate  = filename(pos+1:)
    else
      self%prefix = ''
      self%hdate  = ''
    end if

  end function constructor_from_path

  subroutine open_wps_file(self)

    implicit none

    class(wps_intermediate_file_t), intent(in) :: self

    integer                                    :: status

    open(unit=IUNIT, file=trim(self%file), form='unformatted', status='unknown', iostat=status)
    if (status /= 0) then
      write(*, *) 'ERROR: can not open WPS intermediate format file!'
      return
    end if

  end subroutine open_wps_file

  subroutine close_wps_file(self)

    implicit none

    class(wps_intermediate_file_t), intent(in) :: self

    integer                                    :: status

    close(unit=IUNIT, iostat=status)
    if (status /= 0) then
      write(*, *) 'ERROR: can not close WPS intermediate format file!'
      return
    end if

  end subroutine close_wps_file

  subroutine read_all_fields(self)

    implicit none

    class(wps_intermediate_file_t), intent(in) :: self
    
    integer                                    :: status
    type(wps_intermediate_field_t)             :: wps_field

    do while (.true.)
      call wps_field%read(IUNIT, status)
      if (status /= 0) exit
      write(*, *) wps_field
    end do

  end subroutine read_all_fields

  subroutine write_met_field(self, wps_field, status)

    implicit none

    class(wps_intermediate_file_t), intent(in) :: self
    type(wps_intermediate_field_t), intent(in) :: wps_field
    integer, intent(out), optional             :: status

    call wps_field%write(IUNIT, self%hdate, status)

  end subroutine write_met_field

end module wps_intermediate_file_mod
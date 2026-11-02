module wpsim_file_mod
  use wpsim_constants
  use wpsim_projection
  use wpsim_field_mod

  implicit none
  private
  public :: wpsim_file

  type :: wpsim_file
    private
    character(len=24)                :: hdate
    character(len=MAX_FILENAME_LEN)  :: dir
    character(len=MAX_FILENAME_LEN)  :: filename
    character(len=MAX_FILENAME_LEN)  :: prefix
    integer                          :: iunit
    type(wpsim_record_link), pointer :: record_link
  contains
    private
    procedure, public, pass(self)   :: open  => open_file
    procedure, public, pass(self)   :: close => close_file
    procedure, public, pass(self)   :: read  => read_records
    procedure, public, pass(self)   :: print => print_records
    procedure, private, pass(self)  :: release => release_link
  end type wpsim_file

contains

  subroutine open_file(self, dir, date, prefix, file)

    implicit none
    class(wpsim_file), intent(inout)       :: self
    character(len=*), intent(in), optional :: dir
    character(len=*), intent(in), optional :: date
    character(len=*), intent(in), optional :: prefix
    character(len=*), intent(in), optional :: file

    character(len=5)                       :: action
    integer                                :: iostat

    if (present(file)) then
      self%filename = trim(file)
      action        = 'READ'
    else
      if (.not. present(dir) .or. .not. present(date)) then
        error stop "ERROR: Either filename or both dir and date must be provided."
      end if
      self%dir   = trim(dir)
      self%hdate = trim(date)
      if (len_trim(self%hdate) > 19) then
        error stop "ERROR: datetime format must be YYYY-MM-DD_hh:mm:ss"
      end if
      if (present(prefix)) then
        self%prefix = trim(prefix)
      else
        self%prefix = 'FILE:'
      end if
      self%filename = self%dir//"/"//self%prefix//self%hdate
      action        = 'WRITE'
    end if

    open(newunit=self%iunit, file=self%filename, form='unformatted', action=action, iostat=iostat)
    if (iostat /= 0) then
      error stop "ERROR: can not open WPS intermediate format file!"
      return
    end if

  end subroutine open_file

  subroutine close_file(self)

    implicit none
    class(wpsim_file), intent(inout) :: self
    integer                          :: iostat

    call self%release()
    close(unit=self%iunit, iostat=iostat)
    if (iostat /= 0) then
      error stop "ERROR: can not close WPS intermediate format file!"
      return
    end if

  end subroutine close_file

  subroutine read_records(self)

    implicit none
    class(wpsim_file), intent(inout) :: self

    integer                          :: iostat
    type(wpsim_record)               :: record
    type(wpsim_record_link), pointer :: record_link_pointer

    allocate(self%record_link)
    self%record_link%next => null()
    record_link_pointer => self%record_link
    do while (.true.)
      call record%read(self%iunit, iostat)
      if (iostat /= 0) exit
      record_link_pointer%record = record
      allocate(record_link_pointer%next)
      record_link_pointer => record_link_pointer%next
      record_link_pointer%next => null()
    end do

  end subroutine read_records

  subroutine print_records(self, n)

    implicit none
    class(wpsim_file), intent(in)    :: self
    integer, intent(in), optional    :: n ! the number of fields to write, if not provided, write all fields

    type(wpsim_record_link), pointer :: record_link_pointer
    integer                          :: i
    integer                          :: limit

    if (present(n)) then
      limit = n
    else
      limit = huge(0)
    end if

    i = 1
    record_link_pointer => self%record_link
    do while (.true.)
      write(*, *) record_link_pointer%record
      record_link_pointer => record_link_pointer%next
      if (.not. associated(record_link_pointer%next) .or. i > limit) exit
      i = i + 1
    end do

  end subroutine print_records

  subroutine release_link(self)

    implicit none
    class(wpsim_file), intent(inout) :: self

    type(wpsim_record_link), pointer :: record_link_pointer

    do while (.true.)
      record_link_pointer => self%record_link%next
      deallocate(self%record_link)
      self%record_link => record_link_pointer
      if (.not. associated(record_link_pointer)) exit
    end do

  end subroutine release_link

end module wpsim_file_mod
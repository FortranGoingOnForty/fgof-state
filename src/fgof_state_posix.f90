module fgof_state_posix
  use iso_c_binding, only : c_char, c_int, c_null_char
  implicit none
  private

  public :: directory_exists_posix, ensure_directory_posix, path_exists_posix, remove_file_posix

  interface
    integer(c_int) function fgof_state_directory_exists(path) bind(c, name="fgof_state_directory_exists")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: path(*)
    end function fgof_state_directory_exists

    integer(c_int) function fgof_state_ensure_directory(path, error_code) bind(c, name="fgof_state_ensure_directory")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: path(*)
      integer(c_int), intent(out) :: error_code
    end function fgof_state_ensure_directory

    integer(c_int) function fgof_state_path_exists(path) bind(c, name="fgof_state_path_exists")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: path(*)
    end function fgof_state_path_exists

    integer(c_int) function fgof_state_remove_file(path, error_code) bind(c, name="fgof_state_remove_file")
      import :: c_char, c_int
      character(kind=c_char), intent(in) :: path(*)
      integer(c_int), intent(out) :: error_code
    end function fgof_state_remove_file
  end interface

contains

  logical function path_exists_posix(path) result(exists)
    character(len=*), intent(in) :: path
    character(kind=c_char), allocatable :: c_path(:)

    if (len(path) == 0) then
      exists = .false.
      return
    end if

    allocate(c_path(0))
    c_path = to_c_string(path)
    exists = (fgof_state_path_exists(c_path) /= 0_c_int)
  end function path_exists_posix

  logical function directory_exists_posix(path) result(exists)
    character(len=*), intent(in) :: path
    character(kind=c_char), allocatable :: c_path(:)

    if (len(path) == 0) then
      exists = .false.
      return
    end if

    allocate(c_path(0))
    c_path = to_c_string(path)
    exists = (fgof_state_directory_exists(c_path) /= 0_c_int)
  end function directory_exists_posix

  logical function ensure_directory_posix(path, error_code) result(success)
    character(len=*), intent(in) :: path
    integer, intent(out) :: error_code
    character(kind=c_char), allocatable :: c_path(:)
    integer(c_int) :: c_error

    if (len(path) == 0) then
      error_code = 22
      success = .false.
      return
    end if

    allocate(c_path(0))
    c_path = to_c_string(path)
    success = (fgof_state_ensure_directory(c_path, c_error) /= 0_c_int)
    error_code = c_error
  end function ensure_directory_posix

  logical function remove_file_posix(path, error_code) result(success)
    character(len=*), intent(in) :: path
    integer, intent(out) :: error_code
    character(kind=c_char), allocatable :: c_path(:)
    integer(c_int) :: c_error

    if (len(path) == 0) then
      error_code = 22
      success = .false.
      return
    end if

    allocate(c_path(0))
    c_path = to_c_string(path)
    success = (fgof_state_remove_file(c_path, c_error) /= 0_c_int)
    error_code = c_error
  end function remove_file_posix

  function to_c_string(text) result(c_text)
    character(len=*), intent(in) :: text
    character(kind=c_char), allocatable :: c_text(:)
    integer :: i

    allocate(c_text(0:len(text)))
    do i = 1, len(text)
      c_text(i - 1) = text(i:i)
    end do
    c_text(len(text)) = c_null_char
  end function to_c_string

end module fgof_state_posix

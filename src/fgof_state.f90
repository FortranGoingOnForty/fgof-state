module fgof_state
  use fgof_state_posix, only : directory_exists_posix, ensure_directory_posix, path_exists_posix, remove_file_posix
  use fgof_temp, only : atomic_write
  use fgof_temp_types, only : write_result
  use fgof_state_types, only : &
    FGOF_STATE_ERR_INTERNAL, &
    FGOF_STATE_ERR_INVALID_OPTIONS, &
    FGOF_STATE_ERR_IO, &
    FGOF_STATE_ERR_NOT_FOUND, &
    FGOF_STATE_ERR_VERSION, &
    FGOF_STATE_OK, &
    state_document, &
    state_options, &
    state_root, &
    state_text_result
  implicit none
  private

  public :: &
    FGOF_STATE_ERR_INTERNAL, &
    FGOF_STATE_ERR_INVALID_OPTIONS, &
    FGOF_STATE_ERR_IO, &
    FGOF_STATE_ERR_NOT_FOUND, &
    FGOF_STATE_ERR_VERSION, &
    FGOF_STATE_OK, &
    clear_state_document, &
    clear_state_options, &
    clear_state_root, &
    clear_state_text_result, &
    ensure_state_root, &
    load_state_text, &
    remove_state_document, &
    resolve_state_document, &
    save_state_text, &
    state_backend_name, &
    state_document, &
    state_error_name, &
    state_options, &
    state_path_for_name, &
    state_relative_path_for_name, &
    state_root, &
    state_text_result

  character(len=*), parameter :: STATE_FORMAT_MAGIC = "fgof-state/1"

contains

  function clear_state_options() result(options)
    type(state_options) :: options

    options%create_root = .true.
  end function clear_state_options

  function clear_state_root() result(root)
    type(state_root) :: root

    root%ready = .false.
    root%error_code = FGOF_STATE_OK
    root%path = ""
    root%error_message = ""
  end function clear_state_root

  function clear_state_document() result(document)
    type(state_document) :: document

    document%present = .false.
    document%version = 0
    document%error_code = FGOF_STATE_OK
    document%name = ""
    document%root_path = ""
    document%relative_path = ""
    document%path = ""
    document%error_message = ""
  end function clear_state_document

  function clear_state_text_result() result(result_value)
    type(state_text_result) :: result_value

    result_value%found = .false.
    result_value%version_checked = .false.
    result_value%version_matched = .true.
    result_value%expected_version = 0
    result_value%error_code = FGOF_STATE_OK
    result_value%document = clear_state_document()
    result_value%text = ""
    result_value%error_message = ""
  end function clear_state_text_result

  function ensure_state_root(options) result(root)
    type(state_options), intent(in), optional :: options
    type(state_root) :: root
    type(state_options) :: local_options
    character(len=:), allocatable :: root_path
    integer :: sys_errno
    logical :: success

    local_options = merged_options(options)
    root = clear_state_root()

    if (.not. validate_options(local_options, root)) return

    if (.not. resolved_root_path(local_options, root_path)) then
      call set_root_error(root, FGOF_STATE_ERR_INVALID_OPTIONS, &
                          "unable to resolve state root: set root_dir, XDG_STATE_HOME, or HOME")
      return
    end if

    root%path = root_path

    if (local_options%create_root) then
      success = ensure_directory_posix(root_path, sys_errno)
      if (.not. success) then
        call set_root_error(root, FGOF_STATE_ERR_IO, errno_message("state root creation failed", sys_errno))
        return
      end if
    else
      if (.not. directory_exists_posix(root_path)) then
        call set_root_error(root, FGOF_STATE_ERR_NOT_FOUND, "state root does not exist")
        return
      end if
    end if

    if (.not. directory_exists_posix(root_path)) then
      call set_root_error(root, FGOF_STATE_ERR_IO, "state root exists but is not a directory")
      return
    end if

    root%ready = .true.
    root%error_code = FGOF_STATE_OK
    root%error_message = ""
  end function ensure_state_root

  function state_relative_path_for_name(name) result(path)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: path

    if (.not. valid_document_name(name)) then
      path = ""
      return
    end if

    path = name
  end function state_relative_path_for_name

  function state_path_for_name(root_path, name) result(path)
    character(len=*), intent(in) :: root_path
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: path
    character(len=:), allocatable :: relative_path

    relative_path = state_relative_path_for_name(name)
    if (len(relative_path) == 0) then
      path = ""
      return
    end if

    path = join_path(root_path, relative_path)
  end function state_path_for_name

  function resolve_state_document(name, options) result(document)
    character(len=*), intent(in) :: name
    type(state_options), intent(in), optional :: options
    type(state_document) :: document
    type(state_options) :: local_options
    type(state_root) :: root

    document = clear_state_document()
    document%name = name

    if (.not. valid_document_name(name)) then
      call set_document_error(document, FGOF_STATE_ERR_INVALID_OPTIONS, &
                              "document name must not be empty, contain '/', or be '.' or '..'")
      return
    end if

    local_options = merged_options(options)
    root = ensure_state_root(local_options)
    if (.not. root%ready) then
      document%root_path = root%path
      call set_document_error(document, root%error_code, root%error_message)
      return
    end if

    document%root_path = root%path
    document%relative_path = state_relative_path_for_name(name)
    document%path = state_path_for_name(root%path, name)
    document%present = path_exists_posix(document%path)
    if (document%present .and. directory_exists_posix(document%path)) then
      call set_document_error(document, FGOF_STATE_ERR_IO, "state document path exists but is a directory")
      return
    end if

    document%error_code = FGOF_STATE_OK
    document%error_message = ""
  end function resolve_state_document

  function save_state_text(name, text, options, version) result(document)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: text
    type(state_options), intent(in), optional :: options
    integer, intent(in), optional :: version
    type(state_document) :: document
    type(write_result) :: write_outcome
    integer :: local_version
    character(len=:), allocatable :: encoded_text

    document = clear_state_document()
    document%name = name

    if (.not. valid_document_name(name)) then
      call set_document_error(document, FGOF_STATE_ERR_INVALID_OPTIONS, &
                              "document name must not be empty, contain '/', or be '.' or '..'")
      return
    end if

    local_version = effective_state_version(version)
    if (present(version) .and. local_version <= 0) then
      call set_document_error(document, FGOF_STATE_ERR_INVALID_OPTIONS, "state version must be positive")
      return
    end if

    document = resolve_state_document(name, options)
    if (document%error_code /= FGOF_STATE_OK) return

    encoded_text = encode_state_text(text, local_version)
    write_outcome = atomic_write(document%path, encoded_text)
    if (.not. write_outcome%completed) then
      call set_document_error(document, FGOF_STATE_ERR_IO, write_outcome%error_message)
      return
    end if

    document%present = .true.
    document%version = local_version
    document%error_code = FGOF_STATE_OK
    document%error_message = ""
  end function save_state_text

  function load_state_text(name, options, expected_version) result(result_value)
    character(len=*), intent(in) :: name
    type(state_options), intent(in), optional :: options
    integer, intent(in), optional :: expected_version
    type(state_text_result) :: result_value
    type(state_document) :: document
    integer :: local_expected_version
    character(len=:), allocatable :: encoded_text

    result_value = clear_state_text_result()

    local_expected_version = expected_state_version(expected_version)
    result_value%expected_version = local_expected_version
    if (present(expected_version) .and. local_expected_version <= 0) then
      result_value%error_code = FGOF_STATE_ERR_INVALID_OPTIONS
      result_value%error_message = "expected_version must be positive"
      return
    end if
    result_value%version_checked = local_expected_version > 0

    document = resolve_read_document(name, options)
    result_value%document = document

    if (document%error_code /= FGOF_STATE_OK) then
      result_value%error_code = document%error_code
      result_value%error_message = document%error_message
      return
    end if

    if (.not. document%present) then
      result_value%error_code = FGOF_STATE_ERR_NOT_FOUND
      result_value%error_message = "state document not found"
      return
    end if

    call read_text_file(document%path, encoded_text, result_value%error_code, result_value%error_message)
    if (result_value%error_code /= FGOF_STATE_OK) return

    call decode_state_text(encoded_text, result_value%document, result_value%text, &
                           result_value%error_code, result_value%error_message)
    if (result_value%error_code /= FGOF_STATE_OK) return

    if (local_expected_version > 0 .and. result_value%document%version /= local_expected_version) then
      result_value%version_matched = .false.
      result_value%error_code = FGOF_STATE_ERR_VERSION
      result_value%error_message = version_mismatch_message(local_expected_version, result_value%document%version)
      result_value%text = ""
      return
    end if

    result_value%found = .true.
    result_value%version_matched = .true.
    result_value%error_message = ""
  end function load_state_text

  function remove_state_document(name, options) result(document)
    character(len=*), intent(in) :: name
    type(state_options), intent(in), optional :: options
    type(state_document) :: document
    integer :: sys_errno
    logical :: success

    document = resolve_read_document(name, options)
    if (document%error_code /= FGOF_STATE_OK) return

    if (.not. document%present) then
      call set_document_error(document, FGOF_STATE_ERR_NOT_FOUND, "state document not found")
      return
    end if

    success = remove_file_posix(document%path, sys_errno)
    if (.not. success) then
      call set_document_error(document, FGOF_STATE_ERR_IO, errno_message("state document removal failed", sys_errno))
      return
    end if

    document%present = .false.
    document%error_code = FGOF_STATE_OK
    document%error_message = ""
  end function remove_state_document

  function state_backend_name() result(name)
    character(len=:), allocatable :: name

    name = "posix"
  end function state_backend_name

  function state_error_name(error_code) result(name)
    integer, intent(in) :: error_code
    character(len=:), allocatable :: name

    select case (error_code)
    case (FGOF_STATE_OK)
      name = "ok"
    case (FGOF_STATE_ERR_INVALID_OPTIONS)
      name = "invalid-options"
    case (FGOF_STATE_ERR_NOT_FOUND)
      name = "not-found"
    case (FGOF_STATE_ERR_IO)
      name = "io"
    case (FGOF_STATE_ERR_VERSION)
      name = "version"
    case (FGOF_STATE_ERR_INTERNAL)
      name = "internal"
    case default
      name = "unknown"
    end select
  end function state_error_name

  function merged_options(options) result(local_options)
    type(state_options), intent(in), optional :: options
    type(state_options) :: local_options

    local_options = clear_state_options()
    if (present(options)) local_options = options
  end function merged_options

  function resolve_read_document(name, options) result(document)
    character(len=*), intent(in) :: name
    type(state_options), intent(in), optional :: options
    type(state_document) :: document
    type(state_options) :: local_options

    local_options = merged_options(options)
    local_options%create_root = .false.
    document = resolve_state_document(name, local_options)
  end function resolve_read_document

  integer function effective_state_version(version) result(local_version)
    integer, intent(in), optional :: version

    local_version = 1
    if (present(version)) local_version = version
  end function effective_state_version

  integer function expected_state_version(version) result(local_version)
    integer, intent(in), optional :: version

    local_version = 0
    if (present(version)) local_version = version
  end function expected_state_version

  logical function validate_options(options, root) result(valid)
    type(state_options), intent(in) :: options
    type(state_root), intent(inout) :: root

    valid = .false.

    if (allocated(options%root_dir)) then
      if (len(options%root_dir) == 0) then
        call set_root_error(root, FGOF_STATE_ERR_INVALID_OPTIONS, "root_dir must not be empty")
        return
      end if
    end if

    if (allocated(options%namespace)) then
      if (.not. valid_path_segment(options%namespace)) then
        call set_root_error(root, FGOF_STATE_ERR_INVALID_OPTIONS, &
                            "namespace must not be empty, contain '/', or be '.' or '..'")
        return
      end if
    end if

    if (allocated(options%scope)) then
      if (.not. valid_path_segment(options%scope)) then
        call set_root_error(root, FGOF_STATE_ERR_INVALID_OPTIONS, &
                            "scope must not be empty, contain '/', or be '.' or '..'")
        return
      end if
    end if

    valid = .true.
  end function validate_options

  logical function resolved_root_path(options, root_path) result(valid)
    type(state_options), intent(in) :: options
    character(len=:), allocatable, intent(out) :: root_path
    character(len=:), allocatable :: base_path

    valid = .false.
    root_path = ""

    if (allocated(options%root_dir)) then
      base_path = options%root_dir
    else
      base_path = default_state_base()
      if (.not. allocated(base_path)) return
    end if

    if (allocated(options%namespace)) then
      root_path = join_path(base_path, options%namespace)
    else if (allocated(options%root_dir)) then
      root_path = base_path
    else
      root_path = join_path(base_path, "fgof-state")
    end if

    if (allocated(options%scope)) root_path = join_path(root_path, options%scope)

    valid = .true.
  end function resolved_root_path

  function default_state_base() result(base_path)
    character(len=:), allocatable :: base_path
    character(len=:), allocatable :: xdg_state_home
    character(len=:), allocatable :: home

    xdg_state_home = getenv_text("XDG_STATE_HOME")
    if (allocated(xdg_state_home)) then
      base_path = xdg_state_home
      return
    end if

    home = getenv_text("HOME")
    if (allocated(home)) base_path = join_path(home, ".local/state")
  end function default_state_base

  function getenv_text(name) result(value)
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: value
    integer :: length
    integer :: status

    call get_environment_variable(name, length=length, status=status)
    if (status /= 0 .or. length <= 0) return

    allocate(character(len=length) :: value)
    call get_environment_variable(name, value, status=status)
    if (status /= 0) deallocate(value)
  end function getenv_text

  logical function valid_document_name(name) result(valid)
    character(len=*), intent(in) :: name

    valid = valid_path_segment(name)
  end function valid_document_name

  logical function valid_path_segment(segment) result(valid)
    character(len=*), intent(in) :: segment

    valid = .false.

    if (len(segment) == 0) return
    if (index(segment, "/") > 0) return
    if (segment == "." .or. segment == "..") return

    valid = .true.
  end function valid_path_segment

  function join_path(left, right) result(path)
    character(len=*), intent(in) :: left
    character(len=*), intent(in) :: right
    character(len=:), allocatable :: path

    if (len(left) == 0) then
      path = right
    else if (len(right) == 0) then
      path = left
    else if (left(len(left):len(left)) == "/") then
      path = left // right
    else
      path = left // "/" // right
    end if
  end function join_path

  subroutine set_root_error(root, error_code, message)
    type(state_root), intent(inout) :: root
    integer, intent(in) :: error_code
    character(len=*), intent(in) :: message

    root%ready = .false.
    root%error_code = error_code
    root%error_message = message
  end subroutine set_root_error

  subroutine set_document_error(document, error_code, message)
    type(state_document), intent(inout) :: document
    integer, intent(in) :: error_code
    character(len=*), intent(in) :: message

    document%present = .false.
    document%error_code = error_code
    document%error_message = message
  end subroutine set_document_error

  function errno_message(context, error_code) result(message)
    character(len=*), intent(in) :: context
    integer, intent(in) :: error_code
    character(len=:), allocatable :: message
    character(len=32) :: error_text

    write(error_text, "(i0)") error_code
    message = context // " (errno=" // trim(error_text) // ")"
  end function errno_message

  function encode_state_text(text, version) result(encoded_text)
    character(len=*), intent(in) :: text
    integer, intent(in) :: version
    character(len=:), allocatable :: encoded_text
    character(len=32) :: version_text

    write(version_text, "(i0)") version
    encoded_text = STATE_FORMAT_MAGIC // new_line("a") // trim(version_text) // new_line("a") // text
  end function encode_state_text

  subroutine decode_state_text(encoded_text, document, text, error_code, error_message)
    character(len=*), intent(in) :: encoded_text
    type(state_document), intent(inout) :: document
    character(len=:), allocatable, intent(out) :: text
    integer, intent(out) :: error_code
    character(len=:), allocatable, intent(out) :: error_message
    character(len=:), allocatable :: remainder
    character(len=:), allocatable :: version_text
    integer :: first_break
    integer :: second_break
    integer :: ios
    character(len=256) :: iomsg

    error_code = FGOF_STATE_OK
    error_message = ""
    text = ""

    first_break = index(encoded_text, new_line("a"))
    if (first_break <= 1) then
      error_code = FGOF_STATE_ERR_VERSION
      error_message = "state document has an invalid header"
      return
    end if

    if (encoded_text(:first_break - 1) /= STATE_FORMAT_MAGIC) then
      error_code = FGOF_STATE_ERR_VERSION
      error_message = "state document format is not supported"
      return
    end if

    remainder = encoded_text(first_break + 1:)
    second_break = index(remainder, new_line("a"))
    if (second_break <= 1) then
      error_code = FGOF_STATE_ERR_VERSION
      error_message = "state document is missing version metadata"
      return
    end if

    version_text = remainder(:second_break - 1)
    iomsg = ""
    read(version_text, *, iostat=ios, iomsg=iomsg) document%version
    if (ios /= 0 .or. document%version <= 0) then
      error_code = FGOF_STATE_ERR_VERSION
      error_message = io_status_message("state version metadata is invalid", ios, iomsg)
      return
    end if

    if (first_break + second_break < len(encoded_text)) then
      text = encoded_text(first_break + second_break + 1:)
    else
      text = ""
    end if
  end subroutine decode_state_text

  function version_mismatch_message(expected_version, actual_version) result(message)
    integer, intent(in) :: expected_version
    integer, intent(in) :: actual_version
    character(len=:), allocatable :: message
    character(len=32) :: expected_text
    character(len=32) :: actual_text

    write(expected_text, "(i0)") expected_version
    write(actual_text, "(i0)") actual_version
    message = "state version mismatch: expected " // trim(expected_text) // &
              ", found " // trim(actual_text)
  end function version_mismatch_message

  subroutine read_text_file(path, text, error_code, error_message)
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: text
    integer, intent(out) :: error_code
    character(len=:), allocatable, intent(out) :: error_message
    integer :: file_size
    integer :: ios
    integer :: unit
    character(len=256) :: iomsg

    error_code = FGOF_STATE_OK
    error_message = ""

    inquire(file=path, size=file_size, iostat=ios, iomsg=iomsg)
    if (ios /= 0) then
      error_code = FGOF_STATE_ERR_IO
      error_message = io_status_message("state read failed while sizing file", ios, iomsg)
      text = ""
      return
    end if

    allocate(character(len=file_size) :: text)
    if (file_size == 0) return

    iomsg = ""
    open(newunit=unit, file=path, status="old", access="stream", form="unformatted", &
         action="read", iostat=ios, iomsg=iomsg)
    if (ios /= 0) then
      error_code = FGOF_STATE_ERR_IO
      error_message = io_status_message("state read failed while opening file", ios, iomsg)
      deallocate(text)
      text = ""
      return
    end if

    iomsg = ""
    read(unit, iostat=ios, iomsg=iomsg) text
    if (ios /= 0) then
      close(unit)
      error_code = FGOF_STATE_ERR_IO
      error_message = io_status_message("state read failed while reading file", ios, iomsg)
      deallocate(text)
      text = ""
      return
    end if

    iomsg = ""
    close(unit, iostat=ios, iomsg=iomsg)
    if (ios /= 0) then
      error_code = FGOF_STATE_ERR_IO
      error_message = io_status_message("state read failed while closing file", ios, iomsg)
    end if
  end subroutine read_text_file

  function io_status_message(context, io_status, io_message) result(message)
    character(len=*), intent(in) :: context
    integer, intent(in) :: io_status
    character(len=*), intent(in) :: io_message
    character(len=:), allocatable :: message
    character(len=32) :: status_text

    write(status_text, "(i0)") io_status
    if (len_trim(io_message) > 0) then
      message = context // " (iostat=" // trim(status_text) // ", message=" // trim(io_message) // ")"
    else
      message = context // " (iostat=" // trim(status_text) // ")"
    end if
  end function io_status_message

end module fgof_state

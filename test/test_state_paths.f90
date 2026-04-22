program test_state_paths
  use fgof_state, only : &
    FGOF_STATE_ERR_INVALID_OPTIONS, &
    FGOF_STATE_ERR_IO, &
    FGOF_STATE_ERR_NOT_FOUND, &
    FGOF_STATE_OK, &
    clear_state_options, &
    resolve_state_document, &
    state_path_for_name
  use fgof_state_types, only : state_document, state_options
  implicit none

  type(state_options) :: options
  type(state_document) :: document
  character(len=:), allocatable :: base_dir
  character(len=:), allocatable :: file_path
  integer :: unit

  base_dir = unique_root("document-layout")

  options = clear_state_options()
  options%root_dir = base_dir
  options%namespace = "demo-app"
  options%scope = "session"
  document = resolve_state_document("settings.json", options)
  if (document%error_code /= FGOF_STATE_OK) error stop "document resolution should succeed for valid names"
  if (document%present) error stop "newly resolved documents should start absent"
  if (document%root_path /= base_dir // "/demo-app/session") error stop "document resolution should expose the resolved root path"
  if (document%relative_path /= "settings.json") error stop "document resolution should expose the relative path"
  if (document%path /= base_dir // "/demo-app/session/settings.json") error stop "document resolution should expose the full document path"

  file_path = state_path_for_name(document%root_path, "settings.json")
  open(newunit=unit, file=file_path, status="replace", action="write")
  write(unit, "(a)") "hello"
  close(unit)

  document = resolve_state_document("settings.json", options)
  if (.not. document%present) error stop "document resolution should observe present files"

  document = resolve_state_document("", options)
  if (document%error_code /= FGOF_STATE_ERR_INVALID_OPTIONS) error stop "empty document names should be rejected"

  document = resolve_state_document("nested/file.txt", options)
  if (document%error_code /= FGOF_STATE_ERR_INVALID_OPTIONS) error stop "document names containing '/' should be rejected"

  document = resolve_state_document(".", options)
  if (document%error_code /= FGOF_STATE_ERR_INVALID_OPTIONS) error stop "document name '.' should be rejected"

  options = clear_state_options()
  options%create_root = .false.
  options%root_dir = unique_root("missing-document-root")
  options%namespace = "demo-app"
  document = resolve_state_document("settings.json", options)
  if (document%error_code /= FGOF_STATE_ERR_NOT_FOUND) error stop "missing roots should report not-found during document resolution"

  options = clear_state_options()
  options%root_dir = unique_root("directory-document")
  options%namespace = "demo-app"
  document = resolve_state_document("state.json", options)
  if (document%error_code /= FGOF_STATE_OK) error stop "document resolution should create a writable root for valid options"
  call execute_command_line("mkdir -p " // quote_path(document%path), wait=.true.)
  document = resolve_state_document("state.json", options)
  if (document%error_code /= FGOF_STATE_ERR_IO) error stop "document paths that are directories should be rejected"

contains

  function unique_root(label) result(path)
    character(len=*), intent(in) :: label
    character(len=:), allocatable :: path
    integer :: count
    character(len=32) :: count_text

    call system_clock(count)
    write(count_text, "(i0)") count
    path = "build/state-tests/" // label // "-" // trim(count_text)
  end function unique_root

  function quote_path(path_text) result(quoted)
    character(len=*), intent(in) :: path_text
    character(len=:), allocatable :: quoted

    quoted = "'" // path_text // "'"
  end function quote_path

end program test_state_paths

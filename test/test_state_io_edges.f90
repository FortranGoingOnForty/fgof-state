program test_state_io_edges
  use fgof_state, only : &
    FGOF_STATE_ERR_IO, &
    FGOF_STATE_ERR_NOT_FOUND, &
    clear_state_options, &
    load_state_text, &
    remove_state_document, &
    resolve_state_document, &
    save_state_text
  use fgof_state_posix, only : directory_exists_posix
  use fgof_state_types, only : state_document, state_options, state_text_result
  implicit none

  type(state_options) :: options
  type(state_document) :: document
  type(state_text_result) :: load_result
  character(len=:), allocatable :: base_dir

  options = clear_state_options()
  options%create_root = .false.
  options%root_dir = unique_root("missing-state-root")
  options%namespace = "demo-app"

  load_result = load_state_text("settings.json", options)
  if (load_result%error_code /= FGOF_STATE_ERR_NOT_FOUND) error stop "load_state_text should not create missing roots while probing"
  if (directory_exists_posix(options%root_dir)) error stop "load_state_text should not create missing explicit roots"

  document = remove_state_document("settings.json", options)
  if (document%error_code /= FGOF_STATE_ERR_NOT_FOUND) error stop "remove_state_document should not create missing roots while probing"
  if (directory_exists_posix(options%root_dir)) error stop "remove_state_document should not create missing explicit roots"

  base_dir = unique_root("directory-collision")
  options = clear_state_options()
  options%root_dir = base_dir
  options%namespace = "demo-app"
  document = resolve_state_document("state.json", options)
  if (document%error_code /= 0) error stop "document resolution should succeed for directory-collision setup"
  call execute_command_line("mkdir -p " // quote_path(document%path), wait=.true.)

  document = save_state_text("state.json", "hello", options)
  if (document%error_code /= FGOF_STATE_ERR_IO) error stop "save_state_text should reject directory collisions"

  load_result = load_state_text("state.json", options)
  if (load_result%error_code /= FGOF_STATE_ERR_IO) error stop "load_state_text should reject directory collisions"

  document = remove_state_document("state.json", options)
  if (document%error_code /= FGOF_STATE_ERR_IO) error stop "remove_state_document should reject directory collisions"

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

end program test_state_io_edges

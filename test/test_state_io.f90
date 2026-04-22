program test_state_io
  use fgof_state, only : &
    FGOF_STATE_OK, &
    clear_state_options, &
    load_state_text, &
    remove_state_document, &
    save_state_text
  use fgof_state_types, only : state_document, state_options, state_text_result
  implicit none

  type(state_options) :: options
  type(state_document) :: document
  type(state_text_result) :: load_result
  character(len=:), allocatable :: base_dir

  base_dir = unique_root("state-io")

  options = clear_state_options()
  options%root_dir = base_dir
  options%namespace = "demo-app"
  options%scope = "workspace"

  document = save_state_text("settings.json", "hello world", options)
  if (document%error_code /= FGOF_STATE_OK) error stop "save_state_text should succeed for valid options"
  if (.not. document%present) error stop "saved documents should be marked present"

  load_result = load_state_text("settings.json", options)
  if (load_result%error_code /= FGOF_STATE_OK) error stop "load_state_text should succeed for saved documents"
  if (.not. load_result%found) error stop "load_state_text should mark saved documents as found"
  if (load_result%text /= "hello world") error stop "load_state_text should return the saved text"

  document = remove_state_document("settings.json", options)
  if (document%error_code /= FGOF_STATE_OK) error stop "remove_state_document should succeed for saved documents"
  if (document%present) error stop "removed documents should be marked absent"

  load_result = load_state_text("settings.json", options)
  if (load_result%found) error stop "removed documents should no longer load as found"

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

end program test_state_io

program test_state_io
  use fgof_state, only : &
    FGOF_STATE_ERR_VERSION, &
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

  document = save_state_text("settings.json", "hello world", options, 3)
  if (document%error_code /= FGOF_STATE_OK) error stop "save_state_text should succeed for valid options"
  if (.not. document%present) error stop "saved documents should be marked present"
  if (document%version /= 3) error stop "save_state_text should preserve the requested version"

  load_result = load_state_text("settings.json", options)
  if (load_result%error_code /= FGOF_STATE_OK) error stop "load_state_text should succeed for saved documents"
  if (.not. load_result%found) error stop "load_state_text should mark saved documents as found"
  if (load_result%version_checked) error stop "load_state_text without expected_version should not report a version check"
  if (load_result%text /= "hello world") error stop "load_state_text should return the saved text"
  if (load_result%document%version /= 3) error stop "load_state_text should return the stored version"

  load_result = load_state_text("settings.json", options, expected_version=3)
  if (load_result%error_code /= FGOF_STATE_OK) error stop "matching expected_version should succeed"
  if (.not. load_result%version_checked) error stop "matching expected_version should report that a version check happened"
  if (.not. load_result%version_matched) error stop "matching expected_version should report a version match"

  load_result = load_state_text("settings.json", options, expected_version=2)
  if (load_result%error_code /= FGOF_STATE_ERR_VERSION) error stop "mismatched expected_version should report version error"
  if (.not. load_result%version_checked) error stop "mismatched expected_version should still report that a version check happened"
  if (load_result%version_matched) error stop "mismatched expected_version should report version mismatch"
  if (load_result%document%version /= 3) error stop "version mismatch should still surface the stored version"
  if (load_result%text /= "") error stop "version mismatch should not surface payload text yet"

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

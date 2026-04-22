program state_version_demo
  use fgof_state, only : FGOF_STATE_ERR_VERSION, clear_state_options, load_state_text, remove_state_document, save_state_text
  use fgof_state_types, only : state_document, state_options, state_text_result
  implicit none

  type(state_options) :: options
  type(state_document) :: document
  type(state_text_result) :: load_result

  options = clear_state_options()
  options%root_dir = "build/example-state-version"
  options%namespace = "demo-app"

  document = save_state_text("state.json", "hello", options, version=3)
  if (document%error_code /= 0) error stop "save failed in version demo"

  load_result = load_state_text("state.json", options, expected_version=2)
  if (load_result%error_code /= FGOF_STATE_ERR_VERSION) error stop "version demo should surface mismatch"

  print "(a,i0)", "stored_version=", load_result%document%version

  document = remove_state_document("state.json", options)
  if (document%error_code /= 0) error stop "cleanup failed in version demo"
end program state_version_demo

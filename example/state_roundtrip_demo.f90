program state_roundtrip_demo
  use fgof_state, only : clear_state_options, load_state_text, remove_state_document, save_state_text
  use fgof_state_types, only : state_document, state_options, state_text_result
  implicit none

  type(state_options) :: options
  type(state_document) :: document
  type(state_text_result) :: load_result

  options = clear_state_options()
  options%root_dir = "build/example-state-roundtrip"
  options%namespace = "demo-app"
  options%scope = "workspace"

  document = save_state_text("settings.json", "ready", options, version=2)
  if (document%error_code /= 0) error stop "save failed in roundtrip demo"

  load_result = load_state_text("settings.json", options, expected_version=2)
  if (load_result%error_code /= 0) error stop "load failed in roundtrip demo"

  print "(a)", trim(load_result%text)

  document = remove_state_document("settings.json", options)
  if (document%error_code /= 0) error stop "cleanup failed in roundtrip demo"
end program state_roundtrip_demo

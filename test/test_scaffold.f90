program test_scaffold
  use fgof_state, only : &
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
    state_error_name, &
    state_path_for_name, &
    state_relative_path_for_name
  use fgof_state_types, only : state_document, state_options, state_root, state_text_result
  implicit none

  type(state_options) :: options
  type(state_root) :: root
  type(state_document) :: document
  type(state_text_result) :: text_result
  character(len=:), allocatable :: path

  options = clear_state_options()
  if (.not. options%create_root) error stop "state options should create roots by default"
  if (allocated(options%root_dir)) error stop "state options should not allocate root_dir by default"
  if (allocated(options%namespace)) error stop "state options should not allocate namespace by default"
  if (allocated(options%scope)) error stop "state options should not allocate scope by default"

  root = clear_state_root()
  if (root%ready) error stop "state root should start unready"
  if (root%error_code /= FGOF_STATE_OK) error stop "state root should start ok"
  if (root%path /= "") error stop "state root should start with an empty path"
  if (root%error_message /= "") error stop "state root should start with an empty message"

  document = clear_state_document()
  if (document%present) error stop "state document should start absent"
  if (document%version /= 0) error stop "state document should start at version zero"
  if (document%error_code /= FGOF_STATE_OK) error stop "state document should start ok"
  if (document%name /= "") error stop "state document should start with an empty name"
  if (document%root_path /= "") error stop "state document should start with an empty root path"
  if (document%relative_path /= "") error stop "state document should start with an empty relative path"
  if (document%path /= "") error stop "state document should start with an empty path"
  if (document%error_message /= "") error stop "state document should start with an empty message"

  text_result = clear_state_text_result()
  if (text_result%found) error stop "state text result should start not found"
  if (.not. text_result%version_matched) error stop "state text result should start version-matched"
  if (text_result%expected_version /= 0) error stop "state text result should start with no expected version"
  if (text_result%error_code /= FGOF_STATE_OK) error stop "state text result should start ok"
  if (text_result%document%error_code /= FGOF_STATE_OK) error stop "state text result should carry a cleared document by default"
  if (text_result%text /= "") error stop "state text result should start with empty text"
  if (text_result%error_message /= "") error stop "state text result should start with an empty message"

  if (state_backend_name() /= "posix") error stop "backend helper should describe the current backend"
  if (state_error_name(FGOF_STATE_OK) /= "ok") error stop "error helper should map ok"
  if (state_error_name(FGOF_STATE_ERR_INVALID_OPTIONS) /= "invalid-options") error stop "error helper should map invalid options"
  if (state_error_name(FGOF_STATE_ERR_NOT_FOUND) /= "not-found") error stop "error helper should map not-found"
  if (state_error_name(FGOF_STATE_ERR_IO) /= "io") error stop "error helper should map io"
  if (state_error_name(FGOF_STATE_ERR_VERSION) /= "version") error stop "error helper should map version"
  if (state_error_name(FGOF_STATE_ERR_INTERNAL) /= "internal") error stop "error helper should map internal"
  if (state_error_name(999) /= "unknown") error stop "error helper should map unknown codes"

  path = state_relative_path_for_name("settings.json")
  if (path /= "settings.json") error stop "relative-path helper should preserve valid names"
  path = state_relative_path_for_name("nested/name")
  if (path /= "") error stop "relative-path helper should reject invalid names"
  path = state_path_for_name("/tmp/state-root", "settings.json")
  if (path /= "/tmp/state-root/settings.json") error stop "path helper should join root and name"

  root = ensure_state_root()
  if (.not. root%ready) error stop "default state root should be resolvable in normal environments"

  document = resolve_state_document("scaffold.txt")
  if (document%error_code /= FGOF_STATE_OK) error stop "default document resolution should succeed in normal environments"
  if (document%name /= "scaffold.txt") error stop "document resolution should preserve the requested name"
  if (document%root_path == "") error stop "document resolution should surface the resolved root path"
  if (document%relative_path /= "scaffold.txt") error stop "document resolution should surface the relative path"
  if (document%path == "") error stop "document resolution should surface the full path"

  text_result = load_state_text("scaffold.txt")
  if (.not. allocated(text_result%document%path)) error stop "load_state_text should return document metadata"
  document = save_state_text("scaffold.txt", "hello")
  if (document%error_code /= FGOF_STATE_OK) error stop "save_state_text should succeed for basic usage"
  if (document%version /= 1) error stop "default save_state_text version should be one"
  document = remove_state_document("scaffold.txt")
  if (document%error_code /= FGOF_STATE_OK) error stop "remove_state_document should succeed after save"
end program test_scaffold

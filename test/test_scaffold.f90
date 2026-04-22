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
    state_backend_name, &
    state_error_name
  use fgof_state_types, only : state_document, state_options, state_root
  implicit none

  type(state_options) :: options
  type(state_root) :: root
  type(state_document) :: document

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
  if (document%path /= "") error stop "state document should start with an empty path"
  if (document%error_message /= "") error stop "state document should start with an empty message"

  if (state_backend_name() /= "scaffold") error stop "backend helper should describe the current backend"
  if (state_error_name(FGOF_STATE_OK) /= "ok") error stop "error helper should map ok"
  if (state_error_name(FGOF_STATE_ERR_INVALID_OPTIONS) /= "invalid-options") error stop "error helper should map invalid options"
  if (state_error_name(FGOF_STATE_ERR_NOT_FOUND) /= "not-found") error stop "error helper should map not-found"
  if (state_error_name(FGOF_STATE_ERR_IO) /= "io") error stop "error helper should map io"
  if (state_error_name(FGOF_STATE_ERR_VERSION) /= "version") error stop "error helper should map version"
  if (state_error_name(FGOF_STATE_ERR_INTERNAL) /= "internal") error stop "error helper should map internal"
  if (state_error_name(999) /= "unknown") error stop "error helper should map unknown codes"
end program test_scaffold

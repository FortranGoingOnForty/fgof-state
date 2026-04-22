program test_state_root
  use fgof_state, only : &
    FGOF_STATE_ERR_INVALID_OPTIONS, &
    FGOF_STATE_ERR_NOT_FOUND, &
    FGOF_STATE_OK, &
    clear_state_options, &
    ensure_state_root
  use fgof_state_types, only : state_options, state_root
  implicit none

  type(state_options) :: options
  type(state_root) :: root
  character(len=:), allocatable :: base_dir
  character(len=:), allocatable :: missing_dir

  base_dir = unique_root("root-layout")

  options = clear_state_options()
  options%root_dir = base_dir
  options%namespace = "demo-app"
  options%scope = "workspace"
  root = ensure_state_root(options)
  if (.not. root%ready) error stop "ensure_state_root should create explicit state roots by default"
  if (root%path /= base_dir // "/demo-app/workspace") error stop "state roots should include namespace and scope layout"

  missing_dir = unique_root("missing-root")
  options = clear_state_options()
  options%create_root = .false.
  options%root_dir = missing_dir
  options%namespace = "demo-app"
  root = ensure_state_root(options)
  if (root%ready) error stop "create_root=false should not create missing explicit roots"
  if (root%error_code /= FGOF_STATE_ERR_NOT_FOUND) error stop "missing explicit roots should report not-found"

  options = clear_state_options()
  options%root_dir = base_dir
  options%namespace = "bad/name"
  root = ensure_state_root(options)
  if (root%error_code /= FGOF_STATE_ERR_INVALID_OPTIONS) error stop "namespace segments with '/' should be rejected"

  options = clear_state_options()
  options%root_dir = base_dir
  options%scope = ".."
  root = ensure_state_root(options)
  if (root%error_code /= FGOF_STATE_ERR_INVALID_OPTIONS) error stop "scope must reject '..'"

  options = clear_state_options()
  options%root_dir = base_dir
  root = ensure_state_root(options)
  if (root%error_code /= FGOF_STATE_OK) error stop "explicit roots without namespace should still succeed"
  if (root%path /= base_dir) error stop "explicit roots without namespace should use root_dir verbatim"

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

end program test_state_root

module fgof_state
  use fgof_state_types, only : &
    FGOF_STATE_ERR_INTERNAL, &
    FGOF_STATE_ERR_INVALID_OPTIONS, &
    FGOF_STATE_ERR_IO, &
    FGOF_STATE_ERR_NOT_FOUND, &
    FGOF_STATE_ERR_VERSION, &
    FGOF_STATE_OK, &
    state_document, &
    state_options, &
    state_root
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
    state_backend_name, &
    state_document, &
    state_error_name, &
    state_options, &
    state_root

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
    document%path = ""
    document%error_message = ""
  end function clear_state_document

  function state_backend_name() result(name)
    character(len=:), allocatable :: name

    name = "scaffold"
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

end module fgof_state

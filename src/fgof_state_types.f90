module fgof_state_types
  implicit none
  private

  integer, parameter, public :: FGOF_STATE_OK = 0
  integer, parameter, public :: FGOF_STATE_ERR_INVALID_OPTIONS = 10
  integer, parameter, public :: FGOF_STATE_ERR_NOT_FOUND = 20
  integer, parameter, public :: FGOF_STATE_ERR_IO = 30
  integer, parameter, public :: FGOF_STATE_ERR_VERSION = 40
  integer, parameter, public :: FGOF_STATE_ERR_INTERNAL = 99

  type, public :: state_options
    logical :: create_root = .true.
    character(len=:), allocatable :: root_dir
    character(len=:), allocatable :: namespace
    character(len=:), allocatable :: scope
  end type state_options

  type, public :: state_root
    logical :: ready = .false.
    integer :: error_code = FGOF_STATE_OK
    character(len=:), allocatable :: path
    character(len=:), allocatable :: error_message
  end type state_root

  type, public :: state_document
    logical :: present = .false.
    integer :: version = 0
    integer :: error_code = FGOF_STATE_OK
    character(len=:), allocatable :: name
    character(len=:), allocatable :: root_path
    character(len=:), allocatable :: relative_path
    character(len=:), allocatable :: path
    character(len=:), allocatable :: error_message
  end type state_document

  type, public :: state_text_result
    logical :: found = .false.
    logical :: version_matched = .true.
    integer :: expected_version = 0
    integer :: error_code = FGOF_STATE_OK
    type(state_document) :: document
    character(len=:), allocatable :: text
    character(len=:), allocatable :: error_message
  end type state_text_result

end module fgof_state_types

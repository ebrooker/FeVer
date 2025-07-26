module FeVer
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, FeVer!"
  end subroutine say_hello
end module FeVer

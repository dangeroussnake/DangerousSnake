module util
contains
    function to_string(int, length) result(str)
        integer, intent(in) :: int
        integer, intent(in) :: length
        character(len=length) :: str
        character(len=13) :: fmt
        write(fmt, "(A,I10,A)") "(I", length, ")"
        write(str, fmt) int
        return
    end function to_string
end module util

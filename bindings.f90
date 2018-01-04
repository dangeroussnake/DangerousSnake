module bindings 
    
    interface
        subroutine usleep(useconds) bind(C)
            use iso_c_binding
            implicit none
            integer(c_int32_t), value :: useconds
        end subroutine
    end interface
    
end module bindings
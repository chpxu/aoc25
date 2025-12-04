program q1
    ! comment
    use, intrinsic :: iso_fortran_env, only : iostat_end
    implicit none
integer :: io, error
character(len=:), allocatable :: all_ids
character(len=1000) :: line
integer :: i, p, start, id_int1, id_int2
open(newunit=io, file="input.txt", status="old", action="read")
read(io, '(A)', iostat=error) line
close(io)

write(*,*) line


end program q1
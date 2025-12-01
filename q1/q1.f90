program q1
    ! comment
    use, intrinsic :: iso_fortran_env, only : iostat_end
    implicit none
integer :: io, error
integer :: num_lines, i
character(len=4) :: rot, line, step_size_str
character :: direction
integer :: pointer_loc, num_zero, step_size, dis

open(newunit=io, file="input.txt", status="old", action="read")

pointer_loc = 50 ! dial range is [0,99]
num_zero = 0
num_lines = 4510
do i = 1, num_lines
    read(io, *, iostat=error) rot
    ! print *, rot
    line = trim(adjustl(rot))
    direction = line(1:1)
    step_size_str = line(2:len(line))
    read(step_size_str, *) step_size
    ! Know that every 100 rotations gets you back to the same place
    ! so if we didnt start on 0, we won't get 0 and so we can just modulo 100
    !print *, step_size / 100
    step_size = modulo(step_size, 100)
    !if (pointer_loc .eq. 0) num_zero = num_zero + (step_size / 100)
    !write(*,*) num_zero
    
    if (i .lt. 20) then 
        print *, "num_zero at begin of iteration", num_zero
        print *, "starting pointer loc at", i, pointer_loc
        print *, "modulo step size", line,step_size
        print *, ""
    end if
    if (direction == "L") then
        if (pointer_loc < step_size) then ! e.g. pointer_loc = 69, rot =  L70
            pointer_loc = 100 - (step_size - pointer_loc)
            if (pointer_loc == 0) num_zero = num_zero + 1
        else
            pointer_loc = pointer_loc - step_size ! e.g. 69 + L30 = 39
        end if
    end if
    dis = pointer_loc + step_size - 100
    if (direction == "R") then
        if (step_size + pointer_loc > 99) then ! e.g. 50 + R746 = 96
            pointer_loc = dis
            if ((pointer_loc .eq. 0))  num_zero = num_zero + 1
        else
            pointer_loc = pointer_loc + step_size !e.g. 50 + R30 = 80
        end if
    end if
    if (pointer_loc == 0) then 
        num_zero = num_zero  +1 ! case where it's just 0 anyways
    end if

end do
write(*,*) num_zero
close(io)

end program q1
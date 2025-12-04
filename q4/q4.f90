program q4
implicit none
integer :: nlines
!integer, allocatable :: paper_array(:)
integer, dimension(100000) :: paper_array
integer :: io, error, i, line_char_index, line_length, adjacent_paper_count
character(len=138) :: line, trim_line
call get_file_lines
open(newunit=io, file="input.txt", status="old", action="read")
!allocate(paper_array(line_length * nlines))
do i = 1, nlines
    read(io, *, iostat=error) line
    !print *, line
    trim_line = trim(adjustl(line))
    line_length = len(trim_line)
    do line_char_index = 1, line_length
        if (trim_line(line_char_index:line_char_index) .eq. ".") paper_array(i*line_length + line_char_index) = 0
        if (trim_line(line_char_index:line_char_index) .eq. "@") paper_array(i*line_length + line_char_index) = 1
    end do
end do
close(io)
do i = 1, nlines * line_length
  if (paper_array(i) .eq. 0) continue ! 0 is not a paper roll
  adjacent_paper_count = 0

  
end do
! now for a given 1, at location i*len + disp, must look at u*len + disp \pm 1 (l/r), (i \pm 1)*len + disp \pm 1 (top and bottom, and corners)
!deallocate(paper_array)
contains
subroutine get_file_lines
nlines =0
open (1, file = 'input.txt')
do
  read(1,*,iostat=io) line
  line_length = len(line)
  if (io/=0) EXIT
  nlines = nlines + 1
end do
close(1)
end subroutine

end program q4

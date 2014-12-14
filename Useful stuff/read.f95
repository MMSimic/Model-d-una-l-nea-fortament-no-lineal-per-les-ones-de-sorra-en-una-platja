program io2
        !illustrates  writing arrays to files
        implicit none
        real :: num
        integer :: i
            open(12,file='output\output.txt')
        do
            read(12,*,end=10) num
        	print *, num
        end do
        10 close(1)
end program io2
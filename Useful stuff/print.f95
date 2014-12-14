program io2
        !illustrates  writing arrays to files
        implicit none
        real :: num
        integer :: i
            open(12,file='output\output.txt')
        do i = 1,100
          	num=i/3
            write(12,*) num
        end do
        print *, 'finished!'
end program io2
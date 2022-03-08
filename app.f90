program io
    implicit none
    
    integer :: lines,index
    character(len = 20) :: file_name
    real, dimension (:), allocatable :: data_array

    print *, "Enter the file name :"
    read *, file_name
    
    lines = count_lines(file_name)

    allocate(data_array(lines))

    do index = 1, lines
        open(10,file=file_name,status="old")
        read(10,'(A)') data_array(index)
    end do

    call quicksort(data_array)

      ! output data into a file 
    open(11, file = 'HW6Out.txt', status = 'new')  
    do index=1,size(data_array,1) 
        write(11,*) data_array(index)
    end do 
    
    close(10)
    close(11)

    deallocate(data_array)

    !my recursive 
    contains
        
    recursive subroutine quicksort(a)
        implicit none
        real :: a(:)
        real x, t
        integer :: first = 1, last
        integer i, j

        last = size(a, 1)
        x = a( (first+last) / 2 )
        i = first
        j = last
        
        do
            do while (a(i) < x)
                i=i+1
            end do
            do while (x < a(j))
                j=j-1
            end do
            if (i >= j) exit
            t = a(i);  a(i) = a(j);  a(j) = t
            i=i+1
            j=j-1
        end do
        
        if (first < i - 1) call quicksort(a(first : i - 1))
        if (j + 1 < last)  call quicksort(a(j + 1 : last))
        end subroutine quicksort

    ! function to count lines in a file
    ! contains
    
    function count_lines(filename) result(nlines)
        implicit none
        character(len=*)    :: filename
        integer             :: nlines 
        integer             :: io

        open(10,file=filename, iostat=io, status='old')
        if (io/=0) stop 'Cannot open file! '

        nlines = 0
        do
            read(10,*,iostat=io)
            if (io/=0) exit
            nlines = nlines + 1
        end do
        close(10)
    end function count_lines

end program io    
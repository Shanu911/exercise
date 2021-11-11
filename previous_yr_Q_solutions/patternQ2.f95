program q1
    implicit none
    integer :: i,j,m,n=5
    do i=1,n+1
        do j=1,n+1
            m = n-(i-j)
            if (j<=i) then
                write(*,"(i6)",advance="no") m
            else
                exit
            end if
        end do
        print *,""
    end do
end program

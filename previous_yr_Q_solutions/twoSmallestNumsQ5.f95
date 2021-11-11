program q1
    implicit none
    integer :: i,j,n=6
    real, dimension(6) :: datas
    real :: temp
    read *, (datas(i), i=1,6)

    do i=1,n-1
        do j=i,n
            if(datas(j)<datas(i)) then
                temp = datas(i)
                datas(i) = datas(j)
                datas(j) = temp
            end if
        end do
    end do
    print *, datas(1), datas(2)
end program

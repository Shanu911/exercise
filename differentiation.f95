program diff
    implicit none
    real :: f, actf
    real :: x(10),df, h, ordf
    integer :: i

    x = (/(0.2*i,i=0,9)/)  ! values of x= 0,0.2,...1.8

    do i = 1,9
        h = x(i+1)- x(i)
        df = (f(x(i+1))-f(x(i)))/h

        write (*,"(2(a,f3.1,a,f8.3))") "f'(",x(i),") = ",df &
                ,"  actual_f'(",x(i),") = ",actf(x(i))
    end do

end program

real function f(x)
    real, intent(in) :: x
    f = sin(x) - 3*cos(exp(x))
    return
end function

real function actf(x)
    implicit none
    real, intent(in) :: x
    actf = cos(x) + 3*sin(exp(x))*exp(x)
end function

program integrate
    implicit none
    real :: p,y

    real, dimension(6) :: x, fx
    integer :: n,i,x_in

    x = (/5, 10, 15, 20, 25, 30/)
    fx = (/114.78,171.42, 218.31, 301.9, 425.0, 723.0/)

    write (*,"(A,3x,A)")"Original points", "Interpolated points"
    write (*,"(2(x,A,7x,A,5x))") "x","f(x)", "x","f(x)"
    do i = 1,5
        write (*,"(f4.1,f10.2)") x(i),fx(i)
        do x_in = int(x(i)+1),int(x(i+1)-1)
            p = (x_in-x(i))/(x(i+1)-x(i))
            y = fx(i)*(1.0-p) + fx(i+1)*p
            write (*,"(16x,i4,3x,f9.5)")x_in,y
        end do
    end do
    write (*,"(f4.1,f10.2)") x(i),fx(i)
end program

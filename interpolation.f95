program integrate
    implicit none
    real :: lagrangeIntp, lag_i
    real :: quadIntp, q_i
    real :: linearIntp, lin_i

    real :: x_in
    real,allocatable :: x(:), fx(:)
    integer :: n,i

    print *, "Enter total number of points:"
    read *, n
    allocate(x(n)); allocate(fx(n))
    print *, "Enter the values of x"
    read *, (x(i), i=1,n)
    print *, "Enter the values of fx"
    read *, (fx(i), i=1,n)
    print *,"Enter the value of interpolation"
    read *, x_in

    lag_i = lagrangeIntp(x_in, x, fx, n)
    q_i = quadIntp(x_in, x, fx, n)
    lin_i = linearIntp(x_in, x, fx, n)

    write (*,"(3(2/,A,/,A,f10.5))") "Using Lagrange Interpolation method"&
    ," f(x_in)=" ,lag_i,  "Using Quadratic Interpolation method"," f(x_in)="&
    ,q_i,  "Using Linear Interpolation method"," f(x_in)=",lin_i

    deallocate(x);deallocate(fx)
end program

!------------------ Lagrange Interpolation method

real function lagrangeIntp(x_in, x, fx, n)
implicit none
integer :: i,j
real :: l,y=0
integer, intent(in) :: n
real, intent(IN) :: x_in, x(n), fx(n)
do i=1,n
    l = 1
    do j = 1,n
        if(i /= j) then
            l = l*(x_in-x(j))/(x(i)-x(j))
        end if
    end do
    y = y + l*fx(i)
end do
lagrangeIntp = y
end function

!------------------ Quadratic Interpolation method

real function quadIntp(x_in, x, fx, n)
implicit none
integer :: i,j
real :: l,y=0
integer, intent(in) :: n
real, intent(IN) :: x_in, x(n), fx(n)
do i=1,n
    l = 1
    do j = 1,n
        if(i/=j) then
            l = l*(x_in-x(j))/(x(i)-x(j))
        end if
    end do
    y = y + l*fx(i)
end do
quadIntp = y
end function


!------------------ Linear Interpolation method

real function linearIntp(x_in, x, fx, n)
implicit none
integer :: i,c,mn,mx
real :: p
integer, intent(in) :: n
real, intent(IN) :: x_in, x(n), fx(n)

do i=1,n
    if(abs(x_in - x(1))>abs(x_in - x(i))) then
       c = i
    end if
end do
if(x(c)>x_in) then
    mx = c
    mn = c-1
else
    mx = c+1
    mn = c
end if
p = (x_in-x(mn))/(x(mx)-x(mn))
linearIntp = fx(mn)*(1.0-p) + fx(mx)*p
end function

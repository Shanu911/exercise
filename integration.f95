program integration
    implicit none
    real :: Simpson
    real :: Trapez

    real :: a,b
    integer :: n
    print *,"Enter the values of a,b,n"
    read *, a,b,n
    print *, "Value of the integral using Simpson method I=",Simpson(a,b,n)
    print *, "Value of the integral using Trapezoidal method I=",Trapez(a,b,n)

end program

!-----------------main function ---------
real function f(x)
real, intent(in) :: x
f = exp(x*tan(x))
end function

! --------------- Simpson method----------
real function Simpson(a,b,n)
real, intent(in) :: a,b
integer, intent(in) :: n
integer :: i
real :: h,s
h = (b-a)/real(n)
s = f(a) + f(b)
do i=2,n-1
    if(mod(i,2)==0) then
        s = s+2*f(a+i*h)
    else
        s = s+4*f(a+i*h)
    end if
end do
Simpson = h*s/3.0
end function

!---------- Trapezoidal method-------------
real function Trapez(a,b,n)
real :: f
real, intent(in) :: a,b
integer, intent(in) :: n
integer :: i
real :: h,s
h = (b-a)/real(n)
s = f(a) + f(b)
do i=2,n-1
    s = s+2*f(a+i*h)
end do
Trapez = h*s/2.0
end function

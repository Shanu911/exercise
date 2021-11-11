program q2
    implicit none
    integer :: i,n
    real :: term=1.0, s = 1.0, x
    read *, x ,n

    !----e^(x)-----------
    do i=1,n
       term = term*x/float(i)
       s = s + term
    end do
    print *,s

    !----e^(-x)------------
    s = 1.0
    term = 1.0
    do i=1,n
       term = -term*x/float(i)
       s = s + term
    end do
    print *,s
end program

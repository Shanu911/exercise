program apperent_res
    implicit none

    real, allocatable :: roh(:),h(:),freq(:)
    integer :: i,n,m,j
    real::omega,pi, mu, roh_a, phi
    complex :: c_i = (0.0, 1.0),k,T,S, term
    complex, allocatable :: z(:)

    pi = 4*atan(1.0)
    mu = 4.0*pi* 10.0**(-7.0)
    open(unit=56, file = "frequency.txt", status="old",     &
        position="rewind", action="read")
    m = 10
    allocate(freq(m))
    read (56,*) (freq(i),i=1,m)


do
    write (*,"(a)",advance="no") "n="
    read *, n
    if(n==0) exit
    write (*,*) "roh    h"
    allocate(roh(n));allocate(h(n)); allocate(z(n))
    read (*,*) (roh(i), h(i), i=1,n)

    write (*,*) "freq(j)    roh_a     phi"
    do j = 1,m
        omega = 2.0*pi*freq(j)
        k = sqrt(c_i*omega*mu)

        z(n) = k*sqrt(roh(n))

        do i=(n-1),1,-1
            term = tanh(k*h(i)/sqrt(roh(i)))
            T = term*k*sqrt(roh(i))
            S = term/(k*sqrt(roh(i)))

            z(i) = (z(i+1) + T)/(1.0 + S*z(i+1))
        end do
        roh_a = z(1)**2 / (omega*mu)
        phi = atan(aimag(z(1))/real(z(1)))*180.0/pi
        write (*,"(4(f8.3,5x))") freq(j), z(1),roh_a, phi
    end do
end do
end program

































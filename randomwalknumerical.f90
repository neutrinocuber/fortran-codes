program randwalk1
implicit none
integer:: i,n,m,j,npm,nmm
integer*8::fan,fanpm,fanmm
real*8  ,dimension(:),allocatable::p,d
10 print*,"value of n"
read*,n
if (mod(n,2)==1) go to 10
m=n/2
allocate(p(n+1),d(n+1))
do i=1,n+1
        d(i)=2*(i-1)-n
end do
fan=n

do i=1,n-1
        fan=i*fan
end do
do i=m+1,n+1
        fanpm=1
        fanmm=1
        npm=(n+d(i))/2
        nmm=(n-d(i))/2
        do j=1,npm
                fanpm=j*fanpm
        end do
        do j=1,nmm
                fanmm=fanmm*j
        end do
        p(i)=fan/(fanpm*fanmm)
end do
do i=1,m
        p(i)=p(n+2-i)
end do
p=p/2**n
do i=1,n+1
        print*,d(i),p(i)
end do
open(unit=10,file="wow.dat",status="replace")
do i=1,n+1
        write(10,*)d(i),p(i)
end do
close(10)
end program randwalk1

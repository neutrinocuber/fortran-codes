program pic1
implicit none
real,allocatable,dimension(:)::x,t,xpc
real,allocatable,dimension(:)::h,erx,erx2
real::hh,t1,x1,ax,xp,fun
integer::i,j,n
print*,"enter number of points"
read*,n
allocate(t(n),x(n),h(11),erx(11),erx2(11),xpc(n))
print*,"enter range of t"
read*,t(1),t(n)
print*,"enter value of x at initial t"
read*,x(1)
xpc(1)=x(1)
hh=abs((t(1)-t(n)))/(n-1)
print*,"h",hh
do i=2,n
	t(i)=t(i-1)+hh
end do
h(11)=1
do i=10,1,-1
	h(i)=h(i+1)/2
end do
print*,h
do i=2,n
	xpc(i)=0
	do j=1,10
		!print*,xpc(i)
		xpc(i)=(fun(xpc(i),t(i),hh)+fun(xpc(i-1),t(i-1),hh)+xpc(i-1)-xpc(i))/2
		!print*,xpc(i)
	end do
end do
ax=-t(n)-1+2*exp(t(n))
print*,h
print*,"."
do i=1,11
	x1=x(1)
	xp=0
	t1=t(1)
	do while (t1<t(n))
		do j=1,10
		xp=(fun(x1,t1,h(i))+fun(xp,t1+h(i),h(i))+x1-xp)/2
		end do
		x1=xp
		t1=t1+h(i)
		!print*,xp
	end do
	!print*,xp
	erx(i)=abs(ax-xp)*100/ax
	print*,erx(i),h(i)
end do	
open(unit=16,file="pic1.dat",status="replace")
do i=1,n
	write(16,*)t(i),xpc(i)
end do
close(16)
open(unit=10,file="pic1e.dat",status="replace")
do i=1,11
	write(10,*)h(i),erx(i)
end do
close(10)
end program pic1

real function fun(x0,t0,hh)
	real::x0,l,t0
	fun=x0+hh*(t0+x0)
return
end function

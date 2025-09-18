program rk2
implicit none
real,allocatable,dimension(:)::x,t,xpc
real,allocatable,dimension(:)::h,erx
real::hh,t1,x1,ax,xp,fun,k1,k2,k1h,k2h
integer::i,j,n
print*,"enter number of points"
read*,n
allocate(t(n),h(11),erx(11),xpc(n),x(n))
print*,"enter range of t"
read*,t(1),t(n)
print*,"enter value of x at initial t"
read*,xpc(1)
x(1)=xpc(1)
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
	k1=fun(xpc(i-1),t(i-1),hh)
	k2=fun(xpc(i-1)+k1/2,t(i-1)+hh/2,hh)
	xpc(i)=xpc(i-1)+k2
	k1h=fun(x(i-1),t(i-1),hh)
	k2h=fun(x(i-1)+k1h,t(i-1)+hh,hh)
	x(i)=x(i-1)+(k1h+k2h)/2
end do
ax=t(n)-2+3*exp(-t(n)/2)
print*,h
print*,"."
do i=1,11
	x1=xpc(1)
	t1=t(1)
	do while (t1<t(n))
		k1=fun(x1,t1,h(i))
		k2=fun(x1+k1/2,t1+hh/2,h(i))
		x1=x1+k2
		t1=t1+h(i)
		!print*,xp
	end do
	!print*,xp
	erx(i)=abs(ax-x1)*100/ax
	print*,erx(i),h(i)
end do	
open(unit=16,file="rk21.dat",status="replace")
do i=1,n
	write(16,*)t(i),xpc(i),x(i)
end do
close(16)
open(unit=10,file="rk21e.dat",status="replace")
do i=1,11
	write(10,*)h(i),erx(i)
end do
close(10)
end program rk2

real function fun(x0,t0,hh)
	real::x0,l,t0
	fun=hh*(t0-x0)/2
return
end function

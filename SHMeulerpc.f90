program eul2
implicit none
real,allocatable,dimension(:)::x,v,t,xpc,vpc
real,allocatable,dimension(:)::h,erx,erx2,erv,erv2
real::hh,t1,x1,v1,ax,xp,av,vp,k,m,w,fun
integer::i,j,n
print*,"enter number of points"
read*,n
allocate(t(n),x(n),v(n),h(11),erx(11),erx2(11),erv(11),erv2(11),vpc(n),xpc(n))
print*,"enter range of t"
read*,t(1),t(n)
print*,"enter value of k and m in SI unit"
read*,k,m
w=(k/m)**0.5
print*,"enter value of x at initial t"
read*,x(1)
print*,"enter value of v at initial t"
read*,v(1)
xpc(1)=x(1)
vpc(1)=v(1)
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
	x(i)=fun(x(i-1),v(i-1),hh,1.0)
	v(i)=fun(v(i-1),x(i-1),hh,-k/m)
	xpc(i)=x(i)
	vpc(i)=v(i)
	do j=1,2
		!print*,xpc(i)
		xpc(i)=(fun(xpc(i),vpc(i),hh,1.0)+fun(xpc(i-1),vpc(i-1),hh,1.0)+xpc(i-1)-xpc(i))/2
		vpc(i)=(fun(vpc(i),xpc(i),hh,-k/m)+fun(vpc(i-1),xpc(i-1),hh,-k/m)+vpc(i-1)-vpc(i))/2
		!print*,xpc(i)
	end do
end do
ax=cos(w*t(n))
av=-w*sin(w*t(n))
print*,h
print*,"."
do i=1,11
	x1=x(1)
	v1=v(1)
	xp=x(1)
	vp=v(1)
	t1=t(1)
	do while (t1<t(n))
		x1=fun(x1,v1,h(i),1.0)
		v1=fun(v1,x1,h(i),-k/m)
		xp=(fun(x1,v1,hh,1.0)+fun(xp,vp,hh,1.0)+xp-x1)/2
		vp=(fun(v1,x1,hh,-k/m)+fun(vp,xp,hh,-k/m)+vp-v1)/2
		t1=t1+h(i)
	end do
	erx(i)=abs(ax-x1)*100/ax
	erx2(i)=abs(ax-xp)*100/ax
	print*,erx(i),erx2(i),t1,h(i)
	erv(i)=abs((av-v1)*100/av)
	erv2(i)=abs((av-vp)*100/av)
	print*,erv(i),erv2(i)
end do	
open(unit=16,file="eul2x.dat",status="replace")
do i=1,n
	write(16,*)t(i),x(i),xpc(i)
end do
close(16)
open(unit=30,file="eul2v.dat",status="replace")
do i=1,n
	write(30,*)t(i),v(i),vpc(i)
end do
close(30)
open(unit=10,file="eul2ex.dat",status="replace")
do i=1,11
	write(10,*)h(i),erx(i),erx2(i)
end do
close(10)
open(unit=29,file="eul2ev.dat",status="replace")
do i=1,11
	write(29,*)h(i),erv(i),erv2(i)
end do
close(29)
end program eul2

real function fun(x0,v0,hh,l)
	real::x0,v0,l
	fun=x0+hh*l*v0
return
end function

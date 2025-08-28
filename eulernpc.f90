program eul1
implicit none
real,allocatable,dimension(:)::x,x2,y,y2,ypc
real,allocatable,dimension(:)::h,er
real::hh,x1,y1,fun,ay
integer::i,j,n
print*,"enter number of points"
read*,n
allocate(x(n),y(n),y2(5*n),h(11),er(11),x2(5*n),ypc(n))
print*,"enter range of x"
read*,x(1),x(n)
print*,"enter value of y at initial x"
read*,y(1)
ypc(1)=y(1)
hh=abs((x(1)-x(n)))/(n-1)
print*,"h",hh
do i=2,n
	x(i)=x(i-1)+hh
	y(i)=fun(x(i-1),y(i-1),hh)
	ypc(i)=(fun(x(i),y(i),hh)+fun(x(i-1),ypc(i-1),hh)+ypc(i-1)-y(i))/2
end do
hh=(abs(x(1)-x(n)))/(5*n-1)
print*,"h",hh
x2(1)=x(1)
y2(1)=y(1)
do i=2,5*n
	x2(i)=x2(i-1)+hh
	y2(i)=fun(x2(i-1),y2(i-1),hh)
end do
h(11)=1
do i=10,1,-1
	h(i)=h(i+1)/2
end do
print*,h
ay=(1+(x(n)**4)/4)*exp(-2*x(n))
do i=1,11
	y1=y(1)
	x1=x(1)
	do while (x1<x(n)) 
		y1=fun(x1,y1,h(i))
		x1=x1+h(i)
	end do
	er(i)=abs(ay-y1)*100/ay
	print*,er(i)
end do	
open(unit=16,file="eul11.dat",status="replace")
do i=1,n
	write(16,*)x(i),y(i),ypc(i)
end do
close(16)
open(unit=30,file="eul12.dat",status="replace")
do i=1,5*n
	write(30,*)x2(i),y2(i)
end do
close(30)
open(unit=10,file="eul1e.dat",status="replace")
do i=1,11
	write(10,*)h(i),er(i)
end do
close(10)
end program eul1

real function fun(x0,y0,hh)
	real::x0,y0
	fun=(1-2*hh)*y0 + hh*(x0**3)*exp(-2*x0)
return
end function

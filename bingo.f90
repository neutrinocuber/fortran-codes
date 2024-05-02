function cl(fn,k) result(nl)
character(len=k)::fn
integer::nl
open(10,file=fn,status="old")
nl=0
do
	read(10,*,iostat=io)
	nl=nl+1
	if (io/=0) exit
end do
close(10)
end function cl

program bingo
implicit none
real,dimension(:),allocatable::s
real::b
integer::i,n,cl,j,m,k
!print*,"enter no. of elements"
!read*,i
!allocate(s(i))
!print*,"enter elements, can be real"
!read*,s
k=len("array.txt")
open(1,file="array.txt",status="old")
i=cl("array.txt",k) -1
print*,i
allocate(s(i))
do n=1,i
	read(1,*)s(n)
end do
!print*,s
m=1
j=1
do 10
	print*,m
	b=s(m)
	do 20 n=m,i
		if (s(n)<b) b=s(n)
	20 continue
	do 30 n=m,i
		if (s(n)==b) then; s(n)=s(j); s(j)=b; j=j+1; end if
	30 continue
	print*,j
	m=j+1
	if (m>=i) exit
10 continue
print*,s
end program bingo
	 





program base1base2
implicit none
integer::b1,b2,x,y,i,j
real::a,k,r,c
print*,"enter initial and final base"
read*,b1,b2
print*,"enter no."
read*,x
a=x
i=0
k=0
y=0
j=0
5 continue
a=a/10
!print*,a
r=(a-floor(a))*10
r=nint(r)
k=k+r*b1**i
i=i+1
a=floor(a)
!print*,a,r
if (a>0) go to 5
print*,i,k
10 continue
k=k/b2
c=(k-floor(k))*b2
c=nint(c)
y=y+c*10**j
j=j+1
k=floor(k)
!print*,k,c
if (k>0) go to 10
!print*,j,y
print*,"the new base no. is"
print*,y
end program base1base2



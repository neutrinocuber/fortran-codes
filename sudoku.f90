program sudokusolver
integer::a(9,9),i,j,k,l,m,n,p,q,cc,cr,cb
print*,"enter sudoku nos. row wise,enter 0 for gaps"

do p=1,9
read*,(a(p,q),q=1,9)
end do

print*,"sudoku received"

do p=1,9
do q=1,9
if (a(p,q)>0) a(p,q)=a(p,q)+10
end do
end do
!print*,a

i=0

10 continue
i=i+1
!print*,i
j=0
if (i==10) go to 1

20 continue
j=j+1
!print*,j
if (j==10) go to 10

30 continue
if (a(i,j)<10) then

2 continue
a(i,j)=a(i,j)+1
!print*,a(i,j)
if (a(i,j)==10) go to 3

do 15 k=1,9
if (k==j) go to 150 
if (a(i,k)==a(i,j)) go to 2
if (a(i,k)-10==a(i,j)) go to 2
150 continue
!print*,150
15 continue
!print*,15

do 25 k=1,9
if (k==i) go to 250
if (a(k,j)==a(i,j)) go to 2
if (a(k,j)-10==a(i,j)) go to 2
250 continue
25 continue
!print*,25

m=1
if (i>3) m=4
if (i>6) m=7
!print*,"m",m
n=1
if (j>3) n=4
if (j>6) n=7
!print*,"n",n

do 35 k=0,2
if (m+k==i) go to 350
!print*,m,k
do 45 l=0,2
if (n+l==j) go to 450
!print*,n,l
if (a(m+k,n+l)==a(i,j)) go to 2
if (a(m+k,n+l)-10==a(i,j)) go to 2
450 continue
45 continue
350 continue
35 continue
!print*,35

go to 20

else
go to 20

end if

3 continue
a(i,j)=0

5 continue
j=j-1
if (j==0) then
j=9
i=i-1
if (a(i,j)>10) go to 5
go to 30

else
if (a(i,j)>10) go to 5
go to 30

end if

1 continue

do p=1,9
do q=1,9
if (a(p,q)>10) a(p,q)=a(p,q)-10
end do
end do

print*,"solved sudoku is"

do p=1,9
print*,(a(p,q),q=1,9)
end do
4 continue
end program sudokusolver

program randwalk1sim
implicit none
integer,dimension(:,:),allocatable::mv,mv2
integer,dimension(:),allocatable::d,x
real,dimension(:,:),allocatable::rd
real*8,dimension(:),allocatable::pb,rms,sq
integer::i,j,m,n,rn,swp,tk
10 print*,"give value of n (even)"
read*,n
if (mod(n,2)==1) go to 10
m=n/2
print*,"give no. of runs"
read*,rn
allocate(mv(n,rn),rd(n,rn),d(n+1),pb(n+1),sq(n),rms(n),x(n))
call random_number(rd)
mv=2*(floor(2*rd))-1
!do i=1,rn
!print*,mv(i,:)
!end do
do i=1,n+1
        d(i)=2*(i-1)-n
end do
!print*,res
x(1)=1
do i=1,rn
        rms(1)=rms(1)+(mv(1,i)**2)
end do
rms(1)=sqrt(rms(1)/rn)
do j=2,n
        do i=1,rn
                mv(j,i)=mv(j,i)+mv(j-1,i)
                sq(j)=sq(j)+(mv(j,i)**2)
                !print*,i,j
        end do
        rms(j)=sqrt(sq(j)/rn)
        x(j)=j
        !print*,j
end do
do i=1,n+1
        tk=0
        do j=1,rn
                if (mv(n,j)==d(i)) tk=tk+1
                !print*,i,j
        end do
        pb(i)=tk
        !print*,i
end do
pb=pb/rn
open(unit=20,file="wow2.dat",status="replace")
do i=1,n+1
        write(20,*)d(i),pb(i)
end do
!print*,"wow2"
open(unit=30,file="wow3.dat",status="replace")
do i=1,n
        write(30,*)x(i),rms(i)
        !print*,i
end do
print*,"wow3"
end program randwalk1sim

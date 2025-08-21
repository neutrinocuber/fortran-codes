program metrovel
implicit none
real::w,p,h,en1,en2,vmax,t,vmin,sg
real*8,dimension(:,:),allocatable::v
real,dimension(:),allocatable::eta,en,bs,be
real*8,dimension(:),allocatable::pbs,pbe
integer::n,m,i,j,bns,bne,ex
print*,"enter number of particles"
read*,n
print*,"number of time steps"
read*,m
print*,"enter temperature"
read*,t
print*,"enter no. of speed bins"
read*,bns
print*,"enter no. of energy bins"
read*,bne
allocate(v(n,m),eta(n),en(m),bs(bns),be(bne))
print*,"enter h"
read*,h
print*,"enter max initial vel"
read*,p
call random_number(eta)
v(:,1)=p*(2*eta(:)-1)
do i=1,n
        en1=en1+(v(i,1)**2)
end do
!print*,v(:,1)
en1=en1/2
j=1
en(1)=en1
be(1)=en1/bne
!print*,"bdw"
ex=0
do while (j<m .and. ex<10000)
        en2=0
        call random_number(eta)
        !print*,"eta"
        !print*,eta
        eta(:)=v(:,j)+h*(2*eta(:)-1)
        do i=1,n
                en2=en2+(eta(i)**2)
        end do
        en2=en2/2
        !print*,en1,en2
        !print*,"vel"
        !print*,eta
 call random_number(w)
        if (en1-en2>=0 .and. w<=exp((en2-en1)/t)) then
                j=j+1
                v(:,j)=eta(:)
                en(j)=en2
                en1=en2
                ex=0
        else
                ex=ex+1
        end if
!print*,j
end do
print*,j,ex
print*,m
if (j>1 .and. j<m) m=j
print*,m
!v(:,:)=abs(v(:,:))
!print*,"adw"
do i=2,bne
        be(i)=be(1)*i
end do
allocate(pbs(bns),pbe(bne))
vmax=v(1,m)
vmin=v(1,m)
do i=2,n
        if (vmax<v(i,m)) vmax=v(i,m)
        if (vmin>v(i,m)) vmin=v(i,m)
end do
print*,vmax,vmin
sg=(abs(vmax)+abs(vmin))/bns
print*,sg
bs(1)=vmin
do i=2,bns
    bs(i)=bs(i-1)+sg
        print*,bs(i)
end do
do i=1,n
    do j=1,bns
        if (bs(j)>=v(i,m)) then
            pbs(j)=pbs(j)+1
            exit
        end if
    end do
end do
print*,"pbs"
pbs(:)=pbs(:)/n
!do i=1,n
!       print*,v(i,m)
!end do
do i=1,m
    do j=1,bne
        if (be(j)>=en(i)) then
            pbe(j)=pbe(j)+1
            exit
        end if
    end do
end do
print*,"pbe"
pbe(:)=pbe(:)/m
open(unit=16,file="spdist.dat",status="replace")
do i=1,bns
    write(16,*)bs(i),pbs(i)
end do
close(16)
open(unit=30,file="endist.dat",status="replace")
do i=1,bne
    write(30,*)be(i),pbe(i)
end do
close(30)
end program metrovel

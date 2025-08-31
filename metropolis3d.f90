program metrovel3
implicit none
real,allocatable,dimension(:,:)::v,th,eta
real,allocatable,dimension(:)::sp,en,bs,be,pbs,pbe
real::s,h,t,en1,en2,vmax,sg,w
real,parameter::pi=3.1415926535
integer::i,j,n,m,ex,bns,bne
print*,"enter no. of particles"
read*,n
print*,"enter time"
read*,m
print*,"enter max speed"
read*,s
print*,"enter value of h"
read*,h
print*,"enter temperature"
read*,t
print*,"enter no. of speed bins"
read*,bns
print*,"enter no. of energy bins"
read*,bne
print*,"enter ex"
read*,ex
allocate(v(n,3),th(n,2),sp(n),eta(n,3),bs(bns),be(bne),en(m))
call random_number(sp)
sp(:)=s*(2*sp(:)-1)
call random_number(th)
th(:,1)=pi*th(:,1)
th(:,2)=2*pi*th(:,2)
v(:,1)=sp(:)*sin(th(:,1))*cos(th(:,2))
v(:,2)=sp(:)*sin(th(:,1))*sin(th(:,2))
v(:,3)=sp(:)*cos(th(:,1))
do i=1,n
	en1=en1+(v(i,1)**2+v(i,2)**2+v(i,3)**2)
end do
en1=en1/2
j=1
en(1)=en1
be(1)=en1/bne
ex=0
print*,"bdw"
do while (j<m .and. ex<10000)
        en2=0
        call random_number(eta)
        !print*,"eta"
        !print*,eta
        eta(:,:)=v(:,:)+h*(2*eta(:,:)-1)
        do i=1,n
                en2=en2+(eta(i,1)**2+eta(i,2)**2+eta(i,3)**2)
        end do
        en2=en2/2
        !print*,en1,en2
        !print*,"vel"
        !print*,eta
	call random_number(w)
        if (en1-en2>=0 .and. w<=exp((en2-en1)/t)) then
                j=j+1
                v(:,:)=eta(:,:)
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
print*,"adw"
do i=2,bne
        be(i)=be(1)*i
end do
allocate(pbs(bns),pbe(bne))
sp(:)=(v(:,1)**2+v(:,2)**2+v(:,3)**2)**0.5
vmax=sp(1)
do i=2,n
        if (vmax<sp(i)) vmax=sp(i)
end do
print*,vmax
sg=vmax/bns
print*,sg
bs(1)=0
do i=2,bns
    bs(i)=bs(i-1)+sg
!        print*,bs(i)
end do
do i=1,n
    do j=1,bns
        if (bs(j)>=sp(i)) then
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
open(unit=16,file="sp3dist.dat",status="replace")
do i=1,bns
    write(16,*)bs(i),pbs(i)
end do
close(16)
open(unit=30,file="en3dist.dat",status="replace")
do i=1,bne
    write(30,*)be(i),pbe(i)
end do
close(30)
end program metrovel3

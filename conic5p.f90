program conic5p
implicit none

	! I CHOSE COMPLEX ARRAY AS INPUT FORMAT IS SAME AS INPUTTING POINTS IN ANY GRAPHING APP
	! THE EQUATION OF A GENERAL CONIC SECTION IS IN THE FORM
	! ax^2 + by^2 + 2hxy + 2gx + 2fy + k = 0
	! GIVEN 5 POINTS,DENOTED BY (x1,y1) ,(x2,y2) ,(x3,y3), (x4,y4), (x5,y5) 
	! THE SET OF THESE EQUATIONS CAN BE WRITTEN AS A MATRIX MULTIPLICATION IN THE FORM
	! ___                         ___   _ _     _  _
	!|x1^2  y1^2  2*x1*y1  2*x1  2*y1| | a |   | -1 |
	!|x2^2  y2^2  2*x2*y2  2*x2  2*y2| | b |   | -1 |
	!|x3^2  y3^2  2*x3*y3  2*x3  2*y3|X| h |=k*| -1 |...LET THIS BE CALLED EQUATION {1}
	!|x4^2  y4^2  2*x4*y4  2*x4  2*y4| | g |   | -1 |
	!|x5^2  y5^2  2*x5*y5  2*x5  2*y5| | f |   | -1 |
	!|___                         ___| |_ _|   |_  _|
	!   
	!  THIS 5X5 MATRIX IS DENOTED BY eq(), THE MATRIX OF COEFFICIENTS IS coef()
	! SINCE THE ARRAY ON RHS IS -1 EVERYWHERE, THERE IS NO NEED TO DEFINE ANOTHER ARRAY, THUS CONSUMING LESS MEMORY
	! THE ABOVE IS 5 EQUATIONS 5 VARIABLE SYSTEM AND THE 6TH VARIABLE 'K' CAN BE ELIMINATED BY DIVING EVERYWHERE
	! TO SOLVE IT THE ABOVE WOULD LOOK LIKE
	!     _ _                    _ _
	!    | a |                  | -1|
	! 1  | b |  1               | -1|
	! _ X| h |= _ X (ADJ(eq)) X | -1|... THIS BE CALLED EQUATION {2}
	!    | g |                  | -1|
	! k  | f | |eq|             | -1|
	!    |_ _|                  |_ _|
	! THE ABOVE HOLDS TRUE IF k IS NOT EQUAL TO 0
	! THE ADJOINT OF MATRIX eq() IS DENOTED BY inv(), c() IS THE MATRIX WHOSE DETERMINANT WILL BE THE COFACTOR
	! det5 IS THE DETERMINANT OF THE MATRIX |eq|, det4 IS THE DETERMINANT OF c()
	! IF K=0 FOR SOME SCENARIO THAT IMPLIES THE CONIC SECTION PASSES THROUGH ORIGIN, HENCE det5 WOULD BE 0
	! THUS THE EQUATION WOULD BE SOLVED USING A 4X5 MATRIX GIVEN BY
	! ___                         ___   _ _   _ _
	!|x1^2  y1^2  2*x1*y1  2*x1  2*y1| | a | | 0 |
	!|x2^2  y2^2  2*x2*y2  2*x2  2*y2| | b | | 0 |
	!|x3^2  y3^2  2*x3*y3  2*x3  2*y3|X| h |=| 0 |
	!|x4^2  y4^2  2*x4*y4  2*x4  2*y4| | g | | 0 |
	!|___                         ___| | f | |_ _|
	!                                  |_ _| 
	! FROM THIS 4X5 MATRIX A MATRIX WITH NON ZERO DETERMINANT WILL BE PICKED, SO IT WILL RESEMBLE 4 EQUATION,4 VARIABLE
	! NOW OUT OF THE COEFFICIENTS, 1 COEFFICIENT OUT FO a,b,h HAS TO BE NON ZERO, ELSE IT IS A STRAIGHT LINE
	! LET a BE THE NON ZERO COEFFICIENT, HENCE I TAKE IT ON RHS AND THE EQUATION BECOMES
	! ___                   ___   _ _       _ _
	!|y1^2  2*x1*y1  2*x1  2*y1| | b |     | -1|
	!|y2^2  2*x2*y2  2*x2  2*y2| | h |     | -1|
	!|y3^2  2*x3*y3  2*x3  2*y3|X| g |= a X| -1|... THIS IS EQUATION {3}
	!|y4^2  2*x4*y4  2*x4  2*y4| | f |     | -1| 
	!|___                   ___| |_ _|     |_ _|
	! THE ABOVE 4X4 MATRIX IS DENOTED BY eq2() AND ITS DETERMINANT DENOTED BY det4, THE ADJOINT OF eq2 BE DENOTED BY
	! inv2() AND MATRIX FOR COFACTOR BE DEONTED BY c2(), ITS DETERMINANT BE det3 
	! THE EQUATION WITH INVERSE BE
	!     _ _                    _ _
	!    | b |                  | -1|
	! 1  | h |  1               | -1|
	! _ X| g |= _ X (ADJ(eq2)) X| -1|... THIS BE CALLED EQUATION {4}
	!    | f |                  | -1|
	! a  |_ _| |eq2|            |_ _|
	!                   
	! ONCE WE HAVE THE COEFFICIENTS, THESE WILL BE PUT IN SEVERAL CHECKING CONDITIONS TO DETERMINE 
	! WHICH CONIC SECTION IT IS AND WILL DISPLAY THE CONIC SECTION AND EQUATION
	! pol IS THE CHECKING CONDITION FOR PAIR OF LINES, IN THEORY IT IS A DETERMINANT AND CONDITION GIVEN BY
	! 
	!|a h g|
	!|h b f|=0 , IF THIS HOLDS TRUE, THE EQUATION HAS TO BE PAIR OF LINES
	!|g f k|
	!
	! IF THIS CONDITION DOESNT HOLD TRUE, VALUE OF del IS CHECKED
	! del=h^2-a*b
	! IF del=0 THEN CONIC SECTION IS PARABOLA
	! IF del<0 AND a=b THE CONIC SECTION IS CIRCLE
	! IF del<0 AND a/=b THE CONIC SECTION IS ELLIPSE
	! IF del>0 THEN CONIC SECTION IS HYPERBOLA

complex::p(5)
real::eq(5,5),coef(5),a,b,h,f,g,k,inv(5,5),pol,del,c(4,4),det4,det5,eq2(4,4),c2(3,3),inv2(4,4),det3,ab,abk,fgh,af2,bg2,kh2
	! THE LAST FEW VARIABLES AFTER det3 WERE USE TO MAKE A FEW THINGS LOOK SIMPLE 
integer::i,j,x,y,m,n,jj,z,xy,yx,t,d
	! HERE ALL THESE VARIABLES ARE USED IN DO LOOPS OR AS COUNTERS
integer,parameter::s(4)=[1,-1,-1,1]
	! THIS 1D ARRAY IS USED TO SET SIGN OF THE TERMS WHILE TAKING ADJOINT OF MATRIX
character(len=13)::conic
print*,"enter 5 points in the form (x,y)"
read*,p

500 continue
do i=2,5
	do j=i+1,5
		if (p(j)==p(i)) then
			print*,"no, put unique values, enter again"       ! THIS IS TO MAKE SURE USER DOESNT PUT 2 SAME POINTS
			read*,p
			go to 500
		end if
	end do
end do

m=0
n=0
do i=1,5
	if (p(i)==(0,0)) then; p(i)=p(5); p(5)=(0,0); end if             ! THIS IS TO CHECK IF USER HAS INPUT (0,0)
	if (real(p(i))==0) m=m+1                                         ! AND TO BRING IT TO BOTTOM SO THAT IT DOESNT 
	if (aimag(p(i))==0) n=n+1                                        ! INTERFERE WITH CODE 
end do                                                                   ! THE COUNTERS M AND N IS TO CHECK IF CONIC IS POSSIBLE 


	! THE BELOW LOOP MAKES THE eq() MATRIX
do i=1,5
	eq(i,:)=[real(p(i))**2 ,aimag(p(i))**2 ,2*real(p(i))*aimag(p(i)) ,2*real(p(i)) ,2*aimag(p(i))]  
	!print*,(eq(i,j),j=1,5)
end do
if (m>=4 .or. n>=4) then; print*," no unique conic section available"; go to 1000; end if    ! CHECKING CONDITION
m=0
n=0

300 continue
if (eq(1,3)==0 .and. eq(2,3)==0 .and. eq(3,3)==0 .and. eq(4,3)==0 .and. eq(5,3)==0) then  ! THIS TACKLES A SPECIAL CASE
	a=0
	b=0
	h=0.5
	f=0
	g=0
	k=0
	conic="pair of lines"
	go to 200
end if 

	! THE BELOW NESTED LOOPS CALCULATE DETERMINANT OF eq()
det5=0
d=0
t=1     ! t SETS THE NO. IN ARRAY s() WHICH DECIDES + OR - SIGN OF TERMS
do 5 i=1,5
	do 15 j=1,5
		if (j==i) cycle
		do 25 x=1,5
			if (x==i) cycle
			if (x==j) cycle
			do 35 y=1,5
				if (y==i) cycle
				if (y==j) cycle
				if (y==x) cycle
				do 45 m=1,5
					if (m==i) cycle
					if (m==j) cycle
					if (m==x) cycle
					if (m==y) cycle
					!print*,i,j,x,y,m,s(t)
					det5=det5-(-1)**i*s(t)*eq(1,i)*eq(2,j)*eq(3,x)*eq(4,y)*eq(5,m)
					!print*,det5
					!print*,d
					!d=d+1
					t=t+1
					if (t==5) t=1               ! THIS KEEPS t BETWEEN 1 AND 4
				45 continue
			35 continue
		25 continue
	15 continue
5 continue
!print*,"det5",det5

	! THIS IS WHERE THE 2 CASES k=0 AND k/=0 ARE TAKEN SEPERATELY

if (floor(det5*1000)==0 .or. ceiling(det5*1000)==0) then
	jj=1
	100 continue
	m=0
	do i=1,4
		m=m+1
		n=0
		do j=1,5
			if (j==jj) cycle
			n=n+1
			eq2(m,n)=eq(i,j)       ! THIS CREATES eq2 USING eq 
		end do
		!print*,(eq2(i,j),j=1,4),i
	end do
	det4=0
	t=1
	do 4 i=1,4
		do 14 j=1,4
			if (j==i) cycle
			do 24 x=1,4
				if (x==i) cycle
				if (x==j) cycle
				do 34 y=1,4
					if (y==i) cycle
					if (y==j) cycle
					if (y==x) cycle
					det4=det4+s(t)*eq2(1,i)*eq2(2,j)*eq2(3,x)*eq2(4,y)
					t=t+1
					if (t==5) t=1
				34 continue
			24 continue
		14 continue
	4 continue
	if (floor(det4*1000)==0 .or. ceiling(det4*1000)==0) then          ! THIS IS TO CHECK WHETHER det4 IS 0 OR NOT
		jj=jj+1                                                   ! IF IT IS 0 THEN IT WILL CHOOSE NEW eq2
		if (jj==4) then; print*,"no unique conic section available"; go to 1000; end if
		go to 100
	end if
	do 11 i=1,4
		do 21 j=1,4                                               ! THESE NESTED LOOPS GIVE THE ADJOINT OF MATRIX
			c=0                                               ! I.E inv2
			m=0
			do 31 x=1,4
				if (x==i) cycle
				m=m+1
				n=0
				do 41 y=1,4
					if (y==j) cycle
					n=n+1
					c2(m,n)=eq2(x,y)                 ! THIS GIVES COFACTOR MATRIX
				41 continue
			31 continue
			det3=0
			t=1
			do 51 x=1,3
				do 61 y=1,3
					if (y==x) cycle
					do 71 xy=1,3
						if (xy==x) cycle
						if (xy==y) cycle
						det3=det3+s(t)*c2(1,x)*c2(2,y)*c2(3,xy)   ! THE COFACTOR DETERMINANT
						t=t+1
						if (t==5) t=1
					71 continue
				61 continue
			51 continue
			inv2(j,i)=(-1)**(i+j)*det3
		21 continue
	11 continue
	coef=0
	do i=1,4
		do j=1,4
			coef(i)=coef(i)-inv2(i,j)*eq(j,jj)               ! THIS IS MULTIPLICATION FROM EQUATION {4}
		end do
	end do
	!print*,"det4",det4
	g=coef(3)
	f=coef(4)                                                       
	k=0
	if (jj==1) then; a=det4; b=coef(1); h=coef(2); end if            ! THIS ASSIGNS VALUE OF THE COEFFICIENTS
	if (jj==2) then; a=coef(1); b=det4; h=coef(2); end if            ! I PURPOSELY DIDNT DIVIDE THE MATRIX BY det4
	if (jj==3) then; a=coef(1); b=coef(2); h=det4; end if            ! SO THAT COEFFICIENTS HAVE A VALUE THATS NOT SO
else                                                                     ! SMALL THAT IT WILL BE NEARLY 0
	do 12 i=1,5
		do 22 j=1,5
			c=0
			m=0
			do 32 x=1,5
				if (x==i) cycle
				m=m+1
				n=0
				do 42 y=1,5
					if (y==j) cycle
					n=n+1
					c(m,n)=eq(x,y)                  ! THIS CREATES THE COFACTOR MATRIX
				42 continue
			32 continue
			det4=0
			t=1
			do 52 x=1,4
				do 62 y=1,4
					if (y==x) cycle
					do 72 xy=1,4
						if (xy==x) cycle
						if (xy==y) cycle
						do 82 yx=1,4
							if (yx==x) cycle
							if (yx==y) cycle           ! TAKING DETERMINANT OF EVERY COFACTOR MATRIX
							if (yx==xy) cycle            
							det4=det4+s(t)*c(1,x)*c(2,y)*c(3,xy)*c(4,yx)
							t=t+1
							if (t==5) t=1
						82 continue
					72 continue
				62 continue
			52 continue
			inv(j,i)=(-1)**(i+j)*det4
		22 continue
	12 continue
	!do i=1,5
		!print*,(inv(i,j),j=1,5)
	!end do
	do 92 i=1,5
		do 102 j=1,5
			coef(i)=coef(i)-inv(i,j)                          ! MULTIPLICATION FROM EQUATION {2}
		102 continue
		!print*,coef(i)
	92 continue
a=coef(1)
b=coef(2)
h=coef(3)
g=coef(4)                 ! ASSIGNING COEFFICIENT VALUES
f=coef(5)
k=det5
end if

if (k==0) then
	if (det4>10000 .or. det4<-10000) then                       ! SCALING DOWN COEFFICIENTS SO THEY GIVE VALUES WHICH
		a=a/5000                                            ! ARE EASY TO COMPUTE AND FOR USERS TO READ
		b=b/5000
		h=h/5000
		g=g/5000
		f=f/5000
	end if
	if (det4>10**8 .or. det4<-10**8) then
		a=a/(5*10**6)
		b=b/(5*10**6)
		h=h/(5*10**6)
		g=g/(5*10**6)
		f=f/(5*10**6)
	end if
	if (det4<1 .and. det4>-1) then
		if (det4>0.001 .or. det4<-0.001) then
			a=a*1000                                ! SCALING UP COEFFICIENTS INCASE THEY ARE SMALL 
			b=b*1000                                ! SO pol ISNT 0
			h=h*1000
			g=g*1000
			f=f*1000
		end if
	end if
else
	if (det5>10000 .or. det5<-10000) then
		a=a/5000
		b=b/5000
		h=h/5000
		g=g/5000
		f=f/5000
		k=k/5000
	end if
	if (det5>10**8 .or. det5<-10**8) then
		a=a/(5*10**6)
		b=b/(5*10**6)
		h=h/(5*10**6)
		g=g/(5*10**6)                               ! SAME THING DONE FOR det5
		f=f/(5*10**6)
		k=k/(5*10**6)
	end if
	if (det5<1 .and. det5>-1) then
		if (det5>0.001 .or. det5<-0.001) then
			a=a*1000
			b=b*1000
			h=h*1000
			g=g*1000
			f=f*1000
			k=k*1000
		end if
	end if
end if

abk=a*b*k
fgh=2*f*g*h
af2=a*(f**2)
bg2=b*(g**2)
kh2=k*(h**2)
!print*,"abk",abk,"fgh",fgh,"af2",af2,"bg2",bg2,"kh2",kh2

pol=abk+fgh-af2-bg2-kh2                    ! THE CONDITION FOR PAIR OF LINES, SINCE ITS TO POWER 3, NOS. WOULD GET VERY BIG
                                           ! WHICH CAN LEAD TO ERROR IN COMPUTING, HENCE THEY WERE SCALED DOWN
                                           ! AND SCALED UP SO THEY ARE NOT TOO SMALL
!print*,'pol',pol
if (floor(pol*1000)==0 .or. ceiling(pol*1000)==0) then
	conic="pair of lines"
else
	del=h**2-a*b                      ! CHECKING CONDITION FOR OTHER CONIC SECTIONS
	if (floor(del*1000)==0 .or. ceiling(del*1000)==0) then
		if (floor(a*1000)==0 .or. ceiling(a*1000)==0 .or. floor(b*1000)==0 .or. ceiling(b*1000)==0) conic="parabola"
	else if (del<0) then
		ab=a-b
		if (floor(ab*1000)==0 .or. ceiling(ab*1000)==0) then
			conic="circle"
		else
			conic="ellipse"
		end if
	else
		conic="hyperbola"
	end if
end if

200 continue
if (floor(a*10000)==0 .or. ceiling(a*10000)==0) a=0                  ! THIS IS TO SIMPLIFY COEFFICIENTS SO VERY SMALL ONES
if (floor(b*10000)==0 .or. ceiling(b*10000)==0) b=0                  ! ARE CONSIDERED AS 0
if (floor(h*10000)==0 .or. ceiling(h*10000)==0) h=0
if (floor(g*10000)==0 .or. ceiling(g*10000)==0) g=0
if (floor(f*10000)==0 .or. ceiling(f*10000)==0) f=0
if (floor(k*10000)==0 .or. ceiling(k*10000)==0) k=0

print*,"coefficients of ax^2+by^2+2hxy+2gx+2fy+c=0 are"
print*,a,b,h,g,f,k
print*,"conic section is:",conic
1000 continue
end program conic5p

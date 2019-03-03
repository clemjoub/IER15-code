!-----------------------------------------------------------------------------------------------------------------------------!
!-----------------------------------------------------------------------------------------------------------------------------!
!
!												USEFUL ROUTINES
!
!-----------------------------------------------------------------------------------------------------------------------------!
!-----------------------------------------------------------------------------------------------------------------------------!


!-----------------------------------------------------------------------------------------------------------------------------!
!												CRRA UTILITY FUNCTION
!-----------------------------------------------------------------------------------------------------------------------------!

subroutine util(cons,ut,mut)

use params
implicit none

double precision:: CRRA,MCRRA
double precision,intent(in)::cons
double precision,intent(out)::ut, mut



if (cons<=cmin) then
	if (abs(sigma(typ)-1.0)<0.0001) then
 		CRRA=(1.0-betaa(typ))* ( log(cmin)+scale*(cons-cmin)/cmin )
	else
 		CRRA=(1.0-betaa(typ))* ( (1.0/(1.0-sigma(typ)))*(cmin**(1.0-sigma(typ))-1.0) + (cmin**-sigma(typ))*scale*(cons - cmin) )
	endif
else
	if (abs(sigma(typ)-1.0)<0.0001) then
 		CRRA=(1.0-betaa(typ))*log(cons)
	else
 		CRRA=(1.0-betaa(typ))*(1.0/(1.0-sigma(typ)))*(cons**(1.0-sigma(typ))-1.0)
	endif
endif

mut=0.0

!if (cons<=cmin) then
!	if (abs(sigma(typ)-1.0)<0.0001) then
 !		MCRRA=(1.0-betaa(typ))* ( (1.0/cmin)-scale*(cons-cmin)/cmin**2.0 )
!	else
 !		MCRRA=(1.0-betaa(typ))* ( cmin**(-sigma(typ)) -sigma(typ)*scale*(cons-cmin)*(cmin**(-sigma(typ)-1.0)) )
!	endif
!else
!	if (abs(sigma(typ)-1.0)<0.0001) then
 !		MCRRA=(1.0-betaa(typ))/cons
!	else
 !		MCRRA=(1.0-betaa(typ))*cons**(-sigma(typ))
!	endif
!endif

ut=CRRA
!mut=MCRRA




end subroutine util




!-----------------------------------------------------------------------------------------------------------------------------!
!												MATRIX MULTIPLICATION
!-----------------------------------------------------------------------------------------------------------------------------!



subroutine matpd(n1,m1,k1,a1,b1,c1)


integer i,j,ii,m1,k1
double precision a1(n1,m1),b1(m1,k1),c1(n1,k1)
double precision s1

	do 91 i=1,n1
		do 90 j=1,k1
			s1=0.0
			do 80 ii=1,m1
				s1=s1+a1(i,ii)*b1(ii,j)
80			continue
		c1(i,j)=s1
90		continue
91	continue

return
endsubroutine matpd


!-----------------------------------------------------------------------------------------------------------------------------!
!												UNIFORM DRAWS
!-----------------------------------------------------------------------------------------------------------------------------!



subroutine ran1(idum,vv,n)

!idum= ?
!n = nombre de nombres aleatoires desires
!vv = la ou les nombres sont ranges

external ran1sub
double precision vv(n),x
integer kk,n,idum
vvloop:do kk=1,n
	call ran1sub(idum,x)
	vv(kk)=x
end do vvloop

end subroutine ran1



subroutine ran1sub (idum,x)

implicit none
integer,parameter::k4b=selected_int_kind(9)
integer(k4b),intent(inout)::idum
double precision::x
integer(k4b),parameter::ia=16807,im=2147483647, iq=127773, ir=2836
double precision,save::am
integer(k4b),save::ix=-1,iy=-1,k

if (idum <= 0 .or. iy < 0) then
	am=nearest(1.0,-1.0)/im
	iy=ior(ieor(888889999,abs(idum)),1)
	ix=ieor(777755555,abs(idum))
	idum=abs(idum)+1
endif

ix=ieor(ix,ishft(ix,13))
ix=ieor(ix,ishft(ix,-17))
ix=ieor(ix,ishft(ix,5))
k=iy/iq
iy=ia*(iy-k*iq)-ir*k
if (iy<0) iy=iy+im
x=am*ior(iand(im,ieor(ix,iy)),1)

endsubroutine ran1sub


!-----------------------------------------------------------------------------------------------------------------------------!
!												GAUSSIAN DRAWS
!-----------------------------------------------------------------------------------------------------------------------------!



subroutine gasdev(idum,vv,n)

!	This function returns a normally distributed deviate with zero mean and unit variance, using ran1(idum) as the source of uniform
!	deviates

	integer iset,n,k,gotoc
	double precision v1,v2,r,fac,gset,gasdev1
	double precision temp1(2),vv(n)

	external ran1

!	write(*,*) 'want',n,'random numbers'
	vvloop:do k=1,n

	iset= 0 
	v1=0.
	v2=0.
	r=0.
	fac=0.
	gset=0.
	gasdev1=0.
	ix1=0.
	ix2=0.
	gotoc=0

	if (iset.eq.0) then

	1	call ran1(idum,temp1,2)
		v1=2*temp1(1)-1.
		v2=2*temp1(2)-1.

	!	write(*,*) 'v1',v1
	!	write(*,*) 'v2',v2
	!	write(*,*) 'idum',idum

		r=v1**2.+v2**2.
		if (r.ge.1.) then
			gotoc=gotoc+1
			if (gotoc.gt.n) then 
		!		write(*,*) 'error in gasdev'
			end if 
			go to 1
		end if

		fac=dsqrt(-2.*dlog(r)/r)
		gset=v1*fac
		gasdev1=v2*fac
		iset=1
	else
		gasde1=gset
		iset=0
	end if

	vv(k)=gasdev1
!	wirte(*,*) 'random normal number',k,vv(k)
	
	end do vvloop

	return

end subroutine gasdev


!-----------------------------------------------------------------------------------------------------------------------------!
!												MATRIX INVERSION
!-----------------------------------------------------------------------------------------------------------------------------!




subroutine dmatinv(a,y,np)
		integer np
		real(8) a(np,np),y(np,np),d,ac(np,np),b(np)
		integer indx(np),i,j,n

		external LUDCMP, lubksb

!		make a copy of a so it will not be destroyed and initialize y
		
		y= 0.0
		loop1: do i=1,np
			ac(i,i) = a(i,i)
			y(i,i) = 1.0
		loop2: do j= 1, np
			ac(i,j) = a(i,j)
		enddo loop2
		enddo loop1
		
		indx(:)=0
		n=np
		call LUDCMP(ac,np,np,indx,d)
		loop13: do j= 1,n
			call lubksb(ac,n,np,indx,y(1,j))
		enddo loop13
		
		return 
end subroutine dmatinv	
	 
	 
	 
	 
	  SUBROUTINE lubksb(a,n,np,indx,b)
      INTEGER n,np,indx(n)
      REAL(8) a(np,np),b(n), sum
      INTEGER i,ii,j,ll

      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        if (i.lt.n)then
			do 13 j=i+1,n
			  sum=sum-a(i,j)*b(j)
13			 continue
		endif
        b(i)=sum/a(i,i)
14    continue
      return
      END

	

	
	
	  SUBROUTINE LUDCMP(A,N,NP,INDX,D)

	  REAL(8) A(NP,NP), VV(400), D, SUM, AAMAX, DUM
!	  REAL(8) A(NP,NP), VV(1000), D, SUM, AAMAX, DUM
	  REAL(8) TINY
	  INTEGER INDX(N),N,NP,IMAX,NMAX
	  INTEGER I,J,K

	  NMAX=400
	  TINY=1.0d-20 *1.0d-20
	  D=1.0d-20
	  VV=0.0d-0
	  SUM = 0.0d-0

      DO 12 I=1,N
        AAMAX=0.
        DO 11 J=1,N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11      CONTINUE
        IF (AAMAX.EQ.0.) PAUSE 'Singular matrix.'
        VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
        IF (J.GT.1) THEN
          DO 14 I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1)THEN
              DO 13 K=1,I-1
                SUM=SUM-A(I,K)*A(K,J)
13            CONTINUE
              A(I,J)=SUM
            ENDIF
14        CONTINUE
        ENDIF
        AAMAX=0.
        DO 16 I=J,N
          SUM=A(I,J)
          IF (J.GT.1)THEN
            DO 15 K=1,J-1
              SUM=SUM-A(I,K)*A(K,J)
15          CONTINUE
            A(I,J)=SUM
          ENDIF
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
16      CONTINUE
        IF (J.NE.IMAX)THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
17        CONTINUE
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF(J.NE.N)THEN
          IF(A(J,J).EQ.0.)A(J,J)=TINY
          DUM=1./A(J,J)
          DO 18 I=J+1,N
            A(I,J)=A(I,J)*DUM
18        CONTINUE
        ENDIF
19    CONTINUE
      IF(A(N,N).EQ.0.)A(N,N)=TINY
      RETURN
      END

subroutine get_eps(evec,evcv,nEps,ndraws_eps,idum,death)


!***********************************************************************************************************************************

implicit none


!***********************************************************************************************************************************
!
!		This subroutine draws the random variables epsilon from a multivariate random normal distribution with variance-covariance
!		matrix EVCV. 
!		Calls the cholesky decomposition subroutine.
!
!***********************************************************************************************************************************

integer,intent(IN)::								nEps
integer,intent(IN)::								ndraws_eps
integer,intent(IN)::								death
integer,intent(IN)::								idum
double precision,intent(IN)::						evcv(nEps,nEps)
double precision,intent(OUT)::						evec(death*ndraws_eps*nEps)

!! LOCAL VARIABLE
double precision					p(nEps,nEps),eiid(nEps)       
integer								i,j,k,l
integer								s1,vseed(3),v1
double precision , allocatable::	r(:)
double precision					etemp(nEps),rtemp(nEps)
double precision					disc

external choldc, ran1

!***********************************************************************************************************************************



allocate(r(ndraws_eps*death*nEps))

r=0.0
p=0.0
eiid=0.0
etemp=0.0
evec=0.0

!! Choleski decomposition of varcovar matrix evcv
call choldc(evcv,nEps,p)

do i=1,nEps
do j=1,nEps

	if (j>i) p(i,j)=0.0

enddo
enddo

!!	Draw normal shocks, store in r
call gasdev(idum,r,ndraws_eps*death*nEps)

!!	Impose varcovar properties to shocks 
loop3: do k=1,death		    	
loop4: do l=1,ndraws_eps
				
		s1=(k-1)*nEps*ndraws_eps+(l-1)*nEps+1	
		rtemp=r(s1:s1+nEps-1)			
		etemp=0.0

		loop1: do i=1,nEps
		loop2: do j=1,nEps

				etemp(i)= etemp(i)+p(i,j)*rtemp(j)

		end do loop2
		end do loop1

		evec(s1:s1+nEps-1) = etemp

end do loop4
end do loop3

deallocate(r)

endsubroutine get_eps








subroutine choldc(a,n,p)

integer,intent(in)::n
double precision, dimension(n,n),intent(in)::a
double precision, dimension(n,n),intent(out)::p
double precision, dimension(n,n)::aa
double precision sum
integer i,j,k

sum=0.0
aa(1:n,1:n)=a(1:n,1:n)
do 13 i =1,n
	do 12 j=1,n
	sum=aa(i,j)
	do 11 k= i-1,1,-1
		sum=sum-aa(i,k)*aa(j,k)
11	continue
	if (i.eq.j) then
		if (sum.le.0)pause'choldc failed'
		p(i,i)=dsqrt(sum)
	else
		aa(j,i)=sum/p(i,i)
		p(j,i)=aa(j,i)
	end if 
12	continue
13	continue
	return

	endsubroutine choldc

subroutine get_theta(theta,sevec,Emax,regg,toggle,ndraws_st,nregg,numv,Rsq,adj_Rsq)

!***********************************************************************************************************************************
!																																 !
!          This subroutine takes as an imput the EMAX matrix (containing the emax values computed for a subset of spaces)        !
!           and the state matrix STATEMAT containing the corresponding states variables. It compute the OLS coefficients         !
!           and stores them in the THETA matrix, as well as the standard errors and R2 of the regression                         !
!																																 !
!***********************************************************************************************************************************

implicit none

!***********************************************************************************************************************************

integer, intent(IN)::										ndraws_st				!Number of state point draws
integer, intent(IN)::										numv					!Number of regressors (excluding the colinear ones)
integer, intent(IN)::										nregg					!Number of regressors (including the colinear ones)
double precision, dimension(1,nregg),intent(OUT)::			sevec					!Vector with std errors of the coefficients
double precision, dimension(1,nregg),intent(OUT)::			theta					!Vector of coefficients
double precision,intent(OUT)::								Rsq,adj_Rsq				!Vector with Rsq values
double precision, dimension(ndraws_st,1),intent(IN)::		Emax					!Vector of Emax values

double precision, dimension(ndraws_st,1)::					ybar1					!Vector containing the average of the Emax values
double precision, dimension(ndraws_st,1)::					res						!Vector containing the fitted errors
double precision, dimension(ndraws_st,1)::					predval					!Vector containing the fitted values
double precision, dimension(ndraws_st,nregg),intent(IN)::	regg					!Matrix of regressors (including colinear ones)
double precision, allocatable::								statemat(:,:)			!Matrix of regressors (excluding colinear ones)
double precision, allocatable::								statematP(:,:)			!Inverse of the matrix of regressors
integer,intent(IN)::										toggle(nregg)			!Vector with regressors to be excluded
integer::													flag1					!Signals a problem in inverting the matrix of regressors
double precision, dimension(numv,1)::						xpy													
double precision, dimension(numv,numv)::					xpx,xpxt,xpxi
double precision::											SSE1,SST1,sigmasq,test
	
!! LOCAL VARIABLES
integer::													i,j,k,l
double precision, dimension(numv,1)::						temp
double precision, dimension(numv,numv)::					checki
double precision::											dist1

external matpd

!***********************************************************************************************************************************



allocate (statemat(ndraws_st,numv))
allocate (statematP(numv,ndraws_st))

statematP=0.0d-0
statemat=0.0d-0
temp=0.0d-0
xpx=0.0d-0
xpy=0.0d-0
xpxi=0.0d-0
checki=0.0d-0
theta=0.0d-0
sevec=0.0d-0



!!Check for lines with only 0
do k=1,numv
test=sum(regg(:,k))
if (test==0) print*, 'line',k,'is 0'
enddo

!!Exclude some regressors
l=0
TOGGLEOUT: do k=1,nregg
			if (toggle(k).ne.1) then
				l=l+1
				statemat(:,l)=regg(:,k)
			endif
enddo TOGGLEOUT



!!Compute OLS coefficient
TRANSPOSE: do j=1,numv
			statematP(j,1:ndraws_st)=statemat(1:ndraws_st,j)
enddo TRANSPOSE

call matpd(numv,ndraws_st,numv,statematP,statemat,xpx)
call matpd(numv,ndraws_st,1,statematP,Emax,xpy)

temp=xpy
xpxi=xpx
xpxt=xpx
call dmatinv (xpxt,xpxi,numv) 

call matpd(numv,numv,numv,xpxi,xpx,checki)
flag1=0
CHECKINV: do i= 1,numv
			dist1=abs(checki(i,i)-1.0)
			if (dist1.gt.0.0001d-0) then 
				flag1=1
!				pause
			endif
enddo CHECKINV
if (flag1.eq.1) then
write(*,*)'****problem with the inverse'
endif

call matpd(numv,numv,1,xpxi,xpy,temp)



!!Predicted values		
call matpd(ndraws_st,numv,1,statemat,temp,predval)



!!Sum of squares
SSE1=0.0d-0
SST1=0.0d-0
ybar1(:,1)=sum(Emax(:,1))/ndraws_st
res=0.0d-0
SUMSQUARES:	do i=1,ndraws_st
			res(i,1)=Emax(i,1)-predval(i,1)
			SSE1=SSE1+res(i,1)*res(i,1)
			SST1=SST1+(Emax(i,1)-ybar1(i,1))*(Emax(i,1)-ybar1(i,1))
enddo SUMSQUARES



!!Put 0s in toggled-off columns
sevec=0.0d-0
l=0
TOGGLEIN:	do i=1,nregg
			if (toggle(i).ne.1) then
				l=l+1
				theta(1,i)=temp(l,1)
				sevec(1,i)=xpxi(l,l)
			elseif (toggle(i)==1) then
				theta(1,i)=0.0
				sevec(1,i)=0.0
			endif
enddo TOGGLEIN



!!Standard Errors
sigmasq=SSE1/(ndraws_st-numv)
STDERRORS:	do i=1,nregg
			sevec(1,i) = dsqrt(sigmasq*sevec(1,i))
enddo STDERRORS



!!R-squared
Rsq=0.0d-0
Rsq=1.0d-0-(SSE1/SST1)
adj_Rsq=0.0d-0
adj_Rsq=1-(1-Rsq)*(ndraws_st-1)/(ndraws_st-numv-1)
return

endsubroutine get_theta


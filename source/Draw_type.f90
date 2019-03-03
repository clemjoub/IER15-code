subroutine draw_type(edu_hh,edu_ww,cohortt,typp)

use params

!*****************************************************************************************
integer,intent(IN)::	edu_hh,edu_ww,cohortt
integer,intent(OUT)::	typp

!! LOCAL VARIABLES
double precision	z(ntypes),ez(ntypes),logit(ntypes)
double precision	p(1)
!*****************************************************************************************


typp=0

do i=1,ntypes
	z(i)=1.0+alphatyp(i)+lambdaedu_h(i)*edu_hh+lambdaedu_w(i)*edu_ww+lambdacohort(i)*cohortt
	ez(i)=dexp(-z(i))
enddo

do i=1,ntypes
	logit(i)=ez(i)/sum(ez(:))
enddo

if (abs(sum(logit(:))-1.0).gt.0.0001) then 
	print*, 'we have got a problemo with the logito'
endif

call ran1(idum,p,1)
cut_off=0.0
do i=1,ntypes
	cut_off=cut_off+logit(i)
	if (p(1)<cut_off) then
	typp=i
	exit
	endif
enddo

endsubroutine draw_type
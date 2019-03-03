
subroutine Draw_states

!*****************************************************************************************

Use params
Use Filenames
!use DFLIB

Implicit none

!*****************************************************************************************
!
! CREER TIRAGES ORDONNES PAR STATE VARIABLE POUR ANALYSER: ERREURS, POLICY FUNCTION, VALUE FUNCTION
!*****************************************************************************************

!! LOCAL VARIABLES
double precision::	draw_st(11),ret
double precision::	tempo(ndraws_st),temp
double precision::	inc_draw(2)
double precision::	start_inc_h,start_inc_w
double precision::	inc_growth_h, inc_growth_w
double precision::	cov_wage
integer::			getseed(2)
integer::			H_yearsinactive,W_yearsinactive, H_yearscovered, W_yearscovered, H_yearsuncovered, W_yearsuncovered
real::				H_pctcovered, W_pctcovered, H_pctuncovered, W_pctuncovered

!! FUNCTIONS
double precision::	costransform


!*****************************************************************************************


call random_seed(put=seedrandom_num)
call random_seed(get=getseed)

DRAWLOOP:	do state=1,ndraws_st
				call ran1(idum,draw_st,11)
				!! Draw permanent characteristics 
				statemat_edu_h	(state,startdraw)=int(draw_st(1)*nedu+1)
				statemat_edu_w	(state,startdraw)=int(draw_st(2)*nedu+1)
				statemat_cohort	(state,startdraw)=int(draw_st(3)*ncohort+1)
				statemat_type	(state,startdraw)=int(draw_st(4)*ntypes+1)

				!! Draw starting income and income growth
				call gasdev(idum,inc_draw,2)
				start_inc_h=min(max(3*inc_draw(1)+1.0,0.5),3.5)
				start_inc_w=min(max(3*inc_draw(2),0.5),2.5)
				inc_growth_h=draw_st(5)/30
				inc_growth_w=draw_st(6)/30

!				start_inc_h=max(3*inc_draw(1)+1.0,1.0)
!				start_inc_w=max(3*inc_draw(2),1.0)
!				inc_growth_h=draw_st(5)/20
!				inc_growth_w=draw_st(6)/20

				!! Draw career profile (fraction of working years in each sector)
				temp=costransform(draw_st(7))
				H_yearsinactive=nint((nwork)*temp)

				temp=costransform(draw_st(8))
				W_yearsinactive=nint((nwork)*temp)

				!! Draw years worked in each sector
				temp=costransform(costransform(draw_st(9)))
				H_yearscovered=nint((nwork-H_yearsinactive)*temp)
				H_yearsuncovered=nwork-H_yearsinactive-H_yearscovered

				temp=costransform(costransform(draw_st(10)))
				W_yearscovered=nint((nwork-w_yearsinactive)*temp)
				W_yearsuncovered=nwork-W_yearsinactive-W_yearscovered

				H_pctcovered=(H_yearscovered*1.0)/nwork
				H_pctuncovered=(H_yearsuncovered*1.0)/nwork
				W_pctcovered=(W_yearscovered*1.0)/nwork
				W_pctuncovered=(W_yearsuncovered*1.0)/nwork


				!! Draw states at start of work
				ia	=int(draw_st(11)*na+1)
				statemat_a		(state,startdraw)=grid_a(startdraw,ia)
				statemat_Bh		(state,startdraw)=0.0
				statemat_Bw		(state,startdraw)=0.0
				statemat_XP_Fh	(state,startdraw)=0
				statemat_XP_Ih	(state,startdraw)=0
				statemat_XP_Fw	(state,startdraw)=0
				statemat_XP_Iw	(state,startdraw)=0

				AGELOOP: do age=startdraw+1,startrec
							call ran1(idum,draw_st,3)
							if (age==27) continue

							!! Update permanent characteristics
							statemat_edu_h	(state,age)=statemat_edu_h	(state,age-1)
							statemat_edu_w	(state,age)=statemat_edu_w	(state,age-1)
							statemat_cohort	(state,age)=statemat_cohort	(state,age-1)
							statemat_type	(state,age)=statemat_type	(state,age-1)
							
							!! Draw non pension savings
							ia	=int(draw_st(1)*na+1)
							statemat_a	(state,age)=grid_a(age,ia)

							!! Compute pension savings from previous period's pension savings and previous period's labor choice

							if (draw_st(2)<H_pctcovered) then 
								statemat_pastd_h(state,age)=1
								statemat_XP_Fh	(state,age)=statemat_XP_Fh	(state,age-1)+1
								statemat_XP_Ih	(state,age)=statemat_XP_Ih	(state,age-1)
								cov_wage=start_inc_h*(1+inc_growth_h)**(age-startdraw)
								statemat_Bh(state,age)=statemat_Bh(state,age-1)*(1+r_B_bar)+tao*cov_wage
							elseif (draw_st(2)<H_pctcovered+H_pctuncovered) then
								statemat_pastd_h(state,age)=2
								statemat_Bh(state,age)=statemat_Bh(state,age-1)*(1+r_B_bar)
								statemat_XP_Fh	(state,age)=statemat_XP_Fh	(state,age-1)
								statemat_XP_Ih	(state,age)=statemat_XP_Ih	(state,age-1)+1
							else
								statemat_pastd_h(state,age)=3
								statemat_Bh(state,age)=statemat_Bh(state,age-1)*(1+r_B_bar)
								statemat_XP_Fh	(state,age)=statemat_XP_Fh	(state,age-1)
								statemat_XP_Ih	(state,age)=statemat_XP_Ih	(state,age-1)
							endif

							if (draw_st(3)<W_pctcovered) then 
								statemat_pastd_w(state,age)=1
								cov_wage=start_inc_w*(1+inc_growth_w)**(age-startdraw)
								statemat_Bw(state,age)=statemat_Bw(state,age-1)*(1+r_B_bar)+tao*cov_wage
								statemat_XP_Fw	(state,age)=statemat_XP_Fw	(state,age-1)+1
								statemat_XP_Iw	(state,age)=statemat_XP_Iw	(state,age-1)
							elseif (draw_st(3)<W_pctcovered+W_pctuncovered) then
								statemat_pastd_w(state,age)=2
								statemat_Bw(state,age)=statemat_Bw(state,age-1)*(1+r_B_bar)
								statemat_XP_Fw	(state,age)=statemat_XP_Fw	(state,age-1)
								statemat_XP_Iw	(state,age)=statemat_XP_Iw	(state,age-1)+1
							else
								statemat_pastd_w(state,age)=3
								statemat_Bw(state,age)=statemat_Bw(state,age-1)*(1+r_B_bar)
								statemat_XP_Fw	(state,age)=statemat_XP_Fw	(state,age-1)
								statemat_XP_Iw	(state,age)=statemat_XP_Iw	(state,age-1)
							endif

							if (age>=retirement) then
								statemat_a	(state,age)=grid_a(age,ia)
								statemat_Bh	(state,age)=0.0
								statemat_Bw	(state,age)=0.0
							endif
							
							!!Cap balances at 150 million
!							if (statemat_Bh(state,age)>150.0) statemat_Bh(state,age)=150.0
!							if (statemat_Bw(state,age)>150.0) statemat_Bw(state,age)=150.0
!							if (statemat_Bh(state,age)>450.0) statemat_Bh(state,age)=450.0
!							if (statemat_Bw(state,age)>450.0) statemat_Bw(state,age)=450.0
				enddo AGELOOP

enddo DRAWLOOP

!do age=startwork+1,startrec

!	tempo=statemat_a(:,age)
!	call SORTQQ(LOC(tempo),ndraws_st,SRT$REAL8)
!	statemat_a(:,age)=tempo
	
!enddo

!open (801, file=pathdatavsmodel//'st_draws.txt',position='rewind')
!do state=1,ndraws_st
!do age=45,startrec
!read(801,fmt=*) statemat_XP_Fh(state,age),statemat_XP_Ih(state,age),statemat_XP_Fw(state,age),statemat_XP_Iw(state,age),statemat_a(state,age),statemat_Bh(state,age),statemat_Bw(state,age)
!enddo
!enddo
!close(801)

do age=startdraw,death
    XP_Fh_max(age)=maxval(statemat_XP_Fh(:,age))
    XP_Fh_min(age)=minval(statemat_XP_Fh(:,age))
    XP_Ih_max(age)=maxval(statemat_XP_Ih(:,age))
    XP_Ih_min(age)=minval(statemat_XP_Ih(:,age))
    XP_Fw_max(age)=maxval(statemat_XP_Fw(:,age))
    XP_Fw_min(age)=minval(statemat_XP_Fw(:,age))
    XP_Iw_max(age)=maxval(statemat_XP_Iw(:,age))
    XP_Iw_min(age)=minval(statemat_XP_Iw(:,age))
	XP_hh_max(age)=maxval(statemat_XP_Fh(:,age)+statemat_XP_Ih(:,age)+statemat_XP_Fw(:,age)+statemat_XP_Iw(:,age))

	wealth_min(age)=minval(statemat_a(:,age)+statemat_Bh(:,age)+statemat_Bw(:,age))
	wealth_max(age)=maxval(statemat_a(:,age)+statemat_Bh(:,age)+statemat_Bw(:,age))
	Bh_max(age)=maxval(statemat_Bh(:,age))
	Bw_max(age)=maxval(statemat_Bw(:,age))
	
	if (age>startrec) then
		wealth_min(age)=0.0
		wealth_max(age)=9999999
		Bh_max(age)=9999999
		Bw_max(age)=9999999
	endif
enddo



endsubroutine Draw_states


function costransform(x)
double precision::x,costransform

if (x>1.0) then
	x=1.0_8
	write(*,*)'costransform requires a number smaller or equal to 1.0'
else if (x<0.0_8) then
	x=0.0_8
	write(*,*)'costransform requires a number larger or equal to 0.0'
endif

costransform=dcos(3.14159265*x)
costransform=costransform/2+0.5

endfunction

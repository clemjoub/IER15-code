subroutine AppspackIn

use params
use filenames
implicit none

character(len=40)			name			! A variable that "absorbs" the parameter name in the read statement
integer, allocatable::		inest(:),paramfile(:)
integer::					totnumparms, i
integer::					cohort										! Birth cohort
double precision::			raw_h_switch, raw_w_switch

integer     				k, kk
double precision::      maleprob, femaleprob


inest=0


call getarg(1,JLSin)
call getarg(2,JLSout)
call getarg(3,tag)

! Read parameters that are being estimated

!open (unit=1, file=path//JLSin,,position='rewind')
open (unit=1, file=path//'Newmin.txt',position='rewind')
open (unit=3, file=path//'otherparameters.txt',position='rewind')
open(unit=4,file=path//'inest.txt',position='rewind')
read(unit=4,fmt=*) totnumparms
allocate(inest(totnumparms))
allocate(paramfile(totnumparms))
read(unit=4,fmt=*) inest
close(4)
paramfile(:)=2*(1-inest(:))+1

i=1
read(fmt=*,unit=1)

!print*, 'reading type logit parameters'
do typ=1,ntypes
read (fmt=*,unit=paramfile(i)) lambdaedu_h(typ)
i=i+1
read (fmt=*,unit=paramfile(i)) lambdaedu_w(typ)
i=i+1
read (fmt=*,unit=paramfile(i)) lambdacohort(typ)
i=i+1
read (fmt=*,unit=paramfile(i)) alphatyp(typ)
i=i+1
enddo
!print*, 'reading preference parameters'
do typ=1,ntypes
read (fmt=*,unit=paramfile(i)) rho(typ)
i=i+1
read (fmt=*,unit=paramfile(i)) sigma(typ)
i=i+1
read (fmt=*,unit=paramfile(i)) raw_delta(typ,1)
i=i+1
read (fmt=*,unit=paramfile(i)) raw_delta(typ,2)
i=i+1
read (fmt=*,unit=paramfile(i)) raw_delta(typ,3)
i=i+1
read (fmt=*,unit=paramfile(i)) raw_delta(typ,4)
i=i+1
read (fmt=*,unit=paramfile(i)) raw_delta(typ,5)
i=i+1
read (fmt=*,unit=paramfile(i)) raw_delta(typ,7)
i=i+1
enddo
read (fmt=*,unit=paramfile(i)) delta_kids
i=i+1
read (fmt=*,unit=paramfile(i)) sig_pref_h
i=i+1
read (fmt=*,unit=paramfile(i)) sig_pref_w
i=i+1
read (fmt=*,unit=paramfile(i)) raw_h_switch_act
i=i+1
read (fmt=*,unit=paramfile(i)) raw_w_switch_act
i=i+1
read (fmt=*,unit=paramfile(i)) raw_h_switch_sect
i=i+1
read (fmt=*,unit=paramfile(i)) raw_w_switch_sect
i=i+1
!! !print*,'reading wage offer parameters'
do typ=1,ntypes
if (typ==1) then
read (fmt=*,unit=paramfile(i)) alpha(typ,1)
i=i+1
read (fmt=*,unit=paramfile(i)) alpha(typ,2)
i=i+1
read (fmt=*,unit=paramfile(i)) alpha(typ,3)
i=i+1
elseif (typ>1) then
read (fmt=*,unit=paramfile(i)) alpha(typ,1)
i=i+1
read (fmt=*,unit=paramfile(i)) alpha(typ,2)
i=i+1
read (fmt=*,unit=paramfile(i)) alpha(typ,3)
i=i+1
endif
enddo
read (fmt=*,unit=paramfile(i)) SR_Fh
i=i+1
read (fmt=*,unit=paramfile(i)) SR_Ih
i=i+1
read (fmt=*,unit=paramfile(i)) SR_Fw
i=i+1
read (fmt=*,unit=paramfile(i)) SR_Iw
i=i+1
read (fmt=*,unit=paramfile(i)) theta_cohort_F
i=i+1
read (fmt=*,unit=paramfile(i)) theta_cohort_I
i=i+1
read (fmt=*,unit=paramfile(i)) g_gap
i=i+1
read (fmt=*,unit=paramfile(i)) theta_FhFh
i=i+1
read (fmt=*,unit=paramfile(i)) theta_IhIh
i=i+1
!read (fmt=*,unit=paramfile(i)) theta_IhFh
!i=i+1
read (fmt=*,unit=paramfile(i)) theta_FwFw
i=i+1
read (fmt=*,unit=paramfile(i)) theta_IwIw
i=i+1
!read (fmt=*,unit=paramfile(i)) theta_FwIw
read (fmt=*,unit=paramfile(i)) theta_FhFh2
i=i+1
read (fmt=*,unit=paramfile(i)) theta_FhIh2
i=i+1
read (fmt=*,unit=paramfile(i)) theta_IhFh2
i=i+1
read (fmt=*,unit=paramfile(i)) theta_IhIh2
i=i+1
read (fmt=*,unit=paramfile(i)) theta_FwFw2
i=i+1
read (fmt=*,unit=paramfile(i)) theta_FwIw2
i=i+1
read (fmt=*,unit=paramfile(i)) theta_IwFw2
i=i+1
read (fmt=*,unit=paramfile(i)) theta_IwIw2
i=i+1
read (fmt=*,unit=paramfile(i)) XPtransfer
i=i+1
read (fmt=*,unit=paramfile(i)) theta_eduFh
i=i+1
read (fmt=*,unit=paramfile(i)) theta_eduIh
i=i+1
read (fmt=*,unit=paramfile(i)) theta_eduFw
i=i+1
read (fmt=*,unit=paramfile(i)) theta_eduIw
i=i+1
read (fmt=*,unit=paramfile(i)) theta_grad_Fh
i=i+1
read (fmt=*,unit=paramfile(i)) theta_grad_Fw
i=i+1
read (fmt=*,unit=paramfile(i)) theta_eduXPh
i=i+1
read (fmt=*,unit=paramfile(i)) theta_eduXPw
i=i+1
read (fmt=*,unit=paramfile(i)) sig_Fh
i=i+1
read (fmt=*,unit=paramfile(i)) sig_Ih
i=i+1
read (fmt=*,unit=paramfile(i)) sig_Fw
i=i+1
read (fmt=*,unit=paramfile(i)) sig_Iw
i=i+1
!! !print*, 'reading unemployment logit model parameters'
read (fmt=*,unit=paramfile(i)) gamma(1)  
i=i+1
read (fmt=*,unit=paramfile(i)) gamma(2)
i=i+1
read (fmt=*,unit=paramfile(i)) gammacov(1)
i=i+1
read (fmt=*,unit=paramfile(i)) gammacov(2)
i=i+1
read (fmt=*,unit=paramfile(i)) gammaXP(1)
i=i+1
read (fmt=*,unit=paramfile(i)) gammaXP(2)
i=i+1
read (fmt=*,unit=paramfile(i)) gammaedu(1)
i=i+1
read (fmt=*,unit=paramfile(i)) gammaedu(2)
i=i+1
read (fmt=*,unit=paramfile(i)) gammaage(1)
i=i+1
read (fmt=*,unit=paramfile(i)) gammaage(2)
i=i+1
!! !!print*, 'reading other parameters'
read (fmt=*,unit=paramfile(i)) nparams
i=i+1
read (fmt=*,unit=paramfile(i))  ndraws_st
i=i+1
read (fmt=*,unit=paramfile(i)) nspl
i=i+1
read (fmt=*,unit=paramfile(i))  ndraws_eps
i=i+1
read (fmt=*,unit=paramfile(i)) curv_a
i=i+1
read (fmt=*,unit=paramfile(i))  curv_B
i=i+1
read (fmt=*,unit=paramfile(i))  nsav
i=i+1
read (fmt=*,unit=paramfile(i))  death
i=i+1
read (fmt=*,unit=paramfile(i))  retirement
i=i+1
read (fmt=*,unit=paramfile(i))  startwork
i=i+1
read (fmt=*,unit=paramfile(i))  startrec
i=i+1
read (fmt=*,unit=paramfile(i))  endrec
i=i+1
read (fmt=*,unit=paramfile(i))  cmin
i=i+1
read (fmt=*,unit=paramfile(i))  scale
i=i+1
read (fmt=*,unit=paramfile(i))  sav_min
i=i+1
read (fmt=*,unit=paramfile(i))  lny_min
i=i+1
read (fmt=*,unit=paramfile(i))  a_min0
i=i+1
read (fmt=*,unit=paramfile(i))  B_min0
i=i+1
read (fmt=*,unit=paramfile(i))  bar0
i=i+1
read (fmt=*,unit=paramfile(i))  abar1
i=i+1
read (fmt=*,unit=paramfile(i))  abar2
i=i+1
read (fmt=*,unit=paramfile(i))  r
i=i+1
read (fmt=*,unit=paramfile(i))  r_B_bar
i=i+1
read (fmt=*,unit=paramfile(i))	sig_ret
i=i+1



!! Allocate array sizes using the parameters values read from input file

!ndraws_eps	= 1
nret		=	death-retirement+1	
nwork		=	retirement-startwork
noutspl  	=	ndraws_st-nspl
nd			=	nds*nsav


do typ=1,ntypes
raw_delta(typ,6)=raw_delta(typ,3)
raw_delta(typ,8)=raw_delta(typ,7)
raw_delta(typ,9)=raw_delta(typ,3)+raw_delta(typ,7)
alpha(typ,2)=alpha(1,2)
alpha(typ,3)=alpha(1,3)
enddo

!alpha(3,1)			=   -1.0
!alpha(3,2)			=	0.005


!MPyrs		=	int((nwork)/2)
MPyrs       =	20
Bbar1		=	abar1						
Bbar2		=	abar2	


!! Fill in grid with age-specific minimum and maximum asset holdings 

do age=1,death
	if (age<retirement) then
!		a_max(age)=1.5*age
		a_max(age)=2.0*age
		B_max(age)=age*age*age/5000
	elseif (age>=retirement) then
		a_max(age)=6*retirement
		B_max(age)=1.0
	endif
enddo

do age=1,death
	a_min(age)=a_min0
	B_min(age)=B_min0
enddo


realizedret(1)	= 0.128
realizedret(2)	= 0.128
realizedret(3)	= 0.2851
realizedret(4)	= 0.2125
realizedret(5)	= 0.0356
realizedret(6)	= 0.1342
realizedret(7)	= 0.1229
realizedret(8)	= 0.0541
realizedret(9)	= 0.0649
realizedret(10)	= 0.0692
realizedret(11)	= 0.1562
realizedret(12)	= 0.2968
realizedret(13)	= 0.0304
realizedret(14)	= 0.1621
realizedret(15)	= 0.1818
realizedret(16)	=-0.0252
realizedret(17)	= 0.0354
realizedret(18)	= 0.0472
realizedret(19)	=-0.0114
realizedret(20)	= 0.1626
realizedret(21)	= 0.0444
realizedret(22)	= 0.0674
realizedret(23)	= 0.0298
realizedret(24)	= 0.1055
realizedret(25)	= 0.0886
realizedret(26)	= 0.0458
realizedret(27)	= 0.1577
realizedret(28)	=-0.1998
realizedret(29)	= 0.1770


!do i=28,100
!realizedret(i)=r_B_bar
!enddo

close (1)
close (3)

fixedcom=0.00393
varcomrt=0.026

fixedbalcom=0.0
varbalcomrt=0.0

do cohort=1,ncohort
do age=1,100
if ((cohort==1 .and. age.ge.23 .and. age.le.30).or.(cohort==2 .and. age.ge.18.and. age.le.25).or.(cohort==3 .and. age.ge.18.and.age.le.20) )then
	fixedbalcom(cohort,age)=0.014
	varbalcomrt(cohort,age)=0.007
endif
enddo
enddo

allocate(grid_sav           (nsav                                                   ))
allocate(Vfun               (nd                                                     ))
allocate(statemat_a			(			ndraws_st		,death	))
allocate(statemat_Bh    	(			ndraws_st		,death	))
allocate(statemat_Bw    	(			ndraws_st		,death	))
allocate(statemat_XP_Fh  	(			ndraws_st		,death	))
allocate(statemat_XP_Fw 	(			ndraws_st		,death	))
allocate(statemat_XP_Ih 	(			ndraws_st		,death	))
allocate(statemat_XP_Iw 	(			ndraws_st		,death	))
allocate(statemat_edu_h 	(			ndraws_st		,death	))
allocate(statemat_edu_w 	(			ndraws_st		,death	))
allocate(statemat_cohort    (			ndraws_st		,death	))
allocate(statemat_type  	(			ndraws_st		,death	))
allocate(statemat_pastd_h   (			ndraws_st		,death	))
allocate(statemat_pastd_w   (			ndraws_st		,death	))
allocate(Emax				(			ndraws_st		,death	))
allocate(Emaxspl			(nspl	   	                			))
allocate(an_Emax			(			ndraws_st		,death	))
allocate(Num_Error			(			ndraws_st		,death	))
allocate(Interp_Emax	    (			ndraws_st		,death	))
allocate(Interp_Error    	(			ndraws_st		,death	))
allocate(avsav_out			(			ndraws_st		,death	))
allocate(avy_out			(			ndraws_st		,death	))
allocate(d_out				(nds,			ndraws_st		,death	))
!allocate(Emax				(		ntypes,	ndraws_st		,death	))
!allocate(an_Emax			(		ntypes,	ndraws_st		,death	))
!allocate(Num_Error			(		ntypes,	ndraws_st		,death	))
!allocate(Interp_Emax   	(		ntypes,	ndraws_st		,death	))
!allocate(Interp_Error   	(		ntypes,	ndraws_st		,death	))
!allocate(avsav_out			(		ntypes,	ndraws_st		,death	))
!allocate(avy_out			(		ntypes,	ndraws_st		,death	))
!allocate(d_out				(nds,   	ntypes,	ndraws_st		,death	))
!allocate(savprob_out    	(nsav,	        ntypes,	ndraws_st		,death	))
allocate(sav_out			(			 	  ndraws_eps		))
allocate(y_out				(				  ndraws_eps		))
allocate(Vmax				(				  ndraws_eps		))
allocate(an_Vmax			(				  ndraws_eps		))
allocate(d		        	(nds,		        	  ndraws_eps		))
allocate(savdum				(nsav,   			  ndraws_eps		))


if (verbose1==1) then

open (unit=93, file=pathoutput//'Spec.txt',position='rewind')
write (93,*) 'path'           ,path
write (93,*) 'pathoutput'     ,pathoutput
write (93,*) 'pathdatavsmodel',pathdatavsmodel
write (93,*) 'counterfactual', counterfactual
close(93)

open (unit=98,file=pathoutput//'checkparams.txt',position='rewind')

write(98,*),'totnumparms',totnumparms

write(98,*) 	lambdaedu_h(1),'	lambdaedu_h(1)'
write(98,*) 	lambdaedu_w(1),'	lambdaedu_w(1)'
write(98,*) 	lambdacohort(1),'	lambdacohort(1)'
write(98,*) 	alphatyp(1),'	alphatyp(1)'
write(98,*) 	lambdaedu_h(2),'	lambdaedu_h(2)'
write(98,*) 	lambdaedu_w(2),'	lambdaedu_w(2)'
write(98,*) 	lambdacohort(2)	,'lambdacohort(2)'
write(98,*) 	alphatyp(2)	,'alphatyp(2)'
write(98,*) 	lambdaedu_h(3)	,'lambdaedu_h(3)'
write(98,*) 	lambdaedu_w(3)	,'lambdaedu_w(3)'
write(98,*) 	lambdacohort(3)	,'lambdacohort(3)'
write(98,*) 	alphatyp(3)	,'alphatyp'
write(98,*) 	rho(1)	,'rho(1)'
write(98,*) 	sigma(1),'	sigma(1)'
write(98,*) 	raw_delta(1,1),'	raw_delta(11)'
write(98,*) 	raw_delta(1,2),'	raw_delta(12)'
write(98,*) 	raw_delta(1,3)	,'raw_delta(13)'
write(98,*) 	raw_delta(1,4)	,'raw_delta(14)'
write(98,*) 	raw_delta(1,5)	,'raw_delta(15)'
write(98,*) 	raw_delta(1,7)	,'raw_delta(17)'
write(98,*) 	rho(2)	,'rho(2)'
write(98,*) 	sigma(2),'	sigma(2)'
write(98,*) 	raw_delta(2,1),'	raw_delta(21)'
write(98,*) 	raw_delta(2,2)	,'raw_delta(22)'
write(98,*) 	raw_delta(2,3)	,'raw_delta(23)'
write(98,*) 	raw_delta(2,4)	,'raw_delta(24)'
write(98,*) 	raw_delta(2,5)	,'raw_delta(25)'
write(98,*) 	raw_delta(2,7)	,'raw_delta(27)'
write(98,*) 	rho(3)	,'rho(3)'
write(98,*) 	sigma(3),'	sigma(3)'
write(98,*) 	raw_delta(3,1),'	raw_delta(31)'
write(98,*) 	raw_delta(3,2)	,'raw_delta(32)'
write(98,*) 	raw_delta(3,3)	,'raw_delta(33)'
write(98,*) 	raw_delta(3,4)	,'raw_delta(34)'
write(98,*) 	raw_delta(3,5)	,'raw_delta(35)'
write(98,*) 	raw_delta(3,7)	,'raw_delta(37)'
write(98,*) 	delta_kids	,'delta_kids'
write(98,*) 	sig_pref_h	,'sig_pref_h'
write(98,*) 	sig_pref_w	,'sig_pref_w'
write(98,*) 	raw_h_switch_act,'	raw_h_switch_act'
write(98,*) 	raw_w_switch_act,'	raw_w_switch_act'
write(98,*) 	raw_h_switch_sect,'	raw_h_switch_sect'
write(98,*) 	raw_w_switch_sect,'	raw_w_switch_sect'
write(98,*) 	alpha(1,1)	,'alpha(11)'
write(98,*) 	alpha(1,2)	,'alpha(12)'
write(98,*) 	alpha(1,3)	,'alpha(13)'
write(98,*) 	alpha(2,1),'	alpha(21)'
write(98,*) 	alpha(2,2),'	alpha(22)'
write(98,*) 	alpha(2,3),'	alpha(23)'
write(98,*) 	alpha(3,1),'	alpha(31)'
write(98,*) 	alpha(3,2),'	alpha(32)'
write(98,*) 	alpha(3,3),'	alpha(33)'
write(98,*) 	SR_Fh	,'SR_Fh'
write(98,*) 	SR_Ih	,'SR_Ih'
write(98,*) 	SR_Fw	,'SR_Fw'
write(98,*) 	SR_Iw	,'SR_Iw'
write(98,*) 	theta_cohort_F,'	theta_cohort_F'
write(98,*) 	theta_cohort_I	,'theta_cohort_I'
write(98,*) 	g_gap, 'g_gap'	
write(98,*) 	theta_FhFh,'	theta_FhFh'
write(98,*) 	theta_IhIh,'	theta_IhIh'
write(98,*) 	theta_FwFw,'	theta_FwFw'
write(98,*) 	theta_IwIw,'	theta_IwIw'
write(98,*) 	theta_FhFh2,'	theta_FhFh2'
write(98,*) 	theta_FhIh2,'	theta_FhIh2'
write(98,*) 	theta_IhFh2,'	theta_IhFh2'
write(98,*) 	theta_IhIh2,'	theta_IhIh2'
write(98,*) 	theta_FwFw2,'	theta_FwFw2'
write(98,*) 	theta_FwIw2,'	theta_FwIw2'
write(98,*) 	theta_IwFw2,'	theta_IwFw2'
write(98,*) 	theta_IwIw2,'	theta_IwIw2'
write(98,*) 	Xptransfer	,'Xptransfer'
write(98,*) 	theta_eduFh	,'theta_eduFh'
write(98,*) 	theta_eduIh,'	theta_eduIh'
write(98,*) 	theta_eduFw,'	theta_eduFw'
write(98,*) 	theta_eduIw,'	theta_eduIw'
write(98,*) 	theta_grad_Fh,'	theta_grad_Fh'
write(98,*) 	theta_grad_Fw	,'theta_grad_Fw'
write(98,*) 	theta_eduXPh	,'theta_eduXPh'
write(98,*) 	theta_eduXPw	,'theta_eduXPw'
write(98,*) 	sig_Fh,'	sig_Fh'
write(98,*) 	sig_Ih	,'sig_Ih'
write(98,*) 	sig_Fw	,'sig_Fw'
write(98,*) 	sig_Iw	,'sig_Iw'
write(98,*) 	gamma	,'gamma'
write(98,*) 	gamma	,'gamma'
write(98,*) 	gammacov(1),'	gammacov(1)'
write(98,*) 	gammacov(2),'	gammacov(2)'
write(98,*) 	gammaXP,'	gammaXP'
write(98,*) 	gammaXP,'	gammaXP'
write(98,*) 	gammaedu(1),'	gammaedu(1)'
write(98,*) 	gammaedu(2),'	gammaedu(2)'
write(98,*) 	gammaage(1),'	gammaage(1)'
write(98,*) 	gammaage(2),'	gammaage(2)'
write(98,*) 	nparams,'	nparams'
write(98,*) 	ndraws_st,'	ndraws_st'
write(98,*) 	nspl,'	nspl'
write(98,*) 	ndraws_eps,'	ndraws_eps'
write(98,*) 	curv_a,'	curv_a'
write(98,*) 	curv_B,'	curv_B'
write(98,*) 	nsav,'	nsav'
write(98,*) 	death,'	death'
write(98,*) 	retirement,'	retirement'
write(98,*) 	startwork,'	startwork'
write(98,*) 	startrec,'	startrec'
write(98,*) 	endrec,'	endrec'
write(98,*) 	cmin,'	cmin'
write(98,*) 	scale,'	scale'
write(98,*) 	sav_min,'	sav_min'
write(98,*) 	lny_min	,'lny_min'
write(98,*) 	a_min(1),'	a_min'
write(98,*) 	B_min(1),'	B_min'
write(98,*) 	bar0,'	bar0'
write(98,*) 	abar1,'	abar1'
write(98,*) 	abar2,'	abar2'
write(98,*) 	r,'	r'
write(98,*) 	r_B_bar,'	r_B_bar'
write(98,*) 	sig_ret	,'sig_ret'

close(98)

endif


endsubroutine AppspackIn

subroutine AppspackOut

use params
use filenames
implicit none

if (lenovo==1.or.x201==1) then
open(unit=2,file=path//'JLSout.1',position='rewind')
else
open(unit=2,file=path//JLSout,position='rewind')
endif

write(2,*) criterion
close(2)


endsubroutine AppspackOut

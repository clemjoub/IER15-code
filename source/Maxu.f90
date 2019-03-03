subroutine maxu(a,Bh,Bw,edu_h,edu_w,cohort,XP_Fh,XP_Ih,XP_Fw,XP_Iw,&
&				XP_Fh_comp,XP_Ih_comp,XP_Fw_comp,XP_Iw_comp,&
&				Eps_Fh,Eps_Ih,Eps_Fw,Eps_Iw,pastd_h,pastd_w)

use params
implicit none

!*********************************************************************************
!
!	Given state (a,Bh,Bw) find decisions (ds,sav) that maximize V
!	Store maximand (bestV), maximizers (bestsav, bestd) and other vbs at optimum 
!   (besty,bestap,bestBp,bestc)
!	
!*********************************************************************************

!*********************************************************************************
!! LOCAL VARIABLES
integer::			idec							!decision index
double precision::	ut, mut, temp					!utility and marginal utility
double precision::  com_h,com_w                     !pension account fees

!! STATE VARIABLES
double precision::	a								! Savings
double precision::	Bh,Bw							! retirement savings
double precision::	Eps_Fh, Eps_Ih, Eps_Fw, Eps_Iw	! Wage Offer shocks
integer::			edu_h,edu_w						! Education attainments
integer::			edu_hp,edu_wp					! Next period's Education 
integer::			cohort							! Birth cohort
integer::			XP_Fh,XP_Ih,XP_Fw,XP_Iw			! Sector-specific experience
double precision::	XP_Fh_comp,XP_Ih_comp
double precision::  XP_Fw_comp,XP_Iw_comp			! XP component in wage eqn
integer::			XP_Fhp,XP_Ihp,XP_Fwp,XP_Iwp		! Next period's XP
integer::			pastd_h,pastd_w					! Previous period labor choice
integer::			pastd_hp,pastd_wp				! Past sector choices
double precision::	retbal_hh, retbal_ww
!*********************************************************************************

!Initialize benefit variables

H_overMP=0
H_elig=0
H_nonel=0
H_MPbenef=0.0
H_WPbenef=0.0
APS_h=0.0
W_overMP=0
W_elig=0
W_nonel=0
W_MPbenef=0.0
W_WPbenef=0.0
APS_w=0.0
retbal_hh=0.0
retbal_ww=0.0

bestV=-99999999999999999
besty=0.0
bestsav=0.0
bestd=0
idec=0
Vfun=-99999999999999999

!!Determine whether husband/wife gets a covered offer
call Covered_offer(pastd_h,XP_Fh,edu_h,1,offer(1))
call Covered_offer(pastd_w,XP_Fw,edu_w,2,offer(2))


WORKLOOP: do ds=1,9 

		if ((ds==1 .or. ds==2 .or. ds==3).and. offer(1)==0) cycle WORKLOOP
		if ((ds==1 .or. ds==4 .or. ds==7).and. offer(2)==0) cycle WORKLOOP
		if (age>retirement-1 .and. ds<9) cycle WORKLOOP

		EV=0.0
		ap=0.0
		Bhp=0.0
		Bwp=0.0
		XP_Fhp=0
		XP_Ihp=0
		XP_Fwp=0
		XP_Iwp=0
		

		!!Get income and asset holdings given labor choice
		call get_income(Eps_Fh, Eps_Ih, Eps_Fw, Eps_Iw,a,edu_h,edu_w,cohort,XP_Fh_comp,XP_Ih_comp,XP_Fw_comp,XP_Iw_comp)	
		holdings=a*(1+r)+y
		
		!!Update next period's experiences and lagged labor choice

		if (ds==1 .or. ds==2 .or. ds==3) then
			H_formal=1
			H_informal=0
			H_inactive=0
			XP_Fhp=XP_Fh+1
			pastd_hp=1
		elseif (ds==4 .or. ds==5 .or. ds==6) then
			H_formal=0
			H_informal=1
			H_inactive=0
			XP_Ihp=XP_Ih+1
			pastd_hp=2
		else
			H_formal=0
			H_informal=0
			H_inactive=1
			pastd_hp=3
		endif
		if (ds==1 .or. ds==4 .or. ds==7) then
			W_formal=1
			W_informal=0
			W_inactive=0
			XP_Fwp=XP_Fw+1
			pastd_wp=1
		elseif (ds==2 .or. ds==5 .or. ds==8) then
			W_formal=0
			W_informal=1
			w_inactive=0
			XP_Iwp=XP_Iw+1
			pastd_wp=2
		else
			W_formal=0
			W_informal=0
			W_inactive=1
			pastd_wp=3
		endif

		!! Compute switching cost
		if ((pastd_h==2 .and. (ds==1 .or. ds==2 .or. ds==3)).or. &
		&   (pastd_h==1 .and. (ds==4 .or. ds==5 .or. ds==6))) then
			s_cost=h_switch_sect
		elseif ((pastd_h==3 .and. (ds==1 .or. ds==2.or.ds==3.or. &
		&                          ds==4.or.ds==5.or.ds==6))) then 
			s_cost=h_switch_act
		else
			s_cost=0.0
		endif
		if ((pastd_w==2 .and. (ds==1.or.ds==4.or.ds==7)).or. &
		&   (pastd_w==1 .and. (ds==2.or.ds==5.or.ds==8))) then
			s_cost=s_cost+w_switch_sect
		elseif ((pastd_w==3 .and. (ds==1.or.ds==2.or.ds==4.or. &
		&                          ds==5.or.ds==7.or.ds==8))) then 
			s_cost=s_cost+w_switch_act
		endif

		!! Get next period's state given labor decision
		Bhp=max(0.0,Bh*(1+r_B)+contr_h-fixedcom-fixedbalcom(cohort,age)-varcom_h-Bh*varbalcomrt(cohort,age))
		Bwp=max(0.0,Bw*(1+r_B)+contr_w-fixedcom-fixedbalcom(cohort,age)-varcom_w-Bw*varbalcomrt(cohort,age))
		retbal_h=Bhp
		retbal_w=Bwp

		if (age==retirement-1) then	
		Bhp=0.0
		Bwp=0.0
		endif

		if (age<startrec) then
			call regressors2(Bhp,Bwp,age+1,edu_h,edu_w,cohort, &
			&                XP_Fhp,XP_Ihp,XP_Fwp,XP_Iwp,pastd_hp,pastd_wp)
			call matpd(1,nreg2-nreg1,1,theta_Vp(1,nreg1+1:nreg2), &
			&          reg_mat(nreg1+1:nreg2),EV2(1,1))
		endif

		!!	Make sure we don't extrapolate the continuation value
		if (Bhp>Bh_max(age+1)) then
			holdings=holdings+Bhp-Bh_max(age+1)+0.01
			Bhp=Bh_max(age+1)-0.01
			retbal_h=Bh_max(age+1)
		endif
		if (Bwp>Bw_max(age+1)) then
			holdings=holdings+Bwp-Bw_max(age+1)+0.01
			Bwp=Bw_max(age+1)-0.01
			retbal_w=Bw_max(age+1)
		endif
		if (Bhp+Bwp>wealth_max(age+1)) then
			holdings=holdings+Bhp+Bwp-wealth_max(age+1)+0.01
			temp=Bhp+Bwp-wealth_max(age+1)
			Bhp=Bhp-0.5*temp-0.005
			Bwp=Bwp-0.5*temp-0.005
			retbal_h=Bhp
			retbal_w=Bwp
		endif

		SAVINGSLOOP: do isav=1,nsav



					!!Compute consumption and update future assets 
					sav=grid_sav(isav)
					c=(holdings)*(1-sav)
!					ap=max(10.0*a_min(age),holdings*sav)
					ap=holdings-c
					
					if (ap<a_min(age+1)) cycle SAVINGSLOOP
					if (ap+retbal_h+retbal_w<wealth_min(age+1)) cycle SAVINGSLOOP
					if (ap>a_max(age+1)) cycle SAVINGSLOOP
					if (ap+retbal_h+retbal_w>wealth_max(age+1)) cycle SAVINGSLOOP !why this??

					idec=idec+1

					if (age==retirement-1) then	

					H_overMP=0
					H_elig=0
					H_nonel=0
					H_MPbenef=0.0
					H_WPbenef=0.0
					APS_h=0.0
					W_overMP=0
					W_elig=0
					W_nonel=0
					W_MPbenef=0.0
					W_WPbenef=0.0
					APS_w=0.0
					retbal_hh=0.0
					retbal_ww=0.0

						!!Minimum Pension benefits
						if (retbal_h>MP.and.XP_Fh.ge.MPyrs) then
							H_overMP=1
							H_MPbenef=0.0
							retbal_hh=retbal_h
						elseif	(retbal_h.le.MP.and.XP_Fh.ge.MPyrs) then
							H_elig=1
							H_MPbenef=MP-retbal_h
							retbal_hh=MP
						elseif  (XP_Fh<MPyrs) then
							H_nonel=1
							H_MPbenef=0.0
							retbal_hh=retbal_h
						endif

						if (retbal_w>MP.and.XP_Fw.ge.MPyrs) then
							W_overMP=1
							W_MPbenef=0.0
							retbal_ww=retbal_w
						elseif	(retbal_w.le.MP.and.XP_Fw.ge.MPyrs) then
							W_elig=1
							W_MPbenef=MP-retbal_w
							retbal_ww=MP
						elseif  (XP_Fw<MPyrs) then
							W_nonel=1
							W_MPbenef=0.0
							retbal_ww=retbal_w
						endif
						

						!!Welfare Pension benefits:
						if (H_nonel==1.and.retbal_h.lt.WP.and.ap+retbal_h+retbal_w<2*WP) then 
							H_WPbenef=WP-retbal_h
							retbal_hh=WP
						endif
						if (W_nonel==1.and.retbal_w.lt.WP.and.ap+retbal_h+retbal_w<2*WP) then 
							W_WPbenef=WP-retbal_w
							retbal_ww=WP
						endif

						!!PBS and APS benefits
						if (ap+retbal_h+retbal_w.lt.p60 .and. retbal_h<PMAS) then
							APS_h=max(0.0,PBS*(1-retbal_h/PMAS))
							retbal_hh=retbal_h+APS_h 
						endif
						if (ap+retbal_h+retbal_w.lt.p60 .and. retbal_w<PMAS) then
							APS_w=max(0.0,PBS*(1-retbal_w/PMAS))
							retbal_ww=retbal_w+APS_w 
						endif
					ap=ap+retbal_hh+retbal_ww
					endif
					

					!!On the 1st period use analytical continuation value 
					if (age==startrec .and. age+1>=retirement) then
						call analytical_V(ap,Bhp,Bwp,age+1,death,betaa(typ),r,raison(typ),raison2(typ),sigma(typ),delta(typ,9),EV(1,1))
					

					!!Thereafter compute the interpolated continuation value
					elseif (age<startrec) then

						call regressors3(ap,Bhp,Bwp,age+1,edu_h,edu_w,cohort,XP_Fhp,XP_Ihp,XP_Fwp,XP_Iwp,pastd_hp,pastd_wp)
!						call fitted_emax (theta_Vp,newreg,EV,nreg)
!						call fitted_emax (theta_Vp,reg_mat,EV,nreg)
						call matpd(1,nreg-nreg2,1,theta_Vp(1,nreg2+1:nreg),reg_mat(nreg2+1:nreg),EV3(1,1))
						EV=EV1+EV2+EV3
					endif

					!!Compute the total utility for the decision
					call util(c,ut,mut)
					
					Vfun(idec)=ut+pref_shock(ds)-s_cost+betaa(typ)*EV(1,1)
					
                    
					!!Keep utility maximizing decision

					if (Vfun(idec)>bestV .or. (isav==1 .and. ds==1)) then   
						bestV=Vfun(idec)

						!! Can turn those off
						bestsav=sav
						besty=y
						bestw_h=w_h
						bestw_w=w_w
						bestlnw_h=lnw_h
						bestlnw_w=lnw_w
						bestd=ds
						bestap=ap
						bestBhp=Bhp
						bestBwp=Bwp
						bestc=c

						if (age==retirement-1.and.simuldum==1) then	
							if (H_elig==1) then
							continue
							endif

							simH_elig	(h,1)	=	H_elig
							simW_elig	(h,1)	=	W_elig
							simH_nonel	(h,1)	=	H_nonel
							simW_nonel	(h,1)	=	W_nonel
							simH_overMP	(h,1)	=	H_overMP
							simW_overMP	(h,1)	=	W_overMP
							simH_MPbenef(h,1)	=	H_MPbenef
							simW_MPbenef(h,1)	=	W_MPbenef
							simH_WPbenef(h,1)	=	H_WPbenef
							simW_WPbenef(h,1)	=	W_WPbenef
							simH_APS	(h,1)	=	APS_h
							simW_APS	(h,1)	=	APS_w
						endif
 					endif

!!	*******************************************************************************************

			enddo SAVINGSLOOP
enddo WORKLOOP

end subroutine maxu

subroutine Covered_offer(pastd,XP,edu,gender,offerr)
use params

!********************************************************************************
integer:: pd
integer,intent(IN)::pastd,XP,edu,gender	
integer,intent(OUT):: offerr

!! LOCAL VARIABLES
double precision	z,ez,p,eps(1)
!********************************************************************************


offerr=0
if (pastd>1) then
pd=0 
elseif (pastd==1) then
pd=1
endif

z=gamma(gender)+gammacov(gender)*pd+gammaXP(gender)*XP+gammaedu(gender)*edu+ &
& gammaage(gender)*age
ez=dexp(-z)
p=1/(1+ez)

call ran1(idum,eps,1)
if (eps(1)<p) then
offerr=1
endif

endsubroutine 


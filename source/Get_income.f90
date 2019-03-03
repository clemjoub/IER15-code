subroutine get_income(Eps_Fh, Eps_Ih, Eps_Fw, Eps_Iw,a,edu_h,edu_w,cohort,XP_Fh_comp,XP_Ih_comp,XP_Fw_comp,XP_Iw_comp)

!***********************************************************************************************************************************

use params
implicit none

!***********************************************************************************************************************************

double precision::				Eps_Fh, Eps_Ih, Eps_Fw, Eps_Iw				! Wage Offer shocks
double precision::				a,Bh,Bw										! Assets
double precision::				taxable_wages								! Taxable labor income
double precision::				taxable_pension								! Taxable pension income
double precision::				tao_h, tao_w								! Effective contribution rate
integer::						edu_h,edu_w									! Completed Schooling
integer::						cohort										! Birth Cohort
double precision::				XP_Fh_comp,XP_Ih_comp,XP_Fw_comp,XP_Iw_comp	! Sector-specific experience
integer::						XP_Fh,XP_Ih,XP_Fw,XP_Iw								! Sector-specific experience
double precision				tax											! Tax function

!***********************************************************************************************************************************


if (age .ge. retirement) then
	lnw_h=lny_min
	contr_h=0.0
	lnw_w=lny_min
	contr_w=0.0

elseif (age .lt. retirement) then

	if	   (ds==1) then
		lnw_h=SR_Fh+theta_cohort_F*cohort+age_comp(typ,age)+XP_Fh_comp+theta_eduFh*edu_h+Eps_Fh 
		tao_h=tao
		lnw_w=SR_Fw+theta_cohort_F*cohort+g_gap+age_comp(typ,age)+XP_Fw_comp+theta_eduFw*edu_w+Eps_Fw
		tao_w=tao
	elseif (ds==2) then
		lnw_h=SR_Fh+theta_cohort_F*cohort+age_comp(typ,age)+XP_Fh_comp+theta_eduFh*edu_h+Eps_Fh 
		tao_h=tao
		lnw_w=SR_Iw+theta_cohort_I*cohort+g_gap+age_comp(typ,age)+XP_Iw_comp+theta_eduIw*edu_w+Eps_Iw
		tao_w=0.0
	elseif (ds==3) then
		lnw_h=SR_Fh+theta_cohort_F*cohort+age_comp(typ,age)+XP_Fh_comp+theta_eduFh*edu_h+Eps_Fh 
		tao_h=tao
		lnw_w=lny_min
		tao_w=0.0
	elseif (ds==4) then
		lnw_h=SR_Ih+theta_cohort_I*cohort+age_comp(typ,age)+XP_Ih_comp+theta_eduIh*edu_h+Eps_Ih 
		tao_h=0.0
		lnw_w=SR_Fw+theta_cohort_F*cohort+g_gap+age_comp(typ,age)+XP_Fw_comp+theta_eduFw*edu_w+Eps_Fw
		tao_w=tao
	elseif (ds==5) then
		lnw_h=SR_Ih+theta_cohort_I*cohort+age_comp(typ,age)+XP_Ih_comp+theta_eduIh*edu_h+Eps_Ih 
		tao_h=0.0
		lnw_w=SR_Iw+theta_cohort_I*cohort+g_gap+age_comp(typ,age)+XP_Iw_comp+theta_eduIw*edu_w+Eps_Iw
		tao_w=0.0
	elseif (ds==6) then
		lnw_h=SR_Ih+theta_cohort_I*cohort+age_comp(typ,age)+XP_Ih_comp+theta_eduIh*edu_h+Eps_Ih 
		tao_h=0.0
		lnw_w=lny_min
		tao_w=0.0
	elseif (ds==7) then
		lnw_h=lny_min
		tao_h=0.0
		lnw_w=SR_Fw+theta_cohort_F*cohort+g_gap+age_comp(typ,age)+XP_Fw_comp+theta_eduFw*edu_w+Eps_Fw
		tao_w=tao
	elseif (ds==8) then
		lnw_h=lny_min
		tao_h=0.0
		lnw_w=SR_Iw+theta_cohort_I*cohort+g_gap+age_comp(typ,age)+XP_Iw_comp+theta_eduIw*edu_w+Eps_Iw
		tao_w=0.0
	elseif (ds==9) then
		lnw_h=lny_min
		tao_h=0.0
		lnw_w=lny_min
		tao_w=0.0
	endif

endif

w_h=dexp(lnw_h)
w_w=dexp(lnw_w)

!! Commissions
contr_h=min(tope,w_h*tao_h)
contr_w=min(tope,w_w*tao_w)
varcom_h=w_h*varcomrt
varcom_w=w_w*varcomrt
taxable_wages=w_h*(1-tao_h)*H_formal+w_w*(1-tao_w)*W_formal
y=w_h*(1-tao_h)+w_w*(1-tao_w)-tax(taxable_wages+r*a)

end subroutine get_income


!***********************************************************************************************************************************


function tax(taxable)

!***********************************************************************************************************************************

use params
implicit none

!***********************************************************************************************************************************

integer::					br						!index for tax brackets
double precision::			tax						!taxes due
double precision::			taxable					!taxable income

!***********************************************************************************************************************************

tax=0.0
if (tax_toggle==1) then
	tax=taxable*tax_rate
elseif (tax_toggle==2) then
	do br=1,nbracket
		if ((bracket_min(br).lt.taxable) .and.(bracket_max(br).ge.taxable)) then
			tax=bracket_rate(br)*taxable-bracket_adj(br)
			exit
		endif
	enddo
endif

endfunction tax

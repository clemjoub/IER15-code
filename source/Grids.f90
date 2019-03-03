subroutine Grids

use params
implicit none

double precision:: length_a(100), length_B(100),costransform,temp
integer:: cohort
!1. Grid for non retirement assets (grid_a)
length_a=a_max-a_min

do age=startdraw,death
do ia=1,na
	grid_a(age,ia)=a_min(age)+length_a(age)*((ia-1.0)/(na-1.0))**curv_a
end do
end do

!2. Grid for pension account balances (grid_B)

length_B=B_max-B_min
do age=startdraw,death
do iB=1,nB
	grid_B(age,iB)=B_min(age)+length_B(age)*((iB-1.0)/(nB-1.0))**curv_B
end do
end do

!4. Grid for saving rates(grid_sav)
grid_sav(1)=sav_min
do isav=2,nsav
!	grid_sav(isav)=sin((3.141695/2)*(isav/(1.0*nsav+1)))
	temp=isav/(1.0*nsav+1)
	grid_sav(isav)=1-costransform(temp)
end do

!5. Grid for wage offers age profile 

do age=1,death
do typ=1,ntypes
age_comp(typ,age)=alpha(typ,1)+alpha(typ,2)*(age-startwork+1)+alpha(typ,3)*(age-startwork+1)*(age-startwork+1)
enddo
enddo


!! Compute some variables

theta_FhIh=XPtransfer*theta_FhFh
theta_IhFh=XPtransfer*theta_IhIh
theta_FwIw=XPtransfer*theta_FwFw
theta_IwFw=XPtransfer*theta_IwIw

!! Fill in variance-covariance matrix for shocks

evcv=0.0
evcv(1,1)=sig_Fh
evcv(1,2)=0.0
evcv(1,3)=0.0
evcv(1,4)=0.0
evcv(2,1)=0.0
evcv(2,2)=sig_Ih
evcv(2,3)=0.0
evcv(2,4)=0.0
evcv(3,1)=0.0
evcv(3,2)=0.0
evcv(3,3)=sig_Fw
evcv(3,4)=0.0
evcv(4,1)=0.0
evcv(4,2)=0.0
evcv(4,3)=0.0
evcv(4,4)=sig_Iw
!evcv(5,5)=sig_pref
!evcv(6,6)=sig_pref
!evcv(7,7)=sig_pref
!evcv(8,8)=sig_pref
evcv(5,5)=sig_pref_w
evcv(6,6)=sig_pref_w
evcv(7,7)=sig_pref_h
evcv(8,8)=sig_pref_h
evcv(9,9)=sig_pref_h+sig_pref_w
evcv(10,10)=sig_ret


! min pension requirements to determine if  qualify for early retirement
earlycut=999999.0d-0
earlycut(64) = 13.682941d-0
earlycut(63) = 14.021373d-0
earlycut(62) = 14.650655d-0
earlycut(61) = 14.943005d-0
earlycut(60) = 15.221433d-0
earlycut(59) = 15.486603d-0
earlycut(58) = 15.739146d-0
earlycut(57) = 15.979663d-0
earlycut(56) = 15.979663d-0
earlycut(55) = 16.208727d-0
earlycut(54) = 16.426882d-0
earlycut(53) = 16.634650d-0
earlycut(52) = 16.832524d-0
earlycut(51) = 17.020975d-0
earlycut(50) = 17.200452d-0
earlycut(49) = 17.371383d-0
earlycut(48) = 17.534174d-0
earlycut(47) = 17.689214d-0
earlycut(46) = 17.836870d-0
earlycut(45) = 17.977495d-0
        
end subroutine


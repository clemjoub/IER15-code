subroutine Counterfactuals

use params
use filenames
implicit none

integer::                       temp,i


!! Baseline
if (counterfactual==0.0) then
ctag="Baseline/"
tao			=	0.1
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0

!! 2008 Reform
elseif (counterfactual==1.0) then
ctag='2008Reform/'
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	30
realizedret(28:100)=0.05976
notax=0

!! Topup
elseif (counterfactual==2.0) then
ctag='Topup/'
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	9
realizedret(28:100)=0.05976
notax=0

!! Universal
elseif (counterfactual==3.0) then
ctag='Univ/'
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	99999999
realizedret(28:100)=0.05976
notax=0

!! High Taper
elseif (counterfactual==4.0) then
ctag='Hightaper/'
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	15
realizedret(28:100)=0.05976
notax=0


!! 2008 Reform
elseif (counterfactual==5.0) then
ctag='2008Reform2/'
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	9999
PBS			=	9
PMAS		=	30
realizedret(28:100)=0.05976
notax=0

!! Topup
elseif (counterfactual==6.0) then
ctag='Topup2/'
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	9
realizedret(28:100)=0.05976
notax=0

!! Universal
elseif (counterfactual==7.0) then
ctag='Univ2/'
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	9999
PBS			=	9
PMAS		=	99999999
realizedret(28:100)=0.05976
notax=0

!! High Taper
elseif (counterfactual==8.0) then
ctag='Hightaper2/'
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	9999
PBS			=	9
PMAS		=	15
realizedret(28:100)=0.05976
notax=0


elseif (counterfactual==0.05) then
ctag='tao5/'
tao			=	0.05
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0

elseif (counterfactual==0.075) then
ctag='tao75/'
tao			=	0.075
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0

!! Different contribution rates
elseif (counterfactual==0.125) then
ctag='tao125/'
tao			=	0.125
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0

!! Different contribution rates
elseif (counterfactual==0.15) then
ctag='tao15/'
tao			=	0.15
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0

elseif (counterfactual==0.175) then
ctag='tao175/'
tao			=	0.175
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0



elseif (counterfactual==0.20) then
ctag='tao20/'
tao			=	0.2
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0


elseif (counterfactual==0.25) then
ctag='tao25/'
tao			=	0.25
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0


!! Base + Covered offer proba == 1
elseif (counterfactual==1100.0) then
ctag="nobarriers/"
tao			=	0.1
MPyrs       =	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
gamma(1)=1000.0
gamma(2)=1000.0
notax=0


!! No MPG
elseif (counterfactual==1010.0) then
ctag="noMPG/"
tao			=	0.1
MPyrs       =	100
MP			=	0.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0

!! No MPG, no contributions
elseif (counterfactual==1011.0) then
ctag="nocont/"
tao			=	0.001
MPyrs       =	100
MP			=	0.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0


!! No MPG, tao 1%
elseif (counterfactual==1012.0) then
ctag="tao1noMPG/"
tao			=	0.01
MPyrs       =	100
MP			=	0.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0


!! No MPG, tao 2%
elseif (counterfactual==1013.0) then
ctag="tao25noMPG/"
tao			=	0.025
MPyrs       =	100
MP			=	0.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0


!! No MPG, tao 5
elseif (counterfactual==1014.0) then
ctag="tao5noMPG/"
tao			=	0.05
MPyrs       =	100
MP			=	0.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0


!! No MPG, tao 0.75
elseif (counterfactual==1015.0) then
ctag="tao75noMPG/"
tao			=	0.075
MPyrs       =	100
MP			=	0.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0


!! No MPG, tao 12.5
elseif (counterfactual==1016.0) then
ctag="tao125noMPG/"
tao			=	0.125
MPyrs       =	100
MP			=	0.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0


!! No MPG, tao 15
elseif (counterfactual==1017.0) then
ctag="tao15noMPG/"
tao			=	0.15
MPyrs       =	100
MP			=	0.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0


!! No MPG, tao 17.5
elseif (counterfactual==1018.0) then
ctag="tao175noMPG/"
tao			=	0.175
MPyrs       =	100
MP			=	0.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0

!! No MPG, tao 20
elseif (counterfactual==1019.0) then
ctag="tao20noMPG/"
tao			=	0.2
MPyrs       =	100
MP			=	0.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0

!! Base + everything the same across sectors
elseif (counterfactual==1001.0) then
ctag="evthgsame/"

tao			=	0.1
MPyrs       =	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976

!taxes
notax=1

! segmentation
gamma(1)=1000.0
gamma(2)=1000.0

! comparative advantage
do i=1,ntypes
temp=0.5*(SR_Ih(i)+SR_Fh(i))
SR_Ih(i)=temp
SR_Fh(i)=temp
temp=0.5*(SR_Iw(i)+SR_Fw(i))
SR_Iw(i)=temp
SR_Fw(i)=temp
enddo

temp=0.5*(theta_eduFh+theta_eduIh)
theta_eduIh=temp
theta_eduFh=temp
temp=0.5*(theta_eduFw+theta_eduIw)
theta_eduIw=temp
theta_eduFw=temp
theta_grad_Fh=0
theta_grad_Fw=0

! shock variances
sameshocks=0
temp=0.5*(sig_Fh+sig_Ih)
sig_Fh=temp
sig_Ih=temp
temp=0.5*(sig_Fw+sig_Iw)
sig_Fw=temp
sig_Iw=temp

! human capital
temp=0.5*(theta_FhFh+theta_IhIh)
theta_FhFh=temp
theta_IhIh=temp
temp=0.5*(theta_FwFw+theta_IwIw)
theta_FwFw=temp
theta_IwIw=temp
XPtransfer=1

!preferences
!preferences
do i=1,ntypes
raw_delta(i,2)=raw_delta(i,1)
raw_delta(i,4)=raw_delta(i,1)
raw_delta(i,5)=raw_delta(i,1)
raw_delta(i,6)=raw_delta(i,3)
raw_delta(i,8)=raw_delta(i,7)
enddo

!! Base + everything the same across sectors
elseif (counterfactual==1002.0) then
ctag="compadv/"
tao			=	0.1
MPyrs       =	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0

!! segmentation
!gamma(1)=1000.0
!gamma(2)=1000.0

! comparative advantage
do i=1,ntypes
temp=0.5*(SR_Ih(i)+SR_Fh(i))
SR_Ih(i)=temp
SR_Fh(i)=temp
temp=0.5*(SR_Iw(i)+SR_Fw(i))
SR_Iw(i)=temp
SR_Fw(i)=temp
enddo

temp=0.5*(theta_eduFh+theta_eduIh)
theta_eduIh=temp
theta_eduFh=temp
temp=0.5*(theta_eduFw+theta_eduIw)
theta_eduIw=temp
theta_eduFw=temp
theta_grad_Fh=0
theta_grad_Fw=0

! shock variances
sameshocks=0
temp=0.5*(sig_Fh+sig_Ih)
sig_Fh=temp
sig_Ih=temp
temp=0.5*(sig_Fw+sig_Iw)
sig_Fw=temp
sig_Iw=temp

!! human capital
!temp=0.5*(theta_FhFh+theta_IhIh)
!theta_FhFh=temp
!theta_IhIh=temp
!temp=0.5*(theta_FwFw+theta_IwIw)
!theta_FwFw=temp
!theta_IwIw=temp
!XPtransfer=1

!!preferences
!do i=1,ntypes
!temp=0.5*(raw_delta(i,1)+raw_delta(i,2))
!raw_delta(i,1)=temp
!raw_delta(i,2)=temp
!temp=0.5*(raw_delta(i,4)+raw_delta(i,5))
!raw_delta(i,4)=temp
!raw_delta(i,5)=temp
!enddo


!! Base + everything the same across sectors
elseif (counterfactual==1003.0) then
ctag="prefs/"
tao			=	0.1
MPyrs       =	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0
!! segmentation
!gamma(1)=1000.0
!gamma(2)=1000.0


! comparative advantage
!do i=1,ntypes
!temp=0.5*(SR_Ih(i)+SR_Fh(i))
!SR_Ih(i)=temp
!SR_Fh(i)=temp
!temp=0.5*(SR_Iw(i)+SR_Fw(i))
!SR_Iw(i)=temp
!SR_Fw(i)=temp
!enddo

!temp=0.5*(theta_eduFh+theta_eduIh)
!theta_eduIh=temp
!theta_eduFh=temp
!temp=0.5*(theta_eduFw+theta_eduIw)
!theta_eduIw=temp
!theta_eduFw=temp
!theta_grad_Fh=0
!theta_grad_Fw=0
!
!! shock variances
!sameshocks=0
!temp=0.5*(sig_Fh+sig_Ih)
!sig_Fh=temp
!sig_Ih=temp
!temp=0.5*(sig_Fw+sig_Iw)
!sig_Fw=temp
!sig_Iw=temp

!! human capital
!temp=0.5*(theta_FhFh+theta_IhIh)
!theta_FhFh=temp
!theta_IhIh=temp
!temp=0.5*(theta_FwFw+theta_IwIw)
!theta_FwFw=temp
!theta_IwIw=temp
!XPtransfer=1

!preferences
do i=1,ntypes
raw_delta(i,2)=raw_delta(i,1)
raw_delta(i,4)=raw_delta(i,1)
raw_delta(i,5)=raw_delta(i,1)
raw_delta(i,6)=raw_delta(i,3)
raw_delta(i,8)=raw_delta(i,7)
enddo

!! Base + everything the same across sectors
elseif (counterfactual==1004.0) then
ctag="humancap/"
tao			=	0.1
MPyrs       =	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
notax=0

!! segmentation
!gamma(1)=1000.0
!gamma(2)=1000.0

!! comparative advantage
!do i=1,ntypes
!temp=0.5*(SR_Ih(i)+SR_Fh(i))
!SR_Ih(i)=temp
!SR_Fh(i)=temp
!temp=0.5*(SR_Iw(i)+SR_Fw(i))
!SR_Iw(i)=temp
!SR_Fw(i)=temp
!enddo

!temp=0.5*(theta_eduFh+theta_eduIh)
!theta_eduIh=temp
!theta_eduFh=temp
!temp=0.5*(theta_eduFw+theta_eduIw)
!theta_eduIw=temp
!theta_eduFw=temp
!theta_grad_Fh=0
!theta_grad_Fw=0
!
!! shock variances
!sameshocks=0
!temp=0.5*(sig_Fh+sig_Ih)
!sig_Fh=temp
!sig_Ih=temp
!temp=0.5*(sig_Fw+sig_Iw)
!sig_Fw=temp
!sig_Iw=temp

!! human capital
temp=0.5*(theta_FhFh+theta_IhIh)
theta_FhFh=temp
theta_IhIh=temp
temp=0.5*(theta_FwFw+theta_IwIw)
theta_FwFw=temp
theta_IwIw=temp
XPtransfer=1

!!preferences
!do i=1,ntypes
!temp=0.5*(raw_delta(i,1)+raw_delta(i,2))
!raw_delta(i,1)=temp
!raw_delta(i,2)=temp
!temp=0.5*(raw_delta(i,4)+raw_delta(i,5))
!raw_delta(i,4)=temp
!raw_delta(i,5)=temp
!enddo



!! Base + everything the same across sectors
elseif (counterfactual==1005.0) then
ctag="notax/"
tao			=	0.1
MPyrs       =	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976
print*,"done that"
!! taxes
notax=1

!! segmentation
!gamma(1)=1000.0
!gamma(2)=1000.0

!! comparative advantage
!do i=1,ntypes
!temp=0.5*(SR_Ih(i)+SR_Fh(i))
!SR_Ih(i)=temp
!SR_Fh(i)=temp
!temp=0.5*(SR_Iw(i)+SR_Fw(i))
!SR_Iw(i)=temp
!SR_Fw(i)=temp
!enddo

!temp=0.5*(theta_eduFh+theta_eduIh)
!theta_eduIh=temp
!theta_eduFh=temp
!temp=0.5*(theta_eduFw+theta_eduIw)
!theta_eduIw=temp
!theta_eduFw=temp
!theta_grad_Fh=0
!theta_grad_Fw=0
!
!! shock variances
!sameshocks=0
!temp=0.5*(sig_Fh+sig_Ih)
!sig_Fh=temp
!sig_Ih=temp
!temp=0.5*(sig_Fw+sig_Iw)
!sig_Fw=temp
!sig_Iw=temp

!!! human capital
!temp=0.5*(theta_FhFh+theta_IhIh)
!theta_FhFh=temp
!theta_IhIh=temp
!temp=0.5*(theta_FwFw+theta_IwIw)
!theta_FwFw=temp
!theta_IwIw=temp
!XPtransfer=1

!!preferences
!do i=1,ntypes
!temp=0.5*(raw_delta(i,1)+raw_delta(i,2))
!raw_delta(i,1)=temp
!raw_delta(i,2)=temp
!temp=0.5*(raw_delta(i,4)+raw_delta(i,5))
!raw_delta(i,4)=temp
!raw_delta(i,5)=temp
!enddo

endif

endsubroutine
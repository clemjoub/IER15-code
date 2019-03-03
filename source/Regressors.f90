subroutine regressors1(aage,edu_h,edu_w,cohort)

!***********************************************************************************************************************************

use params
implicit none

!***********************************************************************************************************************************

integer,intent(IN)::							aage,edu_h,edu_w,cohort		!State variables

!! LOCAL VARIABLES
integer::										ind,i,j
double precision::								ut,utbh,utbw,mut,utb

!***********************************************************************************************************************************


if (firstreg==1) then
nreg1=18
nreg2=49
nreg=103
allocate(reg			(			ndraws_st,	nreg,	death			))
allocate(regspl			(			nspl,		nreg					))
allocate(reg_mat		(						nreg					))
allocate(theta_Vp		(1,						nreg					))
allocate(theta_V		(1,						nreg					))
allocate(std_errors		(1,						nreg					))
allocate(theta_out		(	ntypes,						death,	nreg	))
allocate(std_errors_out	(								death,	nreg	))
firstreg=0
endif

!! Functions of education
maxedu=max(edu_h,edu_w)
sumedu=edu_h+edu_w
sumedu2=(sumedu==2)
sumedu3=(sumedu==3)
sumedu4=(sumedu==4)
sumedu5=(sumedu==5)
sumedu6=(sumedu==6)
sumedu7=(sumedu==7)
sumedu8=(sumedu==8)
highhhedu=(sumedu>5)
edu_h1=(edu_h==1)
edu_h2=(edu_h==2)
edu_h3=(edu_h==3)
edu_h4=(edu_h==4)
edu_w1=(edu_w==1)
edu_w2=(edu_w==2)
edu_w3=(edu_w==3)
edu_w4=(edu_w==4)
cohort2=(cohort==2)
cohort3=(cohort==3)
cohort4=(cohort==4)
cohort5=(cohort==5)




reg_mat(1)=   1.0 !*
reg_mat(2)=   edu_h2 !*
reg_mat(3)=   edu_h3 !*
reg_mat(3)=   edu_h4 !*
reg_mat(4)=   edu_w2 !*
reg_mat(5)=   edu_w3 !*
reg_mat(6)=  edu_h2*edu_w3 !*
reg_mat(7)=  edu_h2*edu_w4 !
reg_mat(8)=  edu_h3*edu_w2 !*
reg_mat(9)=  edu_h3*edu_w4 !*
reg_mat(10)=  cohort2 !
reg_mat(11)=  cohort3 !
reg_mat(12)=  cohort4 !
reg_mat(13)=  cohort5 !
reg_mat(14)=   sumedu3 !
reg_mat(15)=   sumedu4 !
reg_mat(16)=   sumedu5 !
reg_mat(17)=  sumedu6 !
reg_mat(18)=  sumedu7 !


end subroutine



subroutine regressors2(Bh,Bw,aage,edu_h,edu_w,cohort,XP_Fh,XP_Ih,XP_Fw,XP_Iw,pastd_h,pastd_w)

!***********************************************************************************************************************************

use params
implicit none

!***********************************************************************************************************************************

integer,intent(IN)::							aage,XP_Fh,XP_Ih,XP_Fw,XP_Iw,edu_h,edu_w,cohort		!State variables
double precision,intent(IN)::					Bh,Bw													!State variables
integer,intent(IN)::							pastd_h,pastd_w											! Past sector choices


!! LOCAL VARIABLES
integer::										ind,i,j
double precision::								ut,utbh,utbw,mut,utb

!***********************************************************************************************************************************


maxXP=max(XP_Fw,XP_Iw)+max(XP_Fh,XP_Ih)
maxXPhh=max(XP_Fw,XP_Iw,XP_Fh,XP_Ih)
maxearncapa=max(XP_Fh+5*edu_h,XP_Ih+5*edu_h,XP_Fw+5*edu_w,XP_Iw+5*edu_w)
maxearncapa2=maxearncapa*maxearncapa
maxhhearncapa=max(XP_Fh+5*edu_h,XP_Ih+5*edu_h)+max(XP_Fw+5*edu_w,XP_Iw+5*edu_w)
maxhhearncapa2=maxhhearncapa*maxhhearncapa
maxearncapahigh=(maxearncapa>19)
lowhhearnings=(maxXPhh.le.int(0.25*(age-startwork))+1 .and. maxedu<3)
highhhearnings=(maxXPhh.ge.0.75*(age-startwork) .and. maxedu>3)


reg_mat(19)= XP_Fh*1.0 !*
reg_mat(20)=edu_h4*XP_Fh !*
reg_mat(21)= XP_Ih*1.0 !
reg_mat(22)= XP_Fw*1.0 !`
reg_mat(23)=edu_w4*XP_Fw !`
reg_mat(24)= XP_Iw*1.0 !
reg_mat(25)= maxXP !`
reg_mat(26)= maxXPhh !`
reg_mat(27)= maxXPhh*maxXPhh !
reg_mat(28)= maxXPhh*sumedu  !`
reg_mat(29)= maxearncapa     !
reg_mat(30)= cohort2*maxearncapa !
reg_mat(31)= cohort3*maxearncapa !
reg_mat(32)= cohort4*maxearncapa !
reg_mat(33)= cohort5*maxearncapa !
reg_mat(34)= maxearncapa2 !
reg_mat(35)= cohort2*maxearncapa2 !
reg_mat(36)= cohort3*maxearncapa2 !
reg_mat(37)= cohort4*maxearncapa2 !
reg_mat(38)= cohort5*maxearncapa2 !
reg_mat(39)= maxhhearncapa !
reg_mat(40)= Bh !
reg_mat(41)=cohort2*Bh !
reg_mat(42)=cohort3*Bh !
reg_mat(43)=cohort4*Bh !
reg_mat(44)=cohort5*Bh !
reg_mat(45)= Bw !
reg_mat(46)=cohort2*Bw !
reg_mat(47)=cohort3*Bw !
reg_mat(48)=cohort4*Bw !
reg_mat(49)=cohort5*Bw !

end subroutine


subroutine regressors3(a,Bh,Bw,aage,edu_h,edu_w,cohort,XP_Fh,XP_Ih,XP_Fw,XP_Iw,pastd_h,pastd_w)

!***********************************************************************************************************************************

use params
implicit none

!***********************************************************************************************************************************

integer,intent(IN)::							aage,XP_Fh,XP_Ih,XP_Fw,XP_Iw,edu_h,edu_w,cohort				!State variables
double precision,intent(IN)::					a,Bh,Bw													!State variables
integer,intent(IN)::							pastd_h,pastd_w											!Past sector choices
integer::										pastd_h2,pastd_h3,pastd_w2,pastd_w3
double precision::								wealth
double precision::								logwealthp5, uwealthp1m1
double precision::								WPelig,WPeligxwealth,WPeligxa,WPeligxlogwealthp5
!! LOCAL VARIABLES
integer::										ind,i,j
double precision::								ut,uta,utbh,utbw,utwealth,mut, utb

!***********************************************************************************************************************************



wealth=a+Bh+Bw
wealth2=wealth*wealth
logwealthp5=log(wealth+5.0)
uwealthp1m1=1/(wealth+1.0)
uwealthp1m1xsumedu=uwealthp1m1*sumedu
maxearncapaxlogwealthp5=maxearncapa*logwealthp5
maxearncapaxlogwealthp52=maxearncapaxlogwealthp5*maxearncapaxlogwealthp5
pastd_h2=(pastd_h==2)
pastd_h3=(pastd_h==3)
pastd_w2=(pastd_w==2)
pastd_w3=(pastd_w==3)
WPelig=(wealth<12.0)
WPeligxwealth=WPelig*wealth
WPeligxa=WPelig*a
WPeligxlogwealthp5=WPelig*logwealthp5


reg_mat(50)= wealth2 !
reg_mat(51)= uwealthp1m1 !
reg_mat(52)=cohort2*uwealthp1m1 !
reg_mat(53)=cohort3*uwealthp1m1 !
reg_mat(54)=cohort4*uwealthp1m1 !
reg_mat(55)=cohort5*uwealthp1m1 !
reg_mat(56)=sumedu3*uwealthp1m1 !
reg_mat(57)=sumedu4*uwealthp1m1 !
reg_mat(58)=sumedu5*uwealthp1m1 !
reg_mat(59)=sumedu6*uwealthp1m1 !
reg_mat(60)=sumedu7*uwealthp1m1 !
reg_mat(61)= a !
reg_mat(62)=cohort2*a !
reg_mat(63)=cohort3*a !
reg_mat(64)=cohort4*a !
reg_mat(65)=cohort5*a !
reg_mat(66)= a*a !
reg_mat(67)= wealth*wealth2 !
reg_mat(68)= logwealthp5 !
reg_mat(69)=cohort2*logwealthp5 !
reg_mat(70)=cohort3*logwealthp5 !
reg_mat(71)=cohort4*logwealthp5 !
reg_mat(72)=cohort5*logwealthp5 !
reg_mat(73)= uwealthp1m1xsumedu !
reg_mat(74)=cohort2*uwealthp1m1xsumedu !
reg_mat(75)=cohort3*uwealthp1m1xsumedu !
reg_mat(76)=cohort4*uwealthp1m1xsumedu !
reg_mat(77)=cohort5*uwealthp1m1xsumedu !
reg_mat(78)= maxearncapaxlogwealthp5 !
reg_mat(79)=cohort2*maxearncapaxlogwealthp5 !
reg_mat(80)=cohort3*maxearncapaxlogwealthp5 !
reg_mat(81)=cohort4*maxearncapaxlogwealthp5 !
reg_mat(82)=cohort5*maxearncapaxlogwealthp5 !
reg_mat(83)= maxearncapaxlogwealthp52 !
reg_mat(84)=cohort2*maxearncapaxlogwealthp52 !
reg_mat(85)=cohort3*maxearncapaxlogwealthp52 !
reg_mat(86)=cohort4*maxearncapaxlogwealthp52 !
reg_mat(87)=cohort5*maxearncapaxlogwealthp52 !
reg_mat(88)= logwealthp5*maxXP !
reg_mat(89)= wealth*maxXP !
reg_mat(90)= maxearncapahigh*uwealthp1m1 !
reg_mat(91)= maxhhearncapa*logwealthp5 !
reg_mat(92)= lowhhearnings*logwealthp5 !
reg_mat(93)= highhhearnings*logwealthp5 !
reg_mat(94)= lowhhearnings*wealth2 !
reg_mat(95)= pastd_h2 !
reg_mat(96)= pastd_h3 !
reg_mat(97)= pastd_w2 !
reg_mat(98)= pastd_w3 ! *
reg_mat(99)=  edu_h4*edu_w2 !
reg_mat(100)= WPelig
reg_mat(101)= WPeligxwealth
reg_mat(102)= WPeligxa
reg_mat(103)= WPeligxlogwealthp5


end subroutine

subroutine get_toggle

use params
implicit none

integer::	i

toggle=0
if (age==startwork+1) then
do i=19,28
	toggle(i)=1
enddo
do i=89,94
	toggle(i)=1
enddo

endif
if (age==startwork+1) then
!	toggle(2)=1
!	toggle(3)=1
!	toggle(4)=1
!	toggle(5)=1
!	toggle(6)=1
!	toggle(7)=1
!	toggle(8)=1
endif
if (age<startwork+int((nwork)/2)+1) then
!	toggle(23)=1
!	toggle(24)=1
!	toggle(27)=1
!	toggle(30)=1
endif
if (age>=retirement) then
	toggle(8)=1
	toggle(9)=1
	toggle(10)=1
	toggle(11)=1
!	toggle(12)=1
	toggle(13)=1
	toggle(14)=1
	toggle(17)=1
	toggle(18)=1
	toggle(25)=1
	toggle(27)=1
	toggle(30)=1
!	toggle(31)=1
	toggle(12)=1
	toggle(26)=1
	toggle(25)=1
	toggle(27)=1
	toggle(28)=1
	toggle(29)=1
	toggle(15)=1
	toggle(16)=1
!	toggle(5)=1
!	toggle(6)=1
endif
endsubroutine





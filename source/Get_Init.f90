subroutine get_init

use params
use filenames
implicit none

integer:: i
double precision:: blah

open (10,file=foliofile,position='rewind')
open (30,file=H_edufile,position='rewind')
open (40,file=W_edufile,position='rewind')
open (50,file=firstyearfile,position='rewind')
open (60,file=lastyearfile,position='rewind')
!open (70,file=nfoliosfile,position='rewind')
!open (80,file=wealthagefile,position='rewind')
open (90,file=sexsampledfile,position='rewind')
open (100, file=cohortfile,position='rewind')
open (110, file=mdatafile,position='rewind')
open (120, file=wealthfile,position='rewind')
open (130, file=H_balancefile,position='rewind')
open (140, file=W_balancefile,position='rewind')
open (150, file=H_Xformalfile,position='rewind')
open (160, file=H_Xinformalfile,position='rewind')
open (165, file=H_yrsinactivefile,position='rewind')
open (170, file=W_Xformalfile,position='rewind')
open (180, file=W_Xinformalfile,position='rewind')
open (185, file=W_yrsinactivefile,position='rewind')
open (362, file=H_agefile,position='rewind')
open (363, file=pathdatavsmodel//'futurecohort.txt', position='rewind')

read (fmt=*,unit=10) folio(1:nhh1)
read (fmt=*,unit=30) H_edu(1:nhh1)
read (fmt=*,unit=40) W_edu(1:nhh1)
read (fmt=*,unit=50) first_year(1:nhh1)
read (fmt=*,unit=60) last_year(1:nhh1)
!read (fmt=*,unit=70) nhh(1:nhh1)
!read (fmt=*, unit=80) nwealth(1:nhh1)
read (fmt=*,unit=90) sexsampled(1:nhh1)
read (fmt=*,unit=100) birthcohort(1:nhh1)
!read (fmt=*,unit=110) mdata(1:nhh1)
read (fmt=*,unit=120) init_wealth(1:nhh1)
read (fmt=*,unit=130) init_H_bal(1:nhh1)
read (fmt=*,unit=140) init_W_bal(1:nhh1)
read (fmt=*,unit=150) init_H_Xformal(1:nhh1)
read (fmt=*,unit=160) init_H_Xinformal(1:nhh1)
read (fmt=*,unit=165) init_H_yrsinactive(1:nhh1)
read (fmt=*,unit=170) init_W_Xformal(1:nhh1)
read (fmt=*,unit=180) init_W_Xinformal(1:nhh1)
read (fmt=*,unit=185) init_W_yrsinactive(1:nhh1)
read (fmt=*,unit=362) init_age(1:nhh1)
read (fmt=*,unit=363) futurecohort(1:243)

close (10)
close (30)
close (40)
close (50)
close (60)
!close (70)
close (80)
close (90)
close (100)
close (110)
close (120)
close (130)
close (140)
close (150)
close (160)
close (165)
close (170)
close (180)
close (185)
close (362)
close (363)

!! Read the vector of nonmissing wealths at age of marriage
open (190, file=wealthpoolfile,position='rewind')
do i=1,nhh1
read (fmt=*, unit=190) init_wealth_dum(i), wealthpool(i)
enddo
close(190)

nwealthpool=sum(init_wealth_dum)

!For each clone, draw a value from the nonmissing wealths at age of marriage
call ran1(idum,draw_init,nclones*nhh)
init_draw=reshape(draw_init,(/nhh,nclones/))
do h=1,nhh
do i=1,nclones
if (init_draw(h,i)==1) init_draw(h,i)=0.99
if (init_wealth(h)==-99.or.i>1 ) then
	wealth_draw(h,i)=wealthpool(int(init_draw(h,i)*(nwealthpool)+1))
else
	wealth_draw(h,i)=init_wealth(h)
endif
enddo
enddo


!! When interviewee is the husband, randomly draw a match among hh where wife is interviewee, conditional on wife's education
!! Read the vector of matches conditional on education
open(200,file=W_match1file,position='rewind')
do i=1,nhh1
read(fmt=*,unit=200) W_match1_dum(i), W_match1(i)
enddo
close(200)
nW_match1=sum(W_match1_dum)

open(200,file=W_match2file,position='rewind')
do i=1,nhh1
read(fmt=*,unit=200) W_match2_dum(i), W_match2(i)
enddo
close(200)
nW_match2=sum(W_match2_dum)

open(200,file=W_match3file,position='rewind')
do i=1,nhh1
read(fmt=*,unit=200) W_match3_dum(i), W_match3(i)
enddo
close(200)
nW_match3=sum(W_match3_dum)

open(200,file=W_match4file,position='rewind')
do i=1,nhh1
read(fmt=*,unit=200) W_match4_dum(i), W_match4(i)
enddo
close(200)
nW_match4=sum(W_match4_dum)

open(200,file=H_match1file,position='rewind')
do i=1,nhh1
read(fmt=*,unit=200) H_match1_dum(i), H_match1(i)
enddo
close(200)
nH_match1=sum(H_match1_dum)

open(200,file=H_match2file,position='rewind')
do i=1,nhh1
read(fmt=*,unit=200) H_match2_dum(i), H_match2(i)
enddo
close(200)
nH_match2=sum(H_match2_dum)

open(200,file=H_match3file,position='rewind')
do i=1,nhh1
read(fmt=*,unit=200) H_match3_dum(i), H_match3(i)
enddo
close(200)
nH_match3=sum(H_match3_dum)

open(200,file=H_match4file,position='rewind')
do i=1,nhh1
read(fmt=*,unit=200) H_match4_dum(i), H_match4(i)
enddo
close(200)
nH_match4=sum(H_match4_dum)

do h=1,nhh1
do i=1,nclones
if (init_draw(h,i)==1) init_draw(h,i)=0.99
if (sexsampled(h)==1.and.W_edu(h)==1) then
	match_draw(h,i)=W_match1(int(init_draw(h,i)*(nW_match1)+1))
endif 
if (sexsampled(h)==1.and.W_edu(h)==2) then
	match_draw(h,i)=W_match2(int(init_draw(h,i)*(nW_match2)+1))
endif 
if (sexsampled(h)==1.and.W_edu(h)==3) then
	match_draw(h,i)=W_match3(int(init_draw(h,i)*(nW_match3)+1))
endif
if (sexsampled(h)==1.and.W_edu(h)==4) then
	match_draw(h,i)=W_match4(int(init_draw(h,i)*(nW_match4)+1))
endif 
if (sexsampled(h)==2.and.H_edu(h)==1) then
	match_draw(h,i)=H_match1(int(init_draw(h,i)*(nH_match1)+1))
endif 
if (sexsampled(h)==2.and.H_edu(h)==2) then
	match_draw(h,i)=H_match2(int(init_draw(h,i)*(nH_match2)+1))
endif 
if (sexsampled(h)==2.and.H_edu(h)==3) then
	match_draw(h,i)=H_match3(int(init_draw(h,i)*(nH_match3)+1))
endif 
if (sexsampled(h)==2.and.H_edu(h)==4) then
	match_draw(h,i)=H_match4(int(init_draw(h,i)*(nH_match4)+1))
endif 

enddo
enddo

endsubroutine get_init
subroutine sol_output1
use params
use filenames
implicit none

open(120,file=pathoutput//adjustl(trim(ctag))//emaxfile,position='rewind')
write(120,*) Emax
close(120)
open(130,file=pathoutput//adjustl(trim(ctag))//afile,position='rewind')
write(130,*) statemat_a
close(130)
open(140,file=pathoutput//adjustl(trim(ctag))//Bhfile,position='rewind')
write(140,*) statemat_Bh
close(140)
open(145,file=pathoutput//adjustl(trim(ctag))//Bwfile,position='rewind')
write(145,*) statemat_Bw
close(145)


endsubroutine sol_output1



subroutine sol_output2
use params
use filenames
implicit none

open(150,file=pathoutput//adjustl(trim(ctag))//thetafile,position='rewind')
write(150,*) theta_out
close(150)
open(160,file=pathoutput//adjustl(trim(ctag))//rsqfile,position='rewind')
write(160,*) rsq_out
close(160)
!open(161,file=adj_rsqfile,position='rewind')
!write(161,*) adj_rsq_out
!close(161)
!open(170,file=d1file,position='rewind')
!write(170,*) d_out(1,:,:,:)
!close(170)

endsubroutine sol_output2


subroutine sol_output3
use params
use filenames
implicit none

!open(180,file=pathoutput//adjustl(trim((ctag))d2file,position='rewind')
!write(180,*) d_out(2,:,:,:)
!close(180)
!open(190,file=pathoutput//adjustl(trim((ctag))d3file,position='rewind')
!write(190,*) d_out(3,:,:,:)
!close(190)


endsubroutine sol_output3



subroutine sol_output4
use params
use filenames
implicit none

open(210,file=pathoutput//adjustl(trim(ctag))//d4file,position='rewind')
!write(210,*) d_out(4,:,:,:)
close(210)
open(220,file=pathoutput//adjustl(trim(ctag))//d5file,position='rewind')
!write(220,*) d_out(5,:,:,:)
close(220)
open(230,file=pathoutput//adjustl(trim(ctag))//d6file,position='rewind')
!write(230,*) d_out(6,:,:,:)
close(230)


endsubroutine sol_output4



subroutine sol_output5
use params
use filenames
implicit none

open(240,file=pathoutput//adjustl(trim(ctag))//d7file,position='rewind')
!write(240,*) d_out(7,:,:,:)
close(240)
open(250,file=pathoutput//adjustl(trim(ctag))//d8file,position='rewind')
!write(250,*) d_out(8,:,:,:)
close(250)
open(260,file=pathoutput//adjustl(trim(ctag))//d9file,position='rewind')
!write(260,*) d_out(9,:,:,:)
close(260)

endsubroutine sol_output5



subroutine sol_output6
use params
use filenames
implicit none

!open(270,file=pathoutput//adjustl(trim(ctag))//savingsfile,position='rewind')
!write(270,*) avsav_out
!close(270)
!open(280,file=pathoutput//adjustl(trim(ctag))//incomefile,position='rewind')
!write(280,*) avy_out
!close(280)
open(290,file=pathoutput//adjustl(trim(ctag))//regressorsfile,position='rewind')
write(290,*) reg
close(290)

endsubroutine sol_output6



subroutine sol_output7
use params
use filenames
implicit none

open(340,file=pathoutput//adjustl(trim(ctag))//std_errorsfile,position='rewind')
write(340,*) std_errors_out
close(340)

endsubroutine sol_output7

subroutine sim_output

use params
use filenames
implicit none

integer:: yr,i

!open(700,file=pathoutput//adjustl(trim(ctag))//simulatedMPfile,position='rewind')
!open(600,file=pathoutput//adjustl(trim(ctag))//valuefunctionfile,position='rewind')
!open(500,file=pathoutput//adjustl(trim(ctag))//onehouseholdfile,position='rewind')
open(5000,file=pathoutput//adjustl(trim(ctag))//simulationfile,position='rewind')	
!open(11000,file=MPbeneffile,position='rewind')

HOUSEHOLDLOOP:	do h=1,nhh1

!! Save the value function in 2008 to perform welfare analysis
				!write(600,'(I7,I10,f20.10,I5)') h,folio(h),simv(h,1,first_year(h)-1979),simage(h,1,first_year(h)-1979)
CLONESLOOP:		do i=1,nclones
				!write(700,'(I7,I5,I10,6I4,6f15.2)') h,i,folio(h),simH_elig(h,1), simW_elig(h,1),simH_nonel(h,1),simW_nonel(h,1),simH_overMP(h,1),simW_overMP(h,1),simH_MPbenef(h,1),simW_MPbenef(h,1),simH_WPbenef(h,1),simW_WPbenef(h,1),simH_APS(h,1),simW_APS(h,1)
				YEARLOOP:	do yr=first_year(h),last_year(h),1
								write(5000,'(I5,I5,I10, f20.10,I5,I5,I5,4f15.5,I4,4f15.5,8I3,I6,I3,6f15.5,f15.5)') h,i,folio(h), simv(h,i,yr-1979), simcohort(h,i,yr-1979),sexsampled(h), simage(h,i,yr-1979),&
								&sima(h,i,yr-1979),simBh(h,i,yr-1979),simBw(h,i,yr-1979),simsav(h,i,yr-1979),&
								&simd(h,i,yr-1979),simw_h(h,i,yr-1979),simw_w(h,i,yr-1979),simy(h,i,yr-1979),simc(h,i,yr-1979), &
								&simedu_H(h,i,yr-1979), simedu_W(h,i,yr-1979), simXP_Fh(h,i,yr-1979), simXP_Ih(h,i,yr-1979),&
								&siminact_H(h,i,yr-1979),simXP_Fw(h,i,yr-1979),&
								&simXP_Iw(h,i,yr-1979),siminact_W(h,i,yr-1979), yr, simtype(h,i,yr-1979),&
								&simH_APS(h,i,yr-1979),simW_APS(h,i,yr-1979),simH_MPbenef(h,i,yr-1979),simW_MPbenef(h,i,yr-1979),simH_WPbenef(h,i,yr-1979),simW_WPbenef(h,i,yr-1979),&
								&simtax(h,i,yr-1979)
!								if (folio(h)==folioex) then
!									write(500,'(I5,I5,I10,I5,4f15.2,I4,4f15.2,6I3)') h,i,folio(h),simage(h,i,yr-1979),sima(h,i,yr-1979),simBh(h,i,yr-1979),simBw(h,i,yr-1979),&
!									&simsav(h,i,yr-1979),simd(h,i,yr-1979),simw_h(h,i,yr-1979),simw_w(h,i,yr-1979),simy(h,i,yr-1979),simc(h,i,yr-1979), simedu_H(h,i,yr-1979), &
!									&simedu_W(h,i,yr-1979), simXP_Fh(h,i,yr-1979), simXP_Ih(h,i,yr-1979),simXP_Fw(h,i,yr-1979),simXP_Iw(h,i,yr-1979)		
!								endif
				enddo YEARLOOP
!				write(11000,'(I7,I10,6I4,2f15.2)') h,folio(h),simH_elig(h),simW_elig(h),simH_nonel(h),simW_nonel(h),simH_overMP(h),simW_overMP(h),simH_MPbenef(h), simW_MPbenef(h)
enddo CLONESLOOP
enddo HOUSEHOLDLOOP

close(5000)
!close(500)
!close(11000)
endsubroutine sim_output

subroutine get_data

use params
use filenames
implicit none

open (10,file=foliofile)
open (30,file=H_edufile)
open (40,file=W_edufile)
open (50,file=firstyearfile)
open (60,file=lastyearfile)
!open (70,file=nfoliosfile)
!open (80,file=wealthagefile)
open (90,file=sexsampledfile)
open (100, file=cohortfile)
open (110, file=mdatafile)

read (fmt=*,unit=10) folio
read (fmt=*,unit=30) H_edu
read (fmt=*,unit=40) W_edu
read (fmt=*,unit=50) first_year
read (fmt=*,unit=60) last_year
!read (fmt=*,unit=70) nhh
!read (fmt=*, unit=80) nwealth
read (fmt=*,unit=90) sexsampled
read (fmt=*,unit=100) birthcohort
!read (fmt=*,unit=110) mdata

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
endsubroutine get_data
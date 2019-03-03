subroutine Counterfactuals

use params
use filenames
implicit none



!! Baseline
if (counterfactual==0) then
tao			=	0.1
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976

!! 2008 Reform
elseif (counterfactual==1) then
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	30
realizedret(28:100)=0.05976

!! Topup
elseif (counterfactual==2) then
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	9
realizedret(28:100)=0.05976

!! Universal
elseif (counterfactual==3) then
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	99999999
realizedret(28:100)=0.05976

!! High Taper
elseif (counterfactual==4) then
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	15
realizedret(28:100)=0.05976

!! mandatory contribution 1% 
elseif (counterfactual==1.0) then
tao			=	0.01
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976

!! No mandatory contribution program and no MP(change to tao ==0.00 and toggle off faulty regressors)
elseif (counterfactual==1.99) then
tao			=	0.01
MPyrs		=	9999
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976

elseif (counterfactual==3) then
tao			=	0.03
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976


elseif (counterfactual==3.99) then
tao			=	0.03
MPyrs		=	9999
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976



elseif (counterfactual==5.0) then
tao			=	0.05
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976

elseif (counterfactual==5.99) then
tao			=	0.05
MPyrs		=	9999
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976

!! Base case
elseif (counterfactual==10) then
tao			=	0.1
MPyrs       =	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976

!! Base case, low returns
elseif (counterfactual==10.5) then
tao			=	0.1
MPyrs       =	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.02938

!! High returns
elseif (counterfactual==10.6) then
tao			=	0.10
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.09938

!! Different contribution rates
elseif (counterfactual==15) then
tao			=	0.15
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976

elseif (counterfactual==17.5) then
tao			=	0.175
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976



elseif (counterfactual==20) then
tao			=	0.2
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976


elseif (counterfactual==25) then
tao			=	0.25
MPyrs		=	20
MP			=	12.0
WP			=	6.0
p60			=	-9999
PBS			=	0.0
PMAS		=	0.0
realizedret(28:100)=0.05976



!! 2008 Reform
elseif (counterfactual==2008.0) then
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	30
realizedret(28:100)=0.05976


!! 2008 Reform
elseif (counterfactual==2008.5) then
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	30
realizedret(28:100)=0.02938


!! High returns
elseif (counterfactual==2008.6) then
tao			=	0.10
MPyrs		=	9999
MP			=	0.0
WP			=	0.0
p60			=	30
PBS			=	9
PMAS		=	30
realizedret(28:100)=0.09938

!! Base + Covered offer proba == 1
elseif (counterfactual==1000) then
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


endif

endsubroutine
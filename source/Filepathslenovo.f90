module filenames
implicit none

character(*), parameter:: path				=	'C:\Users\clement\Dropbox\JLSrevise3\JLSrevise3\Baseline\'
character(*), parameter:: pathdatavsmodel	=	path//'Datavsmodel\'
character(*), parameter:: pathoutput		=	path//'Output\'
												
character(*), parameter:: pathcounterf		=	'C:\Users\Clement\Documents\Projects\jointlaborstatus\counterfactuals\'

!character(*), parameter:: path             =   '/home/joubert/JLS/'
!character(*), parameter:: pathdatavsmodel	=	'/home/joubert/JLS/Datavsmodel/'
!character(*), parameter:: pathoutput		=	'/home/joubert/JLS/output/'

!Inputs:
character(*), parameter:: mdatafile			=	pathdatavsmodel//'mdata.txt'
character(*), parameter:: foliofile			=	pathdatavsmodel//'folio.txt'
character(*), parameter:: H_edufile			=	pathdatavsmodel//'H_edugrp.txt'
character(*), parameter:: W_edufile			=	pathdatavsmodel//'W_edugrp.txt'
character(*), parameter:: firstyearfile		=	pathdatavsmodel//'firstyear.txt'
character(*), parameter:: lastyearfile		=	pathdatavsmodel//'lastyear.txt'
character(*), parameter:: wealthfile		=	pathdatavsmodel//'wealth.txt'
character(*), parameter:: H_balancefile		=	pathdatavsmodel//'H_balance.txt'
character(*), parameter:: W_balancefile		=	pathdatavsmodel//'W_balance.txt'
character(*), parameter:: H_Xformalfile		=	pathdatavsmodel//'H_Xformal.txt'
character(*), parameter:: H_Xinformalfile	=	pathdatavsmodel//'H_Xinformal.txt'
character(*), parameter:: H_yrsinactivefile	=	pathdatavsmodel//'H_yrsinactive.txt'
character(*), parameter:: W_Xformalfile		=	pathdatavsmodel//'W_Xformal.txt'
character(*), parameter:: W_Xinformalfile	=	pathdatavsmodel//'W_Xinformal.txt'
character(*), parameter:: W_yrsinactivefile	=	pathdatavsmodel//'W_yrsinactive.txt'
character(*), parameter:: imputedbal04file	=	pathdatavsmodel//'imputedbal04.txt'
character(*), parameter:: nfoliosfile		=	pathdatavsmodel//'nfolios.txt'
character(*), parameter:: sexsampledfile	=	pathdatavsmodel//'sexsampled.txt'
character(*), parameter:: cohortfile		=	pathdatavsmodel//'cohort.txt'
character(*), parameter:: H_agefile			=	pathdatavsmodel//'H_age.txt'
character(*), parameter:: wealthpoolfile	=	pathdatavsmodel//'wealthpool.txt'
character(*), parameter:: W_match1file		=	pathdatavsmodel//'W_match1.txt'
character(*), parameter:: W_match2file		=	pathdatavsmodel//'W_match2.txt'
character(*), parameter:: W_match3file		=	pathdatavsmodel//'W_match3.txt'
character(*), parameter:: W_match4file		=	pathdatavsmodel//'W_match4.txt'
character(*), parameter:: H_match1file		=	pathdatavsmodel//'H_match1.txt'
character(*), parameter:: H_match2file		=	pathdatavsmodel//'H_match2.txt'
character(*), parameter:: H_match3file		=	pathdatavsmodel//'H_match3.txt'
character(*), parameter:: H_match4file		=	pathdatavsmodel//'H_match4.txt'
character(*), parameter:: MPbeneffile		=	pathoutput//'MPbenef.txt'
character(*), parameter:: emaxfile			=	pathoutput//'emax.txt'
character(*), parameter:: afile				=	pathoutput//'a.txt'
character(*), parameter:: Bhfile			=	pathoutput//'Bh.txt'
character(*), parameter:: Bwfile			=	pathoutput//'Bw.txt'
character(*), parameter:: thetafile			=	pathoutput//'theta.txt'
character(*), parameter:: rsqfile			=	pathoutput//'rsq.txt'
character(*), parameter:: adj_rsqfile		=	pathoutput//'adj_rsq.txt'
character(*), parameter:: d1file			=	pathoutput//'d1.txt'
character(*), parameter:: d2file			=	pathoutput//'d2.txt'
character(*), parameter:: d3file			=	pathoutput//'d3.txt'
character(*), parameter:: d4file			=	pathoutput//'d4.txt'
character(*), parameter:: d5file			=	pathoutput//'d5.txt'
character(*), parameter:: d6file			=	pathoutput//'d6.txt'
character(*), parameter:: d7file			=	pathoutput//'d7.txt'
character(*), parameter:: d8file			=	pathoutput//'d8.txt'
character(*), parameter:: d9file			=	pathoutput//'d9.txt'
character(*), parameter:: savingsfile		=	pathoutput//'savings.txt'
character(*), parameter:: incomefile		=	pathoutput//'income.txt'
character(*), parameter:: regressorsfile	=	pathoutput//'regressors.txt'
character(*), parameter:: checkapfile		=	pathoutput//'checkap.txt'
character(*), parameter:: checksavfile		=	pathoutput//'checksav.txt'
character(*), parameter:: checkcfile		=	pathoutput//'checkc.txt'
character(*), parameter:: checkEmaxfile		=	pathoutput//'checkEmax.txt'
character(*), parameter:: std_errorsfile	=	pathoutput//'std_errors.txt'
character(*), parameter:: valuefunctionfile	=	pathoutput//'simvaluefunctions.txt'
character(*), parameter:: simulatedMPfile	=	pathoutput//'simulatedMP.txt'
character(*), parameter:: onehouseholdfile	=	pathoutput//'onehousehold.txt'
character(*), parameter:: simulationfile	=	pathoutput//'simulation.txt'
character(*), parameter:: paramsfile		=	pathoutput//'params.txt'
character(*), parameter:: Xmatfile			=	pathoutput//'Xmat.txt'
character(*), parameter:: Erroranalysis		=	pathoutput//'Erroranalysis.txt'
character(*), parameter:: Erroranalysisold	=	pathoutput//'Erroranalysisold.txt'

	

endmodule


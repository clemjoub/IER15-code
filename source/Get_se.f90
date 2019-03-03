! This files contains the subroutines related to the obtention of parameter std errors


subroutine Bump

! This routine perturbs the value of each parameter in order to obtain the numerical derivative of the moments wrt the parameters
use params
use filenames


goto (1, 2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	19,	20,	21,	22,	23,	24,	25,	26,	27,	28,	29,	30,	31,	32,	33	,34,	35,	36,	37,	38,	39,	40,	41,	42,	43,	44,	45,	46,	47,	48,	49,	50,	51,	52,	53,	54,	55,	56,	57,	58,	59,	60,	61,	62	,63	,64	,65	,66	,67	,68	,69	,70	,71	,72	,73	,74	,75	,76	,77	,78	,79	,80	,81,	82,	83,	84,	85,	86,	87,	88,	89,	90,	91	,92	,93,	94,	95,	96,	97,	98,	99,	119,	101,	102,	103,	104,	105,	106,	107,	108,	109,	110,	111,	112,	113,	114,	115,	116,	117,	118) gradient
 

1	 lambdaedu_h(2)        	=	 lambdaedu_h(2)        		*	(1+hdiff)
goto 100
2	 lambdaedu_w(2)        	=	 lambdaedu_w(2)         	*	(1+hdiff)
goto 100
3	 lambdacohort(2)     	=	 lambdacohort(2)     		*	(1+hdiff)
goto 100
4	 alphatyp(2)            =	 alphatyp(2)            	*	(1+hdiff)
goto 100
5	 lambdaedu_h(3)         =	 lambdaedu_h(3)         	*	(1+hdiff)
goto 100
6	 lambdaedu_w(3)         =	 lambdaedu_w(3)         	*	(1+hdiff)
goto 100
7	 lambdacohort(3)     	=	 lambdacohort(3)     		*	(1+hdiff)
goto 100
8	 alphatyp(3)            =	 alphatyp(3)            	*	(1+hdiff)
goto 100
9	 rho(1)                 =	 rho(1)                 	*	(1+hdiff)
goto 100
10	 raw_delta(1,3)       	=	 raw_delta(1,3)       		*	(1+hdiff)
goto 100
11	 raw_delta(1,7)       	=	 raw_delta(1,7)       		*	(1+hdiff)
goto 100
12	 rho(2)                 =	 rho(2)                 	*	(1+hdiff)
goto 100
13	 raw_delta(2,3)       	=	 raw_delta(2,3)       		*	(1+hdiff)
goto 100
14	 raw_delta(2,7)       	=	 raw_delta(2,7)       		*	(1+hdiff)
goto 100
15	 rho(3)                 =	 rho(3)                 	*	(1+hdiff)
goto 100
16	 raw_delta(3,3)       	=	 raw_delta(3,3)       		*	(1+hdiff)
goto 100
17	 raw_delta(3,7)       	=	 raw_delta(3,7)       		*	(1+hdiff)
goto 100
18	 sig_pref_h          	=	 sig_pref_h          		*	(1+hdiff)
goto 100
19	 sig_pref_w          	=	 sig_pref_w          		*	(1+hdiff)
goto 100
20	 raw_h_switch_act    	=	 raw_h_switch_act    		*	(1+hdiff)
goto 100
21	 raw_w_switch_act    	=	 raw_w_switch_act    		*	(1+hdiff)
goto 100
22	 raw_h_switch_sect   	=	 raw_h_switch_sect   		*	(1+hdiff)
goto 100
23	 raw_w_switch_sect   	=	 raw_w_switch_sect   		*	(1+hdiff)
goto 100
24	 alpha(1,1)           	=	 alpha(1,1)           		*	(1+hdiff)
goto 100
25	 alpha(1,2)           	=	 alpha(1,2)           		*	(1+hdiff)
goto 100
26	 alpha(1,3)           	=	 alpha(1,3)           		*	(1+hdiff)
goto 100
27	 alpha(2,1)           	=	 alpha(2,1)           		*	(1+hdiff)
goto 100
28	 alpha(3,1)           	=	 alpha(3,1)           		*	(1+hdiff)
goto 100
29	 SR_Fh               	=	 SR_Fh               		*	(1+hdiff)
goto 100
30	 SR_Ih               	=	 SR_Ih               		*	(1+hdiff)
goto 100
31	 SR_Fw               	=	 SR_Fw               		*	(1+hdiff)
goto 100
32	 SR_Iw               	=	 SR_Iw               		*	(1+hdiff)
goto 100
33	 theta_cohort_F      	=	 theta_cohort_F      		*	(1+hdiff)
goto 100
34	 theta_cohort_I      	=	 theta_cohort_I      		*	(1+hdiff)
goto 100
35	 theta_FhFh          	=	 theta_FhFh          		*	(1+hdiff)
goto 100
36	 theta_IhIh          	=	 theta_IhIh          		*	(1+hdiff)
goto 100
37	 theta_FwFw          	=	 theta_FwFw          		*	(1+hdiff)
goto 100
38	 theta_IwIw          	=	 theta_IwIw          		*	(1+hdiff)
goto 100
39	 Xptransfer          	=    1 !Xptransfer          		*	(1+hdiff)
goto 100
40	 theta_eduFh         	=	 theta_eduFh         		*	(1+hdiff)
goto 100
41	 theta_eduIh         	=	 theta_eduIh         		*	(1+hdiff)
goto 100
42	 theta_eduFw         	=	 theta_eduFw         		*	(1+hdiff)
goto 100
43	 theta_eduIw         	=	 theta_eduIw         		*	(1+hdiff)
goto 100
44	 theta_grad_Fh       	=	 theta_grad_Fh       		*	(1+hdiff)
goto 100
45	 theta_grad_Fw       	=	 theta_grad_Fw       		*	(1+hdiff)
goto 100
46	 theta_eduXPh        	=	 theta_eduXPh        		*	(1+hdiff)
goto 100
47	 theta_eduXPw        	=	 theta_eduXPw        		*	(1+hdiff)
goto 100
48	 sig_Fh              	=	 sig_Fh              		*	(1+hdiff)
goto 100
49	 sig_Ih              	=	 sig_Ih              		*	(1+hdiff)
goto 100
50	 sig_Fw              	=	 sig_Fw              		*	(1+hdiff)
goto 100
51	 sig_Iw              	=	 sig_Iw              		*	(1+hdiff)
goto 100
52	 gamma(1)               	=	 gamma(1)               		*	(1+hdiff)
goto 100
53	 gamma(2)               	=	 gamma(2)               		*	(1+hdiff)
goto 100
54	 gammacov(1)         	=	 gammacov(1)         		*	(1+hdiff)
goto 100
55	 gammacov(2)         	=	 gammacov(2)         		*	(1+hdiff)
goto 100
56	 gammaXP(1)             	=	 gammaXP(1)             		*	(1+hdiff)
goto 100
57	 gammaXP(2)             	=	 gammaXP(2)             		*	(1+hdiff)
goto 100
58	 gammaedu(1)         	=	 gammaedu(1)         		*	(1+hdiff)
goto 100
59	 gammaedu(2)         	=	 gammaedu(2)         		*	(1+hdiff)
goto 100
60	 lambdaedu_h(2)         	=	 lambdaedu_h(2)         		*	(1-hdiff)
goto 100
61	 lambdaedu_w(2)         	=	 lambdaedu_w(2)         		*	(1-hdiff)
goto 100
62	 lambdacohort(2)     	=	 lambdacohort(2)     		*	(1-hdiff)
goto 100
63	 alphatyp(2)            	=	 alphatyp(2)            		*	(1-hdiff)
goto 100
64	 lambdaedu_h(3)         	=	 lambdaedu_h(3)         		*	(1-hdiff)
goto 100
65	 lambdaedu_w(3)         	=	 lambdaedu_w(3)         		*	(1-hdiff)
goto 100
66	 lambdacohort(3)     	=	 lambdacohort(3)     		*	(1-hdiff)
goto 100
67	 alphatyp(3)            	=	 alphatyp(3)            		*	(1-hdiff)
goto 100
68	 rho(1)                 	=	 rho(1)                 		*	(1-hdiff)
goto 100
69	 raw_delta(1,3)       	=	 raw_delta(1,3)       		*	(1-hdiff)
goto 100
70	 raw_delta(1,7)       	=	 raw_delta(1,7)       		*	(1-hdiff)
goto 100
71	 rho(2)                 	=	 rho(2)                 		*	(1-hdiff)
goto 100
72	 raw_delta(2,3)       	=	 raw_delta(2,3)       		*	(1-hdiff)
goto 100
73	 raw_delta(2,7)       	=	 raw_delta(2,7)       		*	(1-hdiff)
goto 100
74	 rho(3)                 	=	 rho(3)                 		*	(1-hdiff)
goto 100
75	 raw_delta(3,3)       	=	 raw_delta(3,3)       		*	(1-hdiff)
goto 100
76	 raw_delta(3,7)       	=	 raw_delta(3,7)       		*	(1-hdiff)
goto 100
77	 sig_pref_h          	=	 sig_pref_h          		*	(1-hdiff)
goto 100
78	 sig_pref_w          	=	 sig_pref_w          		*	(1-hdiff)
goto 100
79	 raw_h_switch_act    	=	 raw_h_switch_act    		*	(1-hdiff)
goto 100
80	 raw_w_switch_act    	=	 raw_w_switch_act    		*	(1-hdiff)
goto 100
81	 raw_h_switch_sect   	=	 raw_h_switch_sect   		*	(1-hdiff)
goto 100
82	 raw_w_switch_sect   	=	 raw_w_switch_sect   		*	(1-hdiff)
goto 100
83	 alpha(1,1)           	=	 alpha(1,1)           		*	(1-hdiff)
goto 100
84	 alpha(1,2)           	=	 alpha(1,2)           		*	(1-hdiff)
goto 100
85	 alpha(1,3)           	=	 alpha(1,3)           		*	(1-hdiff)
goto 100
86	 alpha(2,1)           	=	 alpha(2,1)           		*	(1-hdiff)
goto 100
87	 alpha(3,1)           	=	 alpha(3,1)           		*	(1-hdiff)
goto 100
88	 SR_Fh               	=	 SR_Fh               		*	(1-hdiff)
goto 100
89	 SR_Ih               	=	 SR_Ih               		*	(1-hdiff)
goto 100
90	 SR_Fw               	=	 SR_Fw               		*	(1-hdiff)
goto 100
91	 SR_Iw               	=	 SR_Iw               		*	(1-hdiff)
goto 100
92	 theta_cohort_F      	=	 theta_cohort_F      		*	(1-hdiff)
goto 100
93	 theta_cohort_I      	=	 theta_cohort_I      		*	(1-hdiff)
goto 100
94	 theta_FhFh          	=	 theta_FhFh          		*	(1-hdiff)
goto 100
95	 theta_IhIh          	=	 theta_IhIh          		*	(1-hdiff)
goto 100
96	 theta_FwFw          	=	 theta_FwFw          		*	(1-hdiff)
goto 100
97	 theta_IwIw          	=	 theta_IwIw          		*	(1-hdiff)
goto 100
98	 Xptransfer          	=	 Xptransfer          		*	(1-hdiff)
goto 100
99	 theta_eduFh         	=	 theta_eduFh         		*	(1-hdiff)
goto 100
119	 theta_eduIh         	=	 theta_eduIh         		*	(1-hdiff)
goto 100
101	 theta_eduFw         	=	 theta_eduFw         		*	(1-hdiff)
goto 100
102	 theta_eduIw         	=	 theta_eduIw         		*	(1-hdiff)
goto 100
103	 theta_grad_Fh       	=	 theta_grad_Fh       		*	(1-hdiff)
goto 100
104	 theta_grad_Fw       	=	 theta_grad_Fw       		*	(1-hdiff)
goto 100
105	 theta_eduXPh        	=	 theta_eduXPh        		*	(1-hdiff)
goto 100
106	 theta_eduXPw        	=	 theta_eduXPw        		*	(1-hdiff)
goto 100
107	 sig_Fh              	=	 sig_Fh              		*	(1-hdiff)
goto 100
108	 sig_Ih              	=	 sig_Ih              		*	(1-hdiff)
goto 100
109	 sig_Fw              	=	 sig_Fw              		*	(1-hdiff)
goto 100
110	 sig_Iw              	=	 sig_Iw              		*	(1-hdiff)
goto 100
111	 gamma(1)               	=	 gamma(1)               		*	(1-hdiff)
goto 100
112	 gamma(2)               	=	 gamma(2)               		*	(1-hdiff)
goto 100
113	 gammacov(1)         	=	 gammacov(1)         		*	(1-hdiff)
goto 100
114	 gammacov(2)         	=	 gammacov(2)         		*	(1-hdiff)
goto 100
115	 gammaXP(1)             	=	 gammaXP(1)             		*	(1-hdiff)
goto 100
116	 gammaXP(2)             	=	 gammaXP(2)             		*	(1-hdiff)
goto 100
117	 gammaedu(1)         	=	 gammaedu(1)         		*	(1-hdiff)
goto 100
118	 gammaedu(2)         	=	 gammaedu(2)         		*	(1-hdiff)
goto 100
120  sigma(1)=sigma(1)*(1-hdiff)
100 continue

endsubroutine



subroutine Save_bumped_moments

use params
use filenames

goto 8989

goto (1, 2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	19,	20,	21,	22,	23,	24,	25,	26,	27,	28,	29,	30,	31,	32,	33	,34,	35,	36,	37,	38,	39,	40,	41,	42,	43,	44,	45,	46,	47,	48,	49,	50,	51,	52,	53,	54,	55,	56,	57,	58,	59,	60,	61,	62	,63	,64	,65	,66	,67	,68	,69	,70	,71	,72	,73	,74	,75	,76	,77	,78	,79	,80	,81,	82,	83,	84,	85,	86,	87,	88,	89,	90,	91	,92	,93,	94,	95,	96,	97,	98,	99,	119,	101,	102,	103,	104,	105,	106,	107,	108,	109,	110,	111,	112,	113,	114,	115,	116,	117,	118) gradient


1	open(unit=19001, file=pathoutput//'lambdaedu_h(2)_plus.txt',position='rewind')
goto 100
2	open(unit=19001, file=pathoutput//'lambdaedu_w(2)_plus.txt',position='rewind')
goto 100
3	open(unit=19001, file=pathoutput//'lambdacohort(2)_plus.txt',position='rewind')
goto 100
4	open(unit=19001, file=pathoutput//'alphatyp(2)_plus.txt',position='rewind')
goto 100
5	open(unit=19001, file=pathoutput//'lambdaedu_h(3)_plus.txt',position='rewind')
goto 100
6	open(unit=19001, file=pathoutput//'lambdaedu_w(3)_plus.txt',position='rewind')
goto 100
7	open(unit=19001, file=pathoutput//'lambdacohort(3)_plus.txt',position='rewind')
goto 100
8	open(unit=19001, file=pathoutput//'alphatyp(3)_plus.txt',position='rewind')
goto 100
9	open(unit=19001, file=pathoutput//'rho(1)_plus.txt',position='rewind')
goto 100
10	open(unit=19001, file=pathoutput//'raw_delta(13)_plus.txt',position='rewind')
goto 100
11	open(unit=19001, file=pathoutput//'raw_delta(17)_plus.txt',position='rewind')
goto 100
12	open(unit=19001, file=pathoutput//'rho(2)_plus.txt',position='rewind')
goto 100
13	open(unit=19001, file=pathoutput//'raw_delta(23)_plus.txt',position='rewind')
goto 100
14	open(unit=19001, file=pathoutput//'raw_delta(27)_plus.txt',position='rewind')
goto 100
15	open(unit=19001, file=pathoutput//'rho(3)_plus.txt',position='rewind')
goto 100
16	open(unit=19001, file=pathoutput//'raw_delta(33)_plus.txt',position='rewind')
goto 100
17	open(unit=19001, file=pathoutput//'raw_delta(37)_plus.txt',position='rewind')
goto 100
18	open(unit=19001, file=pathoutput//'sig_pref_h_plus.txt',position='rewind')
goto 100
19	open(unit=19001, file=pathoutput//'sig_pref_w_plus.txt',position='rewind')
goto 100
20	open(unit=19001, file=pathoutput//'raw_h_switch_act_plus.txt',position='rewind')
goto 100
21	open(unit=19001, file=pathoutput//'raw_w_switch_act_plus.txt',position='rewind')
goto 100
22	open(unit=19001, file=pathoutput//'raw_h_switch_sect_plus.txt',position='rewind')
goto 100
23	open(unit=19001, file=pathoutput//'raw_w_switch_sect_plus.txt',position='rewind')
goto 100
24	open(unit=19001, file=pathoutput//'alpha(11)_plus.txt',position='rewind')
goto 100
25	open(unit=19001, file=pathoutput//'alpha(12)_plus.txt',position='rewind')
goto 100
26	open(unit=19001, file=pathoutput//'alpha(13)_plus.txt',position='rewind')
goto 100
27	open(unit=19001, file=pathoutput//'alpha(21)_plus.txt',position='rewind')
goto 100
28	open(unit=19001, file=pathoutput//'alpha(31)_plus.txt',position='rewind')
goto 100
29	open(unit=19001, file=pathoutput//'SR_Fh_plus.txt',position='rewind')
goto 100
30	open(unit=19001, file=pathoutput//'SR_Ih_plus.txt',position='rewind')
goto 100
31	open(unit=19001, file=pathoutput//'SR_Fw_plus.txt',position='rewind')
goto 100
32	open(unit=19001, file=pathoutput//'SR_Iw_plus.txt',position='rewind')
goto 100
33	open(unit=19001, file=pathoutput//'theta_cohort_F_plus.txt',position='rewind')
goto 100
34	open(unit=19001, file=pathoutput//'theta_cohort_I_plus.txt',position='rewind')
goto 100
35	open(unit=19001, file=pathoutput//'theta_FhFh_plus.txt',position='rewind')
goto 100
36	open(unit=19001, file=pathoutput//'theta_IhIh_plus.txt',position='rewind')
goto 100
37	open(unit=19001, file=pathoutput//'theta_FwFw_plus.txt',position='rewind')
goto 100
38	open(unit=19001, file=pathoutput//'theta_IwIw_plus.txt',position='rewind')
goto 100
39	open(unit=19001, file=pathoutput//'Xptransfer_plus.txt',position='rewind')
goto 100
40	open(unit=19001, file=pathoutput//'theta_eduFh_plus.txt',position='rewind')
goto 100
41	open(unit=19001, file=pathoutput//'theta_eduIh_plus.txt',position='rewind')
goto 100
42	open(unit=19001, file=pathoutput//'theta_eduFw_plus.txt',position='rewind')
goto 100
43	open(unit=19001, file=pathoutput//'theta_eduIw_plus.txt',position='rewind')
goto 100
44	open(unit=19001, file=pathoutput//'theta_grad_Fh_plus.txt',position='rewind')
goto 100
45	open(unit=19001, file=pathoutput//'theta_grad_Fw_plus.txt',position='rewind')
goto 100
46	open(unit=19001, file=pathoutput//'theta_eduXPh_plus.txt',position='rewind')
goto 100
47	open(unit=19001, file=pathoutput//'theta_eduXPw_plus.txt',position='rewind')
goto 100
48	open(unit=19001, file=pathoutput//'sig_Fh_plus.txt',position='rewind')
goto 100
49	open(unit=19001, file=pathoutput//'sig_Ih_plus.txt',position='rewind')
goto 100
50	open(unit=19001, file=pathoutput//'sig_Fw_plus.txt',position='rewind')
goto 100
51	open(unit=19001, file=pathoutput//'sig_Iw_plus.txt',position='rewind')
goto 100
52	open(unit=19001, file=pathoutput//'gamma(1)_plus.txt',position='rewind')
goto 100
53	open(unit=19001, file=pathoutput//'gamma(2)_plus.txt',position='rewind')
goto 100
54	open(unit=19001, file=pathoutput//'gammacov(1)_plus.txt',position='rewind')
goto 100
55	open(unit=19001, file=pathoutput//'gammacov(2)_plus.txt',position='rewind')
goto 100
56	open(unit=19001, file=pathoutput//'gammaXP(1)_plus.txt',position='rewind')
goto 100
57	open(unit=19001, file=pathoutput//'gammaXP(2)_plus.txt',position='rewind')
goto 100
58	open(unit=19001, file=pathoutput//'gammaedu(1)_plus.txt',position='rewind')
goto 100
59	open(unit=19001, file=pathoutput//'gammaedu(2)_plus.txt',position='rewind')
goto 100
60	open(unit=19001, file=pathoutput//'lambdaedu_h(2)_minus.txt',position='rewind')
goto 100
61	open(unit=19001, file=pathoutput//'lambdaedu_w(2)_minus.txt',position='rewind')
goto 100
62	open(unit=19001, file=pathoutput//'lambdacohort(2)_minus.txt',position='rewind')
goto 100
63	open(unit=19001, file=pathoutput//'alphatyp(2)_minus.txt',position='rewind')
goto 100
64	open(unit=19001, file=pathoutput//'lambdaedu_h(3)_minus.txt',position='rewind')
goto 100
65	open(unit=19001, file=pathoutput//'lambdaedu_w(3)_minus.txt',position='rewind')
goto 100
66	open(unit=19001, file=pathoutput//'lambdacohort(3)_minus.txt',position='rewind')
goto 100
67	open(unit=19001, file=pathoutput//'alphatyp(3)_minus.txt',position='rewind')
goto 100
68	open(unit=19001, file=pathoutput//'rho(1)_minus.txt',position='rewind')
goto 100
69	open(unit=19001, file=pathoutput//'raw_delta(13)_minus.txt',position='rewind')
goto 100
70	open(unit=19001, file=pathoutput//'raw_delta(17)_minus.txt',position='rewind')
goto 100
71	open(unit=19001, file=pathoutput//'rho(2)_minus.txt',position='rewind')
goto 100
72	open(unit=19001, file=pathoutput//'raw_delta(23)_minus.txt',position='rewind')
goto 100
73	open(unit=19001, file=pathoutput//'raw_delta(27)_minus.txt',position='rewind')
goto 100
74	open(unit=19001, file=pathoutput//'rho(3)_minus.txt',position='rewind')
goto 100
75	open(unit=19001, file=pathoutput//'raw_delta(33)_minus.txt',position='rewind')
goto 100
76	open(unit=19001, file=pathoutput//'raw_delta(37)_minus.txt',position='rewind')
goto 100
77	open(unit=19001, file=pathoutput//'sig_pref_h_minus.txt',position='rewind')
goto 100
78	open(unit=19001, file=pathoutput//'sig_pref_w_minus.txt',position='rewind')
goto 100
79	open(unit=19001, file=pathoutput//'raw_h_switch_act_minus.txt',position='rewind')
goto 100
80	open(unit=19001, file=pathoutput//'raw_w_switch_act_minus.txt',position='rewind')
goto 100
81	open(unit=19001, file=pathoutput//'raw_h_switch_sect_minus.txt',position='rewind')
goto 100
82	open(unit=19001, file=pathoutput//'raw_w_switch_sect_minus.txt',position='rewind')
goto 100
83	open(unit=19001, file=pathoutput//'alpha(11)_minus.txt',position='rewind')
goto 100
84	open(unit=19001, file=pathoutput//'alpha(12)_minus.txt',position='rewind')
goto 100
85	open(unit=19001, file=pathoutput//'alpha(13)_minus.txt',position='rewind')
goto 100
86	open(unit=19001, file=pathoutput//'alpha(21)_minus.txt',position='rewind')
goto 100
87	open(unit=19001, file=pathoutput//'alpha(31)_minus.txt',position='rewind')
goto 100
88	open(unit=19001, file=pathoutput//'SR_Fh_minus.txt',position='rewind')
goto 100
89	open(unit=19001, file=pathoutput//'SR_Ih_minus.txt',position='rewind')
goto 100
90	open(unit=19001, file=pathoutput//'SR_Fw_minus.txt',position='rewind')
goto 100
91	open(unit=19001, file=pathoutput//'SR_Iw_minus.txt',position='rewind')
goto 100
92	open(unit=19001, file=pathoutput//'theta_cohort_F_minus.txt',position='rewind')
goto 100
93	open(unit=19001, file=pathoutput//'theta_cohort_I_minus.txt',position='rewind')
goto 100
94	open(unit=19001, file=pathoutput//'theta_FhFh_minus.txt',position='rewind')
goto 100
95	open(unit=19001, file=pathoutput//'theta_IhIh_minus.txt',position='rewind')
goto 100
96	open(unit=19001, file=pathoutput//'theta_FwFw_minus.txt',position='rewind')
goto 100
97	open(unit=19001, file=pathoutput//'theta_IwIw_minus.txt',position='rewind')
goto 100
98	open(unit=19001, file=pathoutput//'Xptransfer_minus.txt',position='rewind')
goto 100
99	open(unit=19001, file=pathoutput//'theta_eduFh_minus.txt',position='rewind')
goto 100
119	open(unit=19001, file=pathoutput//'theta_eduIh_minus.txt',position='rewind')
goto 100
101	open(unit=19001, file=pathoutput//'theta_eduFw_minus.txt',position='rewind')
goto 100
102	open(unit=19001, file=pathoutput//'theta_eduIw_minus.txt',position='rewind')
goto 100
103	open(unit=19001, file=pathoutput//'theta_grad_Fh_minus.txt',position='rewind')
goto 100
104	open(unit=19001, file=pathoutput//'theta_grad_Fw_minus.txt',position='rewind')
goto 100
105	open(unit=19001, file=pathoutput//'theta_eduXPh_minus.txt',position='rewind')
goto 100
106	open(unit=19001, file=pathoutput//'theta_eduXPw_minus.txt',position='rewind')
goto 100
107	open(unit=19001, file=pathoutput//'sig_Fh_minus.txt',position='rewind')
goto 100
108	open(unit=19001, file=pathoutput//'sig_Ih_minus.txt',position='rewind')
goto 100
109	open(unit=19001, file=pathoutput//'sig_Fw_minus.txt',position='rewind')
goto 100
110	open(unit=19001, file=pathoutput//'sig_Iw_minus.txt',position='rewind')
goto 100
111	open(unit=19001, file=pathoutput//'gamma(1)_minus.txt',position='rewind')
goto 100
112	open(unit=19001, file=pathoutput//'gamma(2)_minus.txt',position='rewind')
goto 100
113	open(unit=19001, file=pathoutput//'gammacov(1)_minus.txt',position='rewind')
goto 100
114	open(unit=19001, file=pathoutput//'gammacov(2)_minus.txt',position='rewind')
goto 100
115	open(unit=19001, file=pathoutput//'gammaXP(1)_minus.txt',position='rewind')
goto 100
116	open(unit=19001, file=pathoutput//'gammaXP(2)_minus.txt',position='rewind')
goto 100
117	open(unit=19001, file=pathoutput//'gammaedu(1)_minus.txt',position='rewind')
goto 100
118	open(unit=19001, file=pathoutput//'gammaedu(2)_minus.txt',position='rewind')
goto 100

100 continue

write(19001,*) '	 lambdaedu_h(2)         	',	 lambdaedu_h(2)         
write(19001,*) '	 lambdaedu_w(2)         	',	 lambdaedu_w(2)         
write(19001,*) '	 lambdacohort(2)     	',	 lambdacohort(2)     
write(19001,*) '	 alphatyp(2)            	',	 alphatyp(2)            
write(19001,*) '	 lambdaedu_h(3)         	',	 lambdaedu_h(3)         
write(19001,*) '	 lambdaedu_w(3)         	',	 lambdaedu_w(3)         
write(19001,*) '	 lambdacohort(3)     	',	 lambdacohort(3)     
write(19001,*) '	 alphatyp(3)            	',	 alphatyp(3)            
write(19001,*) '	 rho(1)                 	',	 rho(1)                 
write(19001,*) '	 raw_delta(1,3)       	',	 raw_delta(1,3)       
write(19001,*) '	 raw_delta(1,7)       	',	 raw_delta(1,7)       
write(19001,*) '	 rho(2)                 	',	 rho(2)                 
write(19001,*) '	 raw_delta(2,3)       	',	 raw_delta(2,3)       
write(19001,*) '	 raw_delta(2,7)       	',	 raw_delta(2,7)       
write(19001,*) '	 rho(3)                 	',	 rho(3)                 
write(19001,*) '	 raw_delta(3,3)       	',	 raw_delta(3,3)       
write(19001,*) '	 raw_delta(3,7)       	',	 raw_delta(3,7)       
write(19001,*) '	 sig_pref_h          	',	 sig_pref_h          
write(19001,*) '	 sig_pref_w          	',	 sig_pref_w          
write(19001,*) '	 raw_h_switch_act    	',	 raw_h_switch_act    
write(19001,*) '	 raw_w_switch_act    	',	 raw_w_switch_act    
write(19001,*) '	 raw_h_switch_sect   	',	 raw_h_switch_sect   
write(19001,*) '	 raw_w_switch_sect   	',	 raw_w_switch_sect   
write(19001,*) '	 alpha(1,1)           	',	 alpha(1,1)           
write(19001,*) '	 alpha(1,2)           	',	 alpha(1,2)           
write(19001,*) '	 alpha(1,3)           	',	 alpha(1,3)           
write(19001,*) '	 alpha(2,1)           	',	 alpha(2,1)           
write(19001,*) '	 alpha(3,1)           	',	 alpha(3,1)           
write(19001,*) '	 SR_Fh               	',	 SR_Fh               
write(19001,*) '	 SR_Ih               	',	 SR_Ih               
write(19001,*) '	 SR_Fw               	',	 SR_Fw               
write(19001,*) '	 SR_Iw               	',	 SR_Iw               
write(19001,*) '	 theta_cohort_F      	',	 theta_cohort_F      
write(19001,*) '	 theta_cohort_I      	',	 theta_cohort_I      
write(19001,*) '	 theta_FhFh          	',	 theta_FhFh          
write(19001,*) '	 theta_IhIh          	',	 theta_IhIh          
write(19001,*) '	 theta_FwFw          	',	 theta_FwFw          
write(19001,*) '	 theta_IwIw          	',	 theta_IwIw          
write(19001,*) '	 Xptransfer          	',	 Xptransfer          
write(19001,*) '	 theta_eduFh         	',	 theta_eduFh         
write(19001,*) '	 theta_eduIh         	',	 theta_eduIh         
write(19001,*) '	 theta_eduFw         	',	 theta_eduFw         
write(19001,*) '	 theta_eduIw         	',	 theta_eduIw         
write(19001,*) '	 theta_grad_Fh       	',	 theta_grad_Fh       
write(19001,*) '	 theta_grad_Fw       	',	 theta_grad_Fw       
write(19001,*) '	 theta_eduXPh        	',	 theta_eduXPh        
write(19001,*) '	 theta_eduXPw        	',	 theta_eduXPw        
write(19001,*) '	 sig_Fh              	',	 sig_Fh              
write(19001,*) '	 sig_Ih              	',	 sig_Ih              
write(19001,*) '	 sig_Fw              	',	 sig_Fw              
write(19001,*) '	 sig_Iw              	',	 sig_Iw              
write(19001,*) '	 gamma(1)               	',	 gamma(1)               
write(19001,*) '	 gamma(2)               	',	 gamma(2)               
write(19001,*) '	 gammacov(1)         	',	 gammacov(1)         
write(19001,*) '	 gammacov(2)         	',	 gammacov(2)         
write(19001,*) '	 gammaXP(1)             	',	 gammaXP(1)             
write(19001,*) '	 gammaXP(2)             	',	 gammaXP(2)             
write(19001,*) '	 gammaedu(1)         	',	 gammaedu(1)         
write(19001,*) '	 gammaedu(2)         	',	 gammaedu(2)         
      

8989 continue

open (unit=11131,file=pathoutput//'firstdiff.txt')

do i=1,nmoments
do j=1,118
if (i==354.or.i==372.or.i==384.or.i==396) then
read(fmt=*,unit=11131)
firstdiff_mat(i,j)=0
else
read(11131,*) firstdiff_mat(i,j)
endif
enddo
enddo
close(11131)


!do i=1,nmoments
!	k=1
!	if (i==354.or.i==372.or.i==384.or.i==396) then
!		do j=1,int(nparams/3+1)
!			read(fmt=*,unit=11131)
!		enddo
!	firstdiff_mat(i,:)=0
!	goto 386
!	endif
!	do j=1,int(nparams/3)
!		read(fmt=*,unit=11131) firstdiff_mat(i,k),firstdiff_mat(i,k+1),firstdiff_mat(i,k+2)
!		k=k+3
!	enddo
!	read(fmt=*,unit=11131) firstdiff_mat(i,k),firstdiff_mat(i,k+1)
!	386 continue 
!enddo

do i=1,nmoments
write(19001,*) loss(1,i)
firstdiff_mat(i,gradient)=loss(1,i)
enddo
close(19001)

open (unit=11131,file=pathoutput//'firstdiff.txt')
do i=1,nmoments
do j=1,118
write(11131,*) firstdiff_mat(i,j)
enddo
enddo
close(11131)

endsubroutine


subroutine Diff_gradient

use params
use filenames

integer:: p,pp,i

h_mat(1)=2*hdiff*	 lambdaedu_h(2)         
h_mat(2)=2*hdiff*	 lambdaedu_w(2)         
h_mat(3)=2*hdiff*	 lambdacohort(2)     
h_mat(4)=2*hdiff*	 alphatyp(2)            
h_mat(5)=2*hdiff*	 lambdaedu_h(3)         
h_mat(6)=2*hdiff*	 lambdaedu_w(3)         
h_mat(7)=2*hdiff*	 lambdacohort(3)     
h_mat(8)=2*hdiff*	 alphatyp(3)            
h_mat(9)=2*hdiff*	 rho(1)                 
h_mat(10)=2*hdiff*	 raw_delta(1,3)       
h_mat(11)=2*hdiff*	 raw_delta(1,7)       
h_mat(12)=2*hdiff*	 rho(2)                 
h_mat(13)=2*hdiff*	 raw_delta(2,3)       
h_mat(14)=2*hdiff*	 raw_delta(2,7)       
h_mat(15)=2*hdiff*	 rho(3)                 
h_mat(16)=2*hdiff*	 raw_delta(3,3)       
h_mat(17)=2*hdiff*	 raw_delta(3,7)       
h_mat(18)=2*hdiff*	 sig_pref_h          
h_mat(19)=2*hdiff*	 sig_pref_w          
h_mat(20)=2*hdiff*	 raw_h_switch_act    
h_mat(21)=2*hdiff*	 raw_w_switch_act    
h_mat(22)=2*hdiff*	 raw_h_switch_sect   
h_mat(23)=2*hdiff*	 raw_w_switch_sect   
h_mat(24)=2*hdiff*	 alpha(1,1)           
h_mat(25)=2*hdiff*	 alpha(1,2)           
h_mat(26)=2*hdiff*	 alpha(1,3)           
h_mat(27)=2*hdiff*	 alpha(2,1)           
h_mat(28)=2*hdiff*	 alpha(3,1)           
h_mat(29)=2*hdiff*	 SR_Fh               
h_mat(30)=2*hdiff*	 SR_Ih               
h_mat(31)=2*hdiff*	 SR_Fw               
h_mat(32)=2*hdiff*	 SR_Iw               
h_mat(33)=2*hdiff*	 theta_cohort_F      
h_mat(34)=2*hdiff*	 theta_cohort_I      
h_mat(35)=2*hdiff*	 theta_FhFh          
h_mat(36)=2*hdiff*	 theta_IhIh          
h_mat(37)=2*hdiff*	 theta_FwFw          
h_mat(38)=2*hdiff*	 theta_IwIw          
h_mat(39)=2*hdiff*	 Xptransfer          
h_mat(40)=2*hdiff*	 theta_eduFh         
h_mat(41)=2*hdiff*	 theta_eduIh         
h_mat(42)=2*hdiff*	 theta_eduFw         
h_mat(43)=2*hdiff*	 theta_eduIw         
h_mat(44)=2*hdiff*	 theta_grad_Fh       
h_mat(45)=2*hdiff*	 theta_grad_Fw       
h_mat(46)=2*hdiff*	 theta_eduXPh        
h_mat(47)=2*hdiff*	 theta_eduXPw        
h_mat(48)=2*hdiff*	 sig_Fh              
h_mat(49)=2*hdiff*	 sig_Ih              
h_mat(50)=2*hdiff*	 sig_Fw              
h_mat(51)=2*hdiff*	 sig_Iw              
h_mat(52)=2*hdiff*	 gamma(1)               
h_mat(53)=2*hdiff*	 gamma(2)               
h_mat(54)=2*hdiff*	 gammacov(1)         
h_mat(55)=2*hdiff*	 gammacov(2)         
h_mat(56)=2*hdiff*	 gammaXP(1)             
h_mat(57)=2*hdiff*	 gammaXP(2)             
h_mat(58)=2*hdiff*	 gammaedu(1)         
h_mat(59)=2*hdiff*	 gammaedu(2)         

open (unit=11131,file=pathoutput//'firstdiff.txt')

do i=1,953
do j=1,118
	if (i==354.or.i==372.or.i==384.or.i==396) then
		read(fmt=*,unit=11131)
		firstdiff_mat(i,j)=0
	else
		read(11131,*) firstdiff_mat(i,j)
	endif
enddo
enddo
close(11131)

do j=1,59
do i=1,953
gradient_mat(i,j)=(firstdiff_mat(i,j)-firstdiff_mat(i,j+59))*1/h_mat(j)
enddo
enddo

!do p=1,59
!do pp=1,59
!do i=1,953
!temp=gradient_mat(i,p)*gradient_mat(i,pp)*weights(i)
!if (isnan(temp)) temp=0
!sigma1_inv(p,pp)=sigma1_inv(p,pp)+temp
!enddo
!enddo
!enddo

!open(unit=12900, file=pathoutput//'sigma1_inv.txt',position='rewind')
!write(12900,*) 'sigma1_inv'
!do i=1,59
!write(12900,*) sigma1_inv(i,:)
!enddo
!close(12900)

open(unit=12901, file=pathoutput//'gradient.txt',position='rewind')
write(12901,*) 'gradient_mat'
do i=1,953
write(12901,*) gradient_mat(i,:)
enddo
close(12901)

endsubroutine Diff_gradient

subroutine Varcov

!****************************************************************************************************************************************************************************
!	Computes varcovar=DWDinv*D'*W'*OWM*W*D*DWDinv'
!****************************************************************************************************************************************************************************


use params
use filenames
!use IMSL

!****************************************************************************************************************************************************************************
parameter (nskip=1)																							!Number of lines and colums to skip if want to invert just a submatrix																	
double precision,allocatable::OWM(:,:),OWMinv(:,:),W_mat(:,:),D_grad(:,:),Dt(:,:),mat(:,:), mati(:,:), checki(:,:),Dtemp(:,:),DWD(:,:),DWDinv(:,:),DWDinvt(:,:),exclude(:)
double precision,allocatable:: temp(:,:),temp2(:,:),temp3(:,:),temp4(:,:),temp5(:,:),temp6(:,:),temp7(:,:),temp6inv(:,:),temp8(:,:),varcovar(:,:),varcovar2(:,:)
character(len=40):: blah(nskip)
integer::nexcluded
!****************************************************************************************************************************************************************************


nexcluded=161																								! Number of colinear moments to be excluded to allow inversion
nmoments=792																								! Number of noncolinear moments
nparamstot=59
nparamsmax=62																									! Number of parameters
nparams=61

! 1. GET OWM
!***********

allocate(mat(nmoments,nmoments))
allocate(OWM(nmoments,nmoments))
allocate(OWMinv(nmoments,nmoments))
allocate(checki(nmoments,nmoments))

!! Read in moment's varcovar matrix

open(unit=444, file=path//'owm.txt',position='rewind')

do i=1,nskip
	read(fmt=*,unit=444)																					! This is if I want to try with a small submatrix first
enddo
do j=1,nmoments
	read(fmt=*,unit=444) blah(:),OWMinv(:,j)
enddo

close(444)


!! Invert moment's varcovar matrix to obtain Optimal Weighting Matrix, and check result

!!call dmatinv (mat,mati,nmoments) 
!call DLINRG (nmoments, OWMinv, nmoments, OWM, nmoments)

call matpd(nmoments,nmoments,nmoments,OWM,OWMinv,checki)
flag1=0
CHECKINV: do i= 1,nmoments
			dist1=abs(checki(i,i)-1.0)
			if (dist1.gt.0.0001d-0) then 
				flag1=1
!				pause
			endif
enddo CHECKINV
if (flag1.eq.1) then
write(*,*)'****problem with the inverse'
endif


! 2. GET D AND W
!***************

!! Read in gradient matrix with moments's numerical derivatives wrt paramters (Gradient_mat)
!! Exclude colinear moments and store in Gradient_mat_ex

allocate(D_grad(nmoments,nparams))
allocate(Dt(nparams,nmoments))
allocate(W_mat(nmoments,nmoments))
allocate(Dtemp(nmoments,nparamsmax))
allocate(temp8(nmoments+nexcluded,nparamsmax))

!! Read weights from weights.txt

open (unit=777,file=pathdatavsmodel//'weights.txt')
read(unit=777,fmt=*)
do i=1,nmoments+nexcluded
	read(unit=777,fmt=*) blah, weights(i)
	if (weights(i)>0) weights(i)=1/(weights(i)*weights(i))
enddo
close(777)


!! Read gradient into Gradient_mat array from gradient.txt

open (unit=222,file=pathoutput//'gradient.txt')
read(fmt=*,unit=222)

do i=1,nmoments+nexcluded
	k=1
	if (i==354.or.i==372.or.i==384.or.i==396) then
		do j=1,int(nparamstot/3+1)
			read(fmt=*,unit=222)
		enddo
	temp8(i,:)=0
	goto 383
	endif
	do j=1,int(nparamstot/3)
		read(fmt=*,unit=222) temp8(i,k),temp8(i,k+1),temp8(i,k+2)
		k=k+3
	enddo
	read(fmt=*,unit=222) temp8(i,k),temp8(i,k+1)
	383 continue 
enddo
close(222)

!! Read gradient for other parameters from othergrad

open(unit=12977, file=pathoutput//'othergrad2.txt',position='rewind')
do i=1,nmoments+nexcluded
	if (i==354.or.i==372.or.i==384.or.i==396) then
			read(fmt=*,unit=12977)
!	temp8(i,18)=0
!	temp8(i,19)=0
!	temp8(i,39)=0
!	temp8(i,48)=0
!	temp8(i,49)=0
!	temp8(i,50)=0
!	temp8(i,51)=0
	temp8(i,60)=0
	temp8(i,61)=0
	temp8(i,62)=0
	goto 384
	endif
!	read(fmt=*,unit=12977) 	temp8(i,18),temp8(i,19),temp8(i,39),temp8(i,48),temp8(i,49),temp8(i,50),temp8(i,51),temp8(i,60),temp8(i,61),temp8(i,62)
	read(fmt=*,unit=12977) 	temp8(i,60),temp8(i,61),temp8(i,62)
	384 continue 
enddo
close(12977)

!! Exclude collinear moments

allocate(exclude(nexcluded+1))
exclude=(/49,50,51,52,53,54,87,88,89,90,123,124,125,126,167,168,169,170,171,172,185,186,187,188,189,190,199,200,201,202,211,212,213,214,227,228,229,230,231,232,245,246,247,248,249,250,262,263,264,265,266,267,268,273,274,281,282,283,284,285,286,354,372,378,384,396,559,560,561,568,569,570,577,578,579,586,587,588,595,596,597,604,605,606,613,614,615,622,623,624,631,632,633,640,641,642,649,650,651,689,694,699,704,709,714,719,724,740,741,742,743,744,745,782,783,784,785,786,787,800,801,802,803,804,805,818,819,820,821,822,823,836,837,838,839,840,841,852,853,854,855,856,867,868,869,870,871,880,881,882,883,892,893,894,895,902,903,904,911,912,913,100000/)

k=1
D_grad=0.0
W_mat=0.0
do i=1,nmoments+nexcluded
	if (i==exclude(k)) then
		k=k+1
	else
		Dtemp(i-k+1,:)=temp8(i,:)
		W_mat(i-k+1,i-k+1)=weights(i)
	endif
enddo


!! Exclude unidentified parameters

D_grad(:,1:16)=Dtemp(:,1:16)
D_grad(:,17:61)=Dtemp(:,18:62)

Dt=transpose(D_grad)


! 3. GET DWDinv
!***************

allocate(DWD(nparams,nparams))
allocate(DWDinv(nparams,nparams))
allocate(DWDinvt(nparams,nparams))
allocate(temp6(nparams,nmoments))

call matpd(nparams,nmoments,nmoments,Dt,W_mat,temp6)
call matpd(nparams,nmoments,nparams,temp6,D_grad,DWD)

!DWD(1:16,1:16)=temp6(1:16,1:16)
!DWD(17:35,1:16)=temp6(20:38,1:16)
!DWD(36:43,1:16)=temp6(40:47,1:16)
!DWD(44:51,1:16)=temp6(52:59,1:16)

!DWD(1:16,17:35)=temp6(1:16,20:38)
!DWD(17:35,17:35)=temp6(20:38,20:38)
!DWD(36:43,17:35)=temp6(40:47,20:38)
!DWD(44:51,17:35)=temp6(52:59,20:38)

!DWD(1:16,36:43)=temp6(1:16,40:47)
!DWD(17:35,36:43)=temp6(20:38,40:47)
!DWD(36:43,36:43)=temp6(40:47,40:47)
!DWD(44:51,36:43)=temp6(52:59,40:47)

!DWD(1:16,44:51)=temp6(1:16,52:59)
!DWD(17:35,44:51)=temp6(20:38,52:59)
!DWD(36:43,44:51)=temp6(40:47,52:59)
!DWD(44:51,44:51)=temp6(52:59,52:59)

call dmatinv (DWD,DWDinv,nparams) 

deallocate(checki)
allocate(checki(nparams,nparams))

call matpd(nparams,nparams,nparams,DWD,DWDinv,checki)
flag1=0
CHECKINV2: do i= 1,nparams
			dist1=abs(checki(i,i)-1.0)
			if (dist1.gt.0.0001d-0) then 
				flag1=1
!				pause
			endif
enddo CHECKINV2
if (flag1.eq.1) then
write(*,*)'****problem with the inverse'
endif

DWDinvt=transpose(DWDinv)





!! Multiply matrices to get parameters varcovars

allocate(temp(nparams,nparams))
allocate(temp2(nparams,nmoments))
allocate(temp3(nparams,nmoments))
allocate(temp4(nparams,nmoments))
allocate(temp5(nparams,nparams))
allocate(temp7(nparams,nmoments))
allocate(varcovar(nparams,nparams))
allocate(varcovar2(nparams,nparams))

call matpd(nparams,nmoments,nmoments,Dt,W_mat,temp2)
call matpd(nparams,nmoments,nmoments,temp2,OWMinv,temp3)
call matpd(nparams,nmoments,nmoments,temp3,W_mat,temp4)
call matpd(nparams,nmoments,nparams,temp4,D_grad,temp)

call matpd(nparams,nparams,nparams,DWDinv,temp,temp5)
call matpd(nparams,nparams,nparams,temp5,DWDinvt,varcovar)

varcovar=varcovar/33825

open(unit=2323, file=pathoutput//'stderrors.txt',position='rewind')
do i=1,nparams
write(2323,*) varcovar(i,i)
enddo
close(2323)

call matpd(nparams,nmoments,nmoments,Dt,OWM,temp7)
call matpd(nparams,nmoments,nparams,temp7,D_grad,temp5)


!call DLINRG (nparams,temp5, nparams, varcovar2, nparams)
call matpd(nparams,nparams,nparams,temp5,varcovar2,checki)
flag1=0
CHECKINV3: do i= 1,nparams
			dist1=abs(checki(i,i)-1.0)
			if (dist1.gt.0.0001d-0) then 
				flag1=1
!				pause
			endif
enddo CHECKINV3

if (flag1.eq.1) then
write(*,*)'****problem with the inverse'
endif

varcovar2=varcovar2/33825

deallocate(mat)
deallocate(OWM)
deallocate(checki)
deallocate(DWDinv)
deallocate(exclude)
deallocate(temp)
deallocate(temp2)
deallocate(temp3)
deallocate(temp4)
deallocate(temp5)
deallocate(varcovar)
deallocate(D_grad)
deallocate(Dt)
deallocate(W_mat)

endsubroutine
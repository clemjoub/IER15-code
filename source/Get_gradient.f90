subroutine Bump

! This routine perturbs the value of each parameter in order to obtain the numerical derivative of the moments wrt the parameters
use params
use filenames


goto (1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	19,	20,	21,	22,	23,	24,	25,	26,	27,	28,	29,	30,	31,	32,	33	,34,	35,	36,	37,	38,	39,	40,	41,	42,	43,	44,	45,	46,	47,	48,	49,	50,	51,	52,	53,	54,	55,	56,	57,	58,	59,	60,	61,	62	,63	,64	,65	,66	,67	,68	,69	,70	,71	,72	,73	,74	,75	,76	,77	,78	,79	,80	,81,	82,	83,	84,	85,	86,	87,	88,	89,	90,	91	,92	,93,	94,	95,	96,	97,	98,	99,	119,	101,	102,	103,	104,	105,	106,	107,	108,	109,	110,	111,	112,	113,	114,	115,	116,	117,	118) gradient
 

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
39	 Xptransfer          	=	 Xptransfer          		*	(1+hdiff)
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

100 continue

endsubroutine



subroutine Save_bumped_moments

use params
use filenames

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
      
do i=1,nmoments
write(19001,*) loss(1,i)
firstdiff_mat(i,gradient)=loss(1,i)
enddo
close(19001)


endsubroutine


subroutine Invert

! This subroutine reads in the moment's variance covariance matrix and inverts it
! It reads in the matrix sigma1_inv (DWD') and inverts it
! It also reads in the gradient matrix that contains the numerical derivatives of the moments with respect to the parameters and computes the varcovar matrix of the estimated parameters

use params
use filenames
use IMSL

parameter (nskip=1)
double precision,allocatable::mat(:,:), mati(:,:), checki(:,:),sigma1_mat(:,:),Gradient_mat_ex(:,:),exclude(:)
character(len=40):: blah(nskip)
integer::nexcluded



nexcluded=161
nmoments=792
nparams=59

allocate(mat(nmoments,nmoments))
allocate(mati(nmoments,nmoments))
allocate(checki(nmoments,nmoments))
allocate(sigma1_mat(nparams,nparams))

open(unit=444, file=path//'owm.txt',position='rewind')
do i=1,nskip
read(fmt=*,unit=444) 
enddo
do j=1,nmoments
read(fmt=*,unit=444) blah(:),mat(:,j)
enddo
close(444)

!call dmatinv (mat,mati,nmoments) 
!call DLINRG (nmoments, mat, nmoments, mati, nmoments)

!call matpd(nmoments,nmoments,nmoments,mati,mat,checki)
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

deallocate(checki)
allocate(checki(nparams,nparams))


open (unit=111,file=pathoutput//'sigma1_inv.txt')
read(fmt=*,unit=111)
do i=1,nparams
k=1
do j=1,int(nparams/3)
read(fmt=*,unit=111) sigma1_inv(k,i),sigma1_inv(k+1,i),sigma1_inv(k+2,i)
k=k+3
enddo
read(fmt=*,unit=111) sigma1_inv(k,i),sigma1_inv(k+1,i)
enddo
close(111)

call dmatinv (sigma1_inv,sigma1_mat,nparams) 

call matpd(nparams,nparams,nparams,sigma1_inv,sigma1_mat,checki)
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

allocate(Gradient_mat_ex(nmoments,nparams))
exclude=(/49,50,51,52,53,54,87,88,89,90,123,124,125,126,167,168,169,170,171,172,185,186,187,188,189,190,199,200,201,202,211,212,213,214,227,228,229,230,231,232,245,246,247,248,249,250,263,264,265,266,267,268,281,282,283,284,285,286,559,560,561,568,569,570,577,578,579,586,587,588,595,596,597,604,605,606,613,614,615,622,623,624,631,632,633,640,641,642,649,650,651,689,694,699,704,709,714,719,724,740,741,742,743,744,745,782,783,784,785,786,787,800,801,802,803,804,805,818,819,820,821,822,823,836,837,838,839,840,841,852,853,854,855,856,867,868,869,870,871,880,881,882,883,892,893,894,895,902,903,904,911,912,913/)


open (unit=222,file=pathoutput//'gradient.txt')
read(fmt=*,unit=222)
do i=1,nmoments+nexcluded
k=1

if (i==354.or.i==372.or.i==384.or.i==396) then
do j=1,int(nparams/3+1)
read(fmt=*,unit=222)
k=k+3
enddo
Gradient_mat(i,:)=0
goto 383
endif

do j=1,int(nparams/3)
read(fmt=*,unit=222) Gradient_mat(i,k),Gradient_mat(i,k+1),Gradient_mat(i,k+2)
k=k+3
enddo
read(fmt=*,unit=222) Gradient_mat(i,k),Gradient_mat(i,k+1)
383 continue 
enddo
close(222)


allocate(exclude(nexcluded))

k=1
do i=1,nmoments
if (i==exclude(k)) then
k=k+1
else
Gradient_mat_ex(i-k+1,:)=Gradient_mat(i,:)
endif
enddo


deallocate(mat)
deallocate(mati)
!deallocate(checki)
deallocate(sigma1_mat)
deallocate(Gradient_mat_ex)
deallocate(exclude)

endsubroutine
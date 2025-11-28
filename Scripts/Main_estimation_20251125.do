clear 
import excel "C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Price_dispersion data.xlsx", sheet("data") firstrow

gen time=mofd(month)
format month %tm
tsset time, monthly 


***************************************************************************************
**********************************Charts***********************************************
***************************************************************************************

gen b= .
replace b = 0 if time > tm(2017/06) 
replace b = 1 if time < tm(2017/07)

twoway (line hcpi time), ytitle("Inflation") xtitle("time") tline(2017m7)

twoway (line rvp time), ytitle("Relative price dispersion") xtitle("time") 

twoway (scatter	rvp	hcpi)|| qfit rvp hcpi, ytitle("Relative price dispersion") xtitle("Inflation")legend(off)

twoway (scatter	rvp	hcpi)|| qfit rvp hcpi if b==0 || qfit rvp hcpi if b==1, ///
 ytitle("Relative price dispersion") xtitle("Inflation")
 

*********************************************************************************
**************************************GENERATING LOGS****************************
*********************************************************************************

gen lhcpi=log(hcpi)
gen lrvp=log(rvp)


 
*********************************************************************************
**************************************LAG ORDER SELECTION*************************
*********************************************************************************
log using lag_order_stationarity_tests



arimasoc hcpi

arimasoc lrvp

*Best fit for HCPI is ARMA(1,1) 
*Best fit for LVP is ARMA(1,2)

*HCPI
*Selected	(min)	AIC:	ARMA(2,2)
*Selected	(min)	BIC:	ARMA(1,1)
*Selected	(min)	HQIC:	ARMA(1,1)

*LRVP
*Selected	(min)	AIC:	ARMA(1,2)
*Selected	(min)	BIC:	ARMA(1,2)
*Selected	(min)	HQIC:	ARMA(1,2)


*********************************************************************************
**************************************STATIONARITY TEST***************************
*********************************************************************************

dfuller hcpi,lags(1)
pperron hcpi, lags(1)
kpss hcpi

dfuller rvp,lags(1)
pperron rvp, lags(1)
kpss rvp

/*
*************STATIONARITY TESTS************: 

*The ADF and PP tests indicate that the hcpi is stationarity at the 5% level for the ADF and at 10% for the PP test. The KPSS test also supports stationarity

*All tests show that the rvp is also stationarity
 
*/

log close 
translate lag_order_stationarity_tests.smcl lag_order_stationarity_tests.pdf


**********************************************************************************************************
****************ESTIMATE EXPECTED INFLATION FROM ARMA(1,1) MODEL AND INFLATION UNCERTAINTY****************
**********************************************************************************************************

log using arma_model

*Best fit for HCPI is ARMA(1,1) 

arima hcpi, ar(1) ma(1)
predict res_hcpi, residuals 

dwstat
wntestq res_hcpi, lags(10)
swilk hcpi

gen res_hcpi_sq=res_hcpi^2
reg res_hcpi_sq L(1/2).res_hcpi_sq


*e(N)=194
*e(r2)=0.01783579

*ARCH LM test statistic is given by N x R2
gen archlm_test_statistic = 194*0.01783579


*dwstat= Durbin-Watson test for first-order serial correlation
*wntestq= Portmanteau (Q) test for white noise in the residuals or serial correlation
*estat archlm= ARCH LM test
*swilk = Shapiro–Wilk W test for normality

***************************************Results************************************************************
*Durbin–Watson	d-statistic =	1.803806 // no first-order serial correlation

*Portmanteau test p-value is 0.8686 // residuals resemble a white noise process therefore no serial correlation

*Shapiro–Wilk W test p-value is	0.28113 // hcpi is normally distributed 
			
*The archlm_test_statistic= 3.4601433
*Chi-square distribution critical value with degree of freedom = 2 is is 5.991
*Since 3.4601433 < 5.991, there is no evidence of Arch effects

*********************************************************************************************************
*Estimating one-step ahead forecast as expected inflation and inflation uncertainty 

arima hcpi, ar(1) ma(1)
predict hcpi_f, xb   

gen hcpi_u=hcpi-hcpi_f
gen hcpi_uv= hcpi_u^2

*Estimating trend inflation 
tsfilter hp ct=hcpi, trend(hcpi_t)

log close 
translate arma_model.smcl arma_model.pdf


**************************************************************************************************************
log using basic_regressions


*************************************************************************************************************
***********************LIST OF VARIABLES TO BE USED IN REGRESSIONS*******************************************
**************************************************************************************************************
*hcpi = headline inflation
*hcpi_f = expected inflation
*hcpi_uv = unexpected inflation/ inflation uncertainty 
*hcpi_t = trend inflation 
*lrvp = log of relative price dispersion

****************************************************************************************************************
*****************************************************BASIC REGRESSIONS*************************************
****************************************************************************************************************

************************************DETERMINING APPROPRIATE LAGS TO INCLUDE IN REGRESSIONS********************************

varsoc hcpi, maxlag(10) 

///optimal lag is 2

varsoc lrvp, maxlag(10) 

///optimal lag is 3

gen hcpi_sq=hcpi^2

varsoc hcpi_sq

///optimal lag is 2


*********************************************************ESTIMATING STRUCTURAL BREAKS********************************

xtbreak lrvp hcpi L1.hcpi L2.hcpi L1.lrvp L2.lrvp L3.lrvp,vce(hac)

/*				
#	Index	Date	[95% Conf.	Interval]
					
1	33	2011m12	2011m10	2012m2 
2	64	2014m7	2012m3	2016m11
3	100	2017m7	2017m6	2017m8 
4	131	2020m2	2020m1	2020m3 
5	161	2022m8	2022m7	2022m9 				
*/

*********************************************CREATING DUMMY VARIABLES***************************************************
gen dum_2011m12=1 if time == tm(2011/12)
replace dum_2011m12 = 0 if time !=tm(2011/12)

gen dum_2014m7=1 if time == tm(2014/07)
replace dum_2014m7 = 0 if time !=tm(2014/07)

gen dum_2017m7=1 if time == tm(2017/07)
replace dum_2017m7 = 0 if time !=tm(2017/07)

gen dum_2020m2=1 if time == tm(2020/02)
replace dum_2020m2 = 0 if time !=tm(2020/02)

gen dum_2022m8=1 if time == tm(2022/08)
replace dum_2022m8 = 0 if time !=tm(2022/08)

*dum_2011m12 dum_2014m7 dum_2017m7 dum_2020m2 dum_2022m8
*************************************************************************************************************
************************************************REGRESSIONS***************************************************
**************************************************************************************************************


//////////////////////////////////////////// BASIC REGRESSION ////////////////////////////////////////////////

global dvars "dum_2011m12 dum_2014m7 dum_2017m7 dum_2020m2 dum_2022m8"



reg lrvp hcpi L1.lrvp L2.lrvp L3.lrvp i.$dvars, vce(robust)
estimates store full_sample

reg lrvp hcpi L1.lrvp L2.lrvp L3.lrvp i.$dvars if b==1, vce(robust)
estimates store pre_2017

reg lrvp hcpi L1.lrvp L2.lrvp L3.lrvp i.$dvars if b==0, vce(robust)
estimates store post_2017

esttab full_sample pre_2017 post_2017 using baseline_regression_results.rtf, stats(N r2) b(%9.3f) p(3) star(* 0.10 ** 0.05 *** 0.01) drop(_cons)

log close 
translate basic_regressions.smcl basic_regressions.pdf
*************************************************************************************************************************************


*****************************************************COEFFICIENT STABILITY TESTS*******************************************************

////////////////////////////////////////////ROLLING & RECURSIVE REGRESSION  ///////////////////////////////////

rolling _b, window(21) saving(betas,replace) keep(time): reg lrvp hcpi L1.lrvp L2.lrvp L3.lrvp i.$dvars, vce(robust)
rolling _b, window(30) saving(betas,replace) keep(time): reg lrvp hcpi L1.lrvp L2.lrvp L3.lrvp i.$dvars, vce(robust)

rolling _b, window(21)recursive saving(betas,replace) keep(time): reg lrvp hcpi L1.lrvp L2.lrvp L3.lrvp i.$dvars, vce(robust)
rolling _b, window(30)recursive saving(betas,replace) keep(time): reg lrvp hcpi L1.lrvp L2.lrvp L3.lrvp i.$dvars, vce(robust)

log using squared_inflation_regression_results
////////////////////////////////////////////REGRESSION WITH SQUARED INFLATION///////////////////////////////////////////

reg lrvp hcpi hcpi_sq L1.lrvp L2.lrvp L3.lrvp i.$dvars, vce(robust)
estimates store full_sample

reg lrvp hcpi hcpi_sq L1.lrvp L2.lrvp L3.lrvp i.$dvars if b==1, vce(robust)
estimates store pre_2017

reg lrvp hcpi hcpi_sq L1.lrvp L2.lrvp L3.lrvp i.$dvars if b==0, vce(robust)
estimates store post_2017

esttab full_sample pre_2017 post_2017 using squared_inflation_regression_results.rtf, stats(N r2) b(%9.3f) p(3) star(* 0.10 ** 0.05 *** 0.01) drop(_cons)

log close 
translate squared_inflation_regression_results.smcl squared_inflation_regression_results.pdf

////////////////////////////////////////////SEMI-PARAMETRIC REGRESSION///////////////////////////////////////////

npregress series lrvp hcpi i.$dvars

npregress series lrvp hcpi dum_2011m12 dum_2014m7 dum_2017m7 dum_2020m2 dum_2022m8, asis(l1_lrvp l2_lrvp l3_lrvp) kernel(gaussian) imaic vce(bootstrap, reps(500))

gen l1_lrvp =L1.lrvp
gen l2_lrvp =L2.lrvp
gen l3_lrvp =L3.lrvp

*l1_lrvp l2_lrvp l3_lrvp

*dum_2011m12 dum_2014m7 dum_2017m7 dum_2020m2 dum_2022m8

semipar lrvp l1_lrvp l2_lrvp l3_lrvp dum_2011m12 dum_2014m7 dum_2017m7 dum_2020m2 dum_2022m8 nonpar(hcpi) kernel(gaussian)
semipar lrvp l1_lrvp l2_lrvp l3_lrvp dum_2011m12 dum_2014m7 dum_2017m7 dum_2020m2 dum_2022m8 nonpar(hcpi) kernel(Epanechnikov)


semipar lrvp nonpar(hcpi) kernel(gaussian)











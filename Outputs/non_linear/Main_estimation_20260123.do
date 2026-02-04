clear 
import excel "C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Main folder\Price_dispersion_data_20260123.xlsx", sheet("data") firstrow

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

twoway (line rpd time), ytitle("Relative price dispersion") xtitle("time") 

twoway (scatter	rpd	hcpi)|| qfit rpd hcpi, ytitle("Relative price dispersion") xtitle("Inflation")legend(off)

twoway (scatter	rpd	hcpi)|| qfit rpd hcpi if b==0 || qfit rpd hcpi if b==1, ///
 ytitle("Relative price dispersion") xtitle("Inflation")
 

*********************************************************************************
**************************************GENERATING LOGS****************************
*********************************************************************************

gen lrpd=log(rpd)


 
*********************************************************************************
**************************************LAG ORDER SELECTION*************************
*********************************************************************************
log using lag_order_stationarity_tests



arimasoc hcpi

arimasoc lrpd

*Best fit for HCPI is ARMA(1,1) 
*Best fit for LRPD is ARMA(1,2)

*HCPI
*Selected	(min)	AIC:	ARMA(2,2)
*Selected	(min)	BIC:	ARMA(1,1)
*Selected	(min)	HQIC:	ARMA(1,1)

*LRPD
*Selected	(min)	AIC:	ARMA(2,2)
*Selected	(min)	BIC:	ARMA(1,2)
*Selected	(min)	HQIC:	ARMA(2,2)




*********************************************************************************
**************************************STATIONARITY TEST***************************
*********************************************************************************

dfuller hcpi
pperron hcpi
kpss hcpi

dfuller rpd,lags(2)
pperron rpd, lags(2)
kpss rpd

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

gen res_hcpi_sq_res=res_hcpi^2
reg res_hcpi_sq L(1/2).res_hcpi_sq


*e(N)=201
*e(r2)=0.0194

*ARCH LM test statistic is given by N x R2
gen archlm_test_statistic = 201*0.0194


*dwstat= Durbin-Watson test for first-order serial correlation
*wntestq= Portmanteau (Q) test for white noise in the residuals or serial correlation
*estat archlm= ARCH LM test
*swilk = Shapiro–Wilk W test for normality

***************************************Results************************************************************
*Durbin–Watson	d-statistic =	 1.803577 // no first-order serial correlation

*Portmanteau test p-value is 0.7780 // residuals resemble a white noise process therefore no serial correlation

*Shapiro–Wilk W test p-value is	0.14324 // hcpi is normally distributed 
			
*The archlm_test_statistic= 3.8994
*Chi-square distribution critical value with degree of freedom = 2 is is 5.991
*Since 3.4601433 < 5.991, there is no evidence of Arch effects

*********************************************************************************************************
*Estimating one-step ahead forecast as expected inflation and inflation uncertainty 
log using arma_model

arima hcpi, ar(1) ma(1)
predict hcpi_f, xb   

gen hcpi_u=hcpi[_n-1]-hcpi_f
gen hcpi_fev= hcpi_u^2


asrol hcpi, st(sd) win(time 12) gen(hcpi_sd)

gen hcpi_sq=hcpi^2

log close 
translate arma_model.smcl arma_model.pdf

*hcpi_f = expected inflation
*hcpi_u= unexpected inflation
*hcpi_sd = 12-month rolling window standard deviation of inflation 
*hcpi_sq= squared inflation
**************************************************************************************************************
log using basic_regressions


*************************************************************************************************************
***********************LIST OF VARIABLES TO BE USED IN REGRESSIONS*******************************************
**************************************************************************************************************
*hcpi = headline inflation
*hcpi_f = expected inflation
*hcpi_u= unexpected inflation 
*hcpi_sd = standard deviation of inflation 
*hcpi_fev= forecast error variance of inflation 


****************************************************************************************************************
*****************************************************BASIC REGRESSIONS*************************************
****************************************************************************************************************

************************************DETERMINING APPROPRIATE LAGS TO INCLUDE IN REGRESSIONS********************************

///optimal lag is 2

varsoc lrpd, maxlag(10) 

///optimal lag is 3

gen lrpd1 =L1.lrpd
gen lrpd2 =L2.lrpd
gen lrpd3 =L3.lrpd

*********************************************************ESTIMATING STRUCTURAL BREAKS********************************

xtbreak lrpd hcpi L1.hcpi L2.hcpi L1.lrpd L2.lrpd L3.lrpd,vce(hac)

/*				

#	Index	Date	[95% Conf.	Interval]
					
1	33	2011m12	2011m11	2012m1 
2	68	2014m11	2013m3	2016m7 
3	104	2017m11	2017m10	2017m12
4	136	2020m7	2020m6	2020m8 
5	166	2023m1	2022m8	2023m6 
					
*/
*********************************************CREATING DUMMY VARIABLES***************************************************
gen dum_2011m12=1 if time == tm(2011/12)
replace dum_2011m12 = 0 if time !=tm(2011/12)

gen dum_2014m11=1 if time == tm(2014/11)
replace dum_2014m11 = 0 if time !=tm(2014/11)

gen dum_2017m11=1 if time == tm(2017/11)
replace dum_2017m11 = 0 if time !=tm(2017/11)

gen dum_2020m7=1 if time == tm(2020/07)
replace dum_2020m7 = 0 if time !=tm(2020/07)

gen dum_2023m1=1 if time == tm(2023/01)
replace dum_2023m1 = 0 if time !=tm(2023/01)

*dum_2011m12 dum_2014m11 dum_2017m11 dum_2020m7 dum_2023m1
*************************************************************************************************************
************************************************REGRESSIONS***************************************************
**************************************************************************************************************
cd "C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Main folder\Results"

//////////////////////////////////////////// BASIC REGRESSION ////////////////////////////////////////////////
log using basic_regression_results_20260123

global dvars "dum_2011m12 dum_2014m11 dum_2017m11 dum_2020m7 dum_2023m1"


reg lrpd hcpi L1.lrpd L2.lrpd L3.lrpd i.$dvars, vce(robust)
*estimates store full_sample

reg lrpd hcpi L1.lrpd L2.lrpd L3.lrpd i.$dvars if time < tm(2017/07), vce(robust)
*estimates store pre_2017

reg lrpd hcpi L1.lrpd L2.lrpd L3.lrpd i.$dvars if time > tm(2017/06), vce(robust)
*estimates store post_2017

*esttab full_sample pre_2017 post_2017 using baseline_regression_results.rtf, stats(N r2) b(%9.3f) p(3) star(* 0.10 ** 0.05 *** 0.01) drop(_cons)

log close 
translate basic_regression_results_20260123.smcl basic_regression_results_20260123.pdf
*************************************************************************************************************************************


*****************************************************COEFFICIENT STABILITY TESTS*******************************************************

////////////////////////////////////////////ROLLING & RECURSIVE REGRESSION  ///////////////////////////////////
/*
rolling _b, window(21) saving(betas,replace) keep(time): reg lrvp hcpi L1.lrvp L2.lrvp L3.lrvp i.$dvars, vce(robust)
rolling _b, window(30) saving(betas,replace) keep(time): reg lrvp hcpi L1.lrvp L2.lrvp L3.lrvp i.$dvars, vce(robust)

rolling _b, window(21)recursive saving(betas,replace) keep(time): reg lrvp hcpi L1.lrvp L2.lrvp L3.lrvp i.$dvars, vce(robust)
rolling _b, window(30)recursive saving(betas,replace) keep(time): reg lrvp hcpi L1.lrvp L2.lrvp L3.lrvp i.$dvars, vce(robust)
*/


////////////////////////////////////////////REGRESSION WITH SQUARED INFLATION///////////////////////////////////////////
log using squared_inflation_regression_results_20260123

reg lrpd hcpi hcpi_sq L1.lrpd L2.lrpd L3.lrpd i.$dvars, vce(robust)
*estimates store full_sample

reg lrpd hcpi hcpi_sq L1.lrpd L2.lrpd L3.lrpd i.$dvars if time < tm(2017/07), vce(robust)
*estimates store pre_2017

reg lrpd hcpi hcpi_sq L1.lrpd L2.lrpd L3.lrpd i.$dvars if time > tm(2017/06), vce(robust)
*estimates store post_2017

*esttab full_sample pre_2017 post_2017 using squared_inflation_regression_results.rtf, stats(N r2) b(%9.3f) p(3) star(* 0.10 ** 0.05 *** 0.01) drop(_cons)

log close 
translate squared_inflation_regression_results_20260123.smcl squared_inflation_regression_results_20260123.pdf

////////////////////////////////////////////SEMI-PARAMETRIC REGRESSION///////////////////////////////////////////
/*

*nonparametric regression
*npregress series lrvp hcpi i.dum_2011m12 i.dum_2014m7 i.dum_2017m7 i.dum_2020m2 i.dum_2022m8, asis(l1_lrvp l2_lrvp l3_lrvp) 
semipar lrpd lrpd1 lrpd2 lrpd3 if time > tm(2017/06), nonpar(hcpi) kernel(cosine) robust ci


*full sample 
semipar lrpd lrpd1 lrpd2 lrpd3, nonpar(hcpi) kernel(gaussian) robust ci
graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Stata files\Results\Equation_10\full_sample_gaussian_eq10.png", /// 
as(png) name("Graph")

semipar lrpd lrpd1 lrpd2 lrpd3, nonpar(hcpi) kernel(epanechnikov) robust ci
graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Stata files\Results\Equation_10\full_sample_epanechnikov_eq10.png", /// 
as(png) name("Graph")

*Pre-2017
semipar lrpd lrpd1 lrpd2 lrpd3 if time < tm(2017/07), nonpar(hcpi) kernel(gaussian) robust ci
graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Stata files\Results\Equation_10\pre2017_gaussian_eq10.png", /// 
as(png) name("Graph")

semipar lrpd lrpd1 lrpd2 lrpd3 if time < tm(2017/07), nonpar(hcpi) kernel(epanechnikov) robust ci 
graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Stata files\Results\Equation_10\pre2017_epanechnikov_eq10.png", /// 
as(png) name("Graph")

*Post-2017
semipar lrpd lrpd1 lrpd2 lrpd3 if time > tm(2017/06), nonpar(hcpi) kernel(gaussian) robust ci
graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Stata files\Results\Equation_10\post2017_gaussian_eq10.png", /// 
as(png) name("Graph")

semipar lrpd lrpd1 lrpd2 lrpd3 if time > tm(2017/06), nonpar(hcpi) kernel(epanechnikov) robust ci
graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Stata files\Results\Equation_10\post2017_epanechnikov_eq10.png", /// 
as(png) name("Graph")


cd"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Main folder\Results\Equation_10"
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////MANUAL CODE TO IMPLEMENT THE ROBINSONS(1988) SEMIPARAMETRIC ESTIMOR////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


****************************STEP 1: Kernel regression of y on z*********************************

* estimating E[y | z], note: z=nonparametric variable=hcpi


npregress kernel lrpd hcpi, kernel(epanechnikov)
predict m_y, mean
gen y_tilde = lrpd - m_y

******************************STEP 2: Kernel regression of each x on z**************************

* E[x | z] for each parametric regressor, note: x=parametric variables=lrpd1 lrpd2 lrpd3

foreach x in lrpd1 lrpd2 lrpd3 {
    npregress kernel `x' hcpi, kernel(epanechnikov)
    predict m_`x', mean
    gen `x'_tilde = `x' - m_`x'
}


********************************STEP 3: OLS on residuals (Robinson estimator)*******************
*Estimate 𝛽


reg y_tilde lrpd1_tilde lrpd2_tilde lrpd3_tilde, vce(robust)

*********************************STEP 4: Construct partialled-out dependent variable******************
*yt*∗=yt​−xt′​β^​

gen y_hat = lrpd ///
    - _b[lrpd1_tilde]*lrpd1 ///
    - _b[lrpd2_tilde]*lrpd2 ///
    - _b[lrpd3_tilde]*lrpd3

*STEP 5: Final kernel regression to estimate g(z)
	
npregress kernel y_hat hcpi, ///
    kernel(epanechnikov) ///
    vce(bootstrap, reps(500) seed(12345))

************************************STEP 7: Plot the Robinson nonparametric component	

summ hcpi
margins, at(hcpi = (`=r(min)'(0.1)`=r(max)'))


marginsplot, ///
    recast(line) ///
    recastci(rarea) ///
    plotopts(lwidth(medthick) lcolor(black)) ///
    ciopts(color(gs12%50)) ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    title("Full sample (Epanechnikov)") ///
    subtitle("") ///
    ytitle("ĝ(hcpi)") ///
    xtitle("Inflation (hcpi)") ///
    legend(off)

graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Main folder\Results\Equation_10\full_sample_epanechnikov_eq10.png", /// 
as(png) name("Graph")
	
/////////////////////////////////////////////SUB-SAMPLES//////////////////////////////////////////////////////////////////

*Post 2017
	
npregress kernel y_hat hcpi if time > tm(2017/06), ///
    kernel(epanechnikov) ///
    vce(bootstrap, reps(500) seed(12345))

summ hcpi
margins, at(hcpi = (`=r(min)'(0.1)`=r(max)'))


marginsplot, ///
    recast(line) ///
    recastci(rarea) ///
    plotopts(lwidth(medthick) lcolor(black)) ///
    ciopts(color(gs12%50)) ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    title("Post-2017 (Epanechnikov)") ///
    subtitle("") ///
    ytitle("ĝ(hcpi)") ///
    xtitle("Inflation (hcpi)") ///
    legend(off)

graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Main folder\Results\Equation_10\Post2017_epanechnikov_eq10.png", /// 
as(png) name("Graph")
	
*Pre 2017
	
npregress kernel y_hat hcpi if time < tm(2017/07), ///
    kernel(epanechnikov) ///
    vce(bootstrap, reps(500) seed(12345))

summ hcpi
margins, at(hcpi = (`=r(min)'(0.1)`=r(max)'))


marginsplot, ///
    recast(line) ///
    recastci(rarea) ///
    plotopts(lwidth(medthick) lcolor(black)) ///
    ciopts(color(gs12%50)) ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    title("Pre-2017 (Epanechnikov)") ///
    subtitle("") ///
    ytitle("ĝ(hcpi)") ///
    xtitle("Inflation (hcpi)") ///
    legend(off)	

graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Main folder\Results\Equation_10\Pre2017_epanechnikov_eq10.png", /// 
as(png) name("Graph")
	
	

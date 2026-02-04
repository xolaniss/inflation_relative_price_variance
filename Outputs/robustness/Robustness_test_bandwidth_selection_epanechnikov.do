////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////ROBUSTNESS TESTS USING DIFFERENT BANDWITH FOR EQUATION 10////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


summ hcpi
range hcpi_grid r(min) r(max) 100

preserve
keep hcpi
duplicates drop
tempfile grid
save `grid'
restore

local bws 0.4 0.6 0.8 1.0
local j = 1

// 1. Loop over bandwidths and generate fitted values

foreach h of local bws {

    di as txt "Estimating npregress with bandwidth = `h'"

    npregress kernel y_hat hcpi, ///
        kernel(epanechnikov) ///
        bwidth(`h' `h', copy)

    predict double ghat`j', mean

    local ++j
}

*ghat1 ghat2 ghat3 ghat4

// 2. Plot robustness curves (smoothed for clarity)

*Predictions are at observed hcpi, we smooth each curve only for plotting:

twoway ///
    (lpoly ghat1 hcpi, bw(0.4) lcolor(black)) ///
    (lpoly ghat2 hcpi, bw(0.6) lcolor(blue)) ///
    (lpoly ghat3 hcpi, bw(0.8) lcolor(red)) ///
    (lpoly ghat4 hcpi, bw(1.0) lcolor(green)), ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    title("Robustness test: Bandwidth selection") ///
	subtitle("Full sample (epanechnikov)") ///
    ytitle("ĝ(hcpi)") ///
    xtitle("Inflation (hcpi)") ///
    legend(order(1 "h = 0.4" 2 "h = 0.6" 3 "h = 0.8" 4 "h = 1.0")) ///
    graphregion(color(white))

	graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Main folder\Results\Robustness\full_sample_epanechnikov_robust_bandwidth_eq10.png", /// 
as(png) name("Graph")	



///////////////////////////////////////////////////////////////////////////SUB- SAMPLES/////////////////////////////////////////////////////////////////////////////////


*POST-2017

foreach h of local bws {

    di as txt "Estimating npregress with bandwidth = `h'"

    npregress kernel y_hat hcpi if  time > tm(2017/06), ///
        kernel(epanechnikov) ///
        bwidth(`h' `h', copy)

    predict double ghat`j', mean

    local ++j
}

*ghat1 ghat2 ghat3 ghat4

// 2. Plot robustness curves (smoothed for clarity)

*Predictions are at observed hcpi, we smooth each curve only for plotting:

twoway ///
    (lpoly ghat1 hcpi if time > tm(2017/06), bw(0.4) lcolor(black)) ///
    (lpoly ghat2 hcpi if time > tm(2017/06), bw(0.6) lcolor(blue)) ///
    (lpoly ghat3 hcpi if time > tm(2017/06), bw(0.8) lcolor(red)) ///
    (lpoly ghat4 hcpi if time > tm(2017/06), bw(1.0) lcolor(green)), ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    title("Robustness test: Bandwidth selection") ///
	subtitle("Post-2017 (epanechnikov)") ///
    ytitle("ĝ(hcpi)") ///
    xtitle("Inflation (hcpi)") ///
    legend(order(1 "h = 0.4" 2 "h = 0.6" 3 "h = 0.8" 4 "h = 1.0")) ///
    graphregion(color(white))

	graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Main folder\Results\Robustness\Post2017_epanechnikov_robust_bandwidth_eq10.png", /// 
as(png) name("Graph")	


*Pre-2017



foreach h of local bws {

    di as txt "Estimating npregress with bandwidth = `h'"

    npregress kernel y_hat hcpi if  time < tm(2017/07), ///
        kernel(epanechnikov) ///
        bwidth(`h' `h', copy)

    predict double ghat`j', mean

    local ++j
}

*ghat1 ghat2 ghat3 ghat4

// 2. Plot robustness curves (smoothed for clarity)

*Predictions are at observed hcpi, we smooth each curve only for plotting:

twoway ///
    (lpoly ghat1 hcpi if  time < tm(2017/07), bw(0.4) lcolor(black)) ///
    (lpoly ghat2 hcpi if  time < tm(2017/07), bw(0.6) lcolor(blue)) ///
    (lpoly ghat3 hcpi if  time < tm(2017/07), bw(0.8) lcolor(red)) ///
    (lpoly ghat4 hcpi if  time < tm(2017/07), bw(1.0) lcolor(green)), ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    title("Robustness test: Bandwidth selection") ///
	subtitle("Post-2017 (epanechnikov)") ///
    ytitle("ĝ(hcpi)") ///
    xtitle("Inflation (hcpi)") ///
    legend(order(1 "h = 0.4" 2 "h = 0.6" 3 "h = 0.8" 4 "h = 1.0")) ///
    graphregion(color(white))

	graph	export	/// 
"C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Main folder\Results\Robustness\Pre2017_epanechnikov_robust_bandwidth_eq10.png", /// 
as(png) name("Graph")	
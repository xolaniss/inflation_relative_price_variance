

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

foreach h of local bws {

    di as txt "Estimating npregress with bandwidth = `h'"

    npregress kernel y_hat hcpi, ///
        kernel(gaussian) ///
        bwidth(`h' `h', copy)

    predict double ghat`j', mean

    local ++j
}

ghat1 ghat2 ghat3 ghat4

twoway ///
    (lpoly ghat1 hcpi, bw(0.4) lcolor(black)) ///
    (lpoly ghat2 hcpi, bw(0.6) lcolor(blue)) ///
    (lpoly ghat3 hcpi, bw(0.8) lcolor(red)) ///
    (lpoly ghat4 hcpi, bw(1.0) lcolor(green)), ///
    yline(0, lpattern(dash) lcolor(gs8)) ///
    title("Bandwidth Robustness: Nonparametric Effect of Inflation") ///
    ytitle("ĝ(hcpi)") ///
    xtitle("Inflation (hcpi)") ///
    legend(order(1 "h = 0.4" 2 "h = 0.6" 3 "h = 0.8" 4 "h = 1.0")) ///
    graphregion(color(white))

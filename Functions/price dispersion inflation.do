clear
cd "C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation"
import excel "C:\Users\5775\OneDrive - TREASURY\Desktop\Price dispersion_inflation\Price_dispersion data.xlsx", sheet("sub") firstrow

gen time=mofd(month)
format month %tm
tsset time, monthly 


*rename (hinfl-fin_serv) (price=)
*reshape long price, i(time) j(priceid) string
*save price_data

*rename (cerealprod-fin_serv) (weights=)
*reshape long weights, i(time) j(priceid) string
*save weights_data

merge 1:1 priceid time using weights_data.dta




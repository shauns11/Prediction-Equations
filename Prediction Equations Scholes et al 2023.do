********************************************************************
*Scholes S, Ng Fat L, Moody A, et al. Does the use of prediction equations to
*correct self-reported height and weight improve obesity prevalence estimates?
*A pooled cross-sectional analysis of Health Survey for England data. 
*BMJ Open 2023;13:e061809. doi:10.1136/ bmjopen-2022-061809
*Stata V17.
*******************************************************************

use "S:\FPHS_EPH_HSE_Shared\2 HSE 2016-19\HSE 2020\Report\Predicting Ht & Wt\Paper\GitHub\HSE 2011-16 Height and Weight.dta", clear
renvars, lower
count                                   /* n=38,942 */
tab1 wtok
list wtsr wtval2 if wtok==5
*drop if measured weight equals self-reported weight as estimated wt 200+kg.
drop if wtok==5
count                                  /* n=38,940 */

*new variables
gen male = sex==1
gen female = sex==2
generate region = gor1
svyset [pweight=wt_int],psu(cpsu) strata(region)

*Ethnicity (separate category for missing).
tab1 origin2
recode origin2 (-9/-8 = 9)
label define olbl 1 "White" 2 "Black" 3 "Asian" 4 "Mixed" 5 "Other" 9 "Missing"
label values origin2 olbl
tab1 origin2

*Educational status (separate category for missing).
tab1 topqual3 
generate topqual=0
replace topqual=1 if topqual3==1
replace topqual=2 if inlist(topqual3,2,3)
replace topqual=3 if inlist(topqual3,4,5,6)
replace topqual=4 if inlist(topqual3,7)
replace topqual=9 if inlist(topqual3,-9,-8,-1)
label define edlbl 1 "Degree+" 2 "Below degree" 3 "O level/other" 4 "None" 9 "Missing"
label values topqual edlbl


*Age in 5-yr bands.
gen age16g5=0
replace age16g5=1 if inrange(age,16,17)
replace age16g5=2 if inrange(age,18,19)
replace age16g5=3 if inrange(age,20,24)
replace age16g5=4 if inrange(age,25,29)
replace age16g5=5 if inrange(age,30,34)
replace age16g5=6 if inrange(age,35,39)
replace age16g5=7 if inrange(age,40,44)
replace age16g5=8 if inrange(age,45,49)
replace age16g5=9 if inrange(age,50,54)
replace age16g5=10 if inrange(age,55,59)
replace age16g5=11 if inrange(age,60,64)
replace age16g5=12 if inrange(age,65,69)
replace age16g5=13 if inrange(age,70,74)
replace age16g5=14 if inrange(age,75,79)
replace age16g5=15 if inrange(age,80,84)
replace age16g5=16 if inrange(age,85,120)
label define agelbl 1 "16-17" 2 "18-19" 3 "20-24" 4 "25-29" 5 "30-34" 6 "35-39" 7 "40-44" 8 "45-49" 9 "50-54" ///
10 "55-59" 11 "60-64" 12 "65-69" 13 "70-74" 14 "75-79" 15 "80-84" 16 "85+"
label values age16g5 agelbl

*Age in broader groups.
generate age4a=0
replace age4a=1 if inrange(age,16,34)
replace age4a=2 if inrange(age,35,54)
replace age4a=3 if inrange(age,55,74)
replace age4a=4 if inrange(age,75,129)
label define age4lbl 1 "16-34" 2 "35-54" 3 "55-74" 4 "75+" 
label values age4a age4lbl

*General health (missing to modal).
tab1 genhelf2
recode genhelf2 (-8=1)

*Long-standing illness (missing to modal).
tab1 ill12m
recode ill12m (-9/-8 = 2)

*Smoking status (separate category for missing).
tab1 cigsta3
recode cigsta3 (-9/-1 = 9)
label define smokelbl 1 "Current" 2 "Ex-regular" 3 "Never" 9 "Missing"
label values cigsta3 smokelbl


*Binary variables (overweight and obese: SR and M).
gen overwt_sr = inlist(bmisrg5,3,4,5)
gen obese_sr = inlist(bmisrg5,4,5)
gen overwt_m = inlist(bmivg52,3,4,5)
gen obese_m = inlist(bmivg52,4,5)

*Relative difference for continuous variables (at the individual level): (SR-M)/M.
generate ht_rdiff = (htsr - htval)/htval
generate wt_rdiff = (wtsr - wtval2)/wtval2
generate bmi_rdiff = (bmisr - bmival2)/bmival2

*************************************.
***Table S1 (n=38,940)
***Difference in means by survey year
*************************************.

gen overwt_diff = (overwt_sr - overwt_m)
gen obese_diff = (obese_sr - obese_m)

svy:mean ht_diff wt_diff bmi_diff,over(sex hseyr)
svy:mean overwt_diff obese_diff,over(sex hseyr)

*p-for trend (not shown).
svy,subpop(male): regress ht_diff c.hseyr
svy,subpop(male): regress wt_diff c.hseyr
svy,subpop(male): regress bmi_diff c.hseyr
svy,subpop(male): regress overwt_diff c.hseyr
svy,subpop(male): regress obese_diff c.hseyr
svy,subpop(female): regress ht_diff c.hseyr
svy,subpop(female): regress wt_diff c.hseyr
svy,subpop(female): regress bmi_diff c.hseyr
svy,subpop(female): regress overwt_diff c.hseyr
svy,subpop(female): regress obese_diff c.hseyr


**************************************************
***Table 1: Includes those with >4SD (N=38,940).
***p-values for sex difference.
**************************************************

svy:mean htsr htval ht_diff ht_rdiff,over(sex)
lincom _b[c.ht_diff@1bn.sex] - _b[c.ht_diff@2.sex]

svy:mean wtsr wtval2 wt_diff wt_rdiff,over(sex)
lincom _b[c.wt_diff@1bn.sex] - _b[c.wt_diff@2.sex]

svy:mean bmisr bmival2 bmi_diff bmi_rdiff,over(sex)
lincom _b[c.bmi_diff@1bn.sex] - _b[c.bmi_diff@2.sex]

*SD of the differences.
svy:mean ht_diff wt_diff bmi_diff,over(sex)
estat sd

*Excess weight
svy:mean overwt_sr overwt_m,over(sex)
svy:mean overwt_diff,over(sex)
lincom _b[c.overwt_diff@1bn.sex] - _b[c.overwt_diff@2.sex]

*Obesity
svy:mean obese_sr obese_m,over(sex)
svy:mean obese_diff,over(sex)
lincom _b[c.obese_diff@1bn.sex] - _b[c.obese_diff@2.sex]

*Difference within units of measured BMI.
*75% within 2 units of measured BMI.
*summ bmi_diff
*count if inrange(bmi_diff,-1.99,1.99)
*di 29279/38940

*means by single year of age (Figures for the Methodology report)
*trim at 90.
*recode age (91/104=90)
*summ age
*collapse (mean) htsr htval wtsr wtval bmisr bmival2 [pweight=wt_int], by(sex age)

****************************************
***Table S2: Discrepancy by BMI.
****************************************

svy:mean htsr htval ht_diff,over(sex bmivg52)
svy:mean wtsr wtval wt_diff,over(sex bmivg52)
svy:mean bmisr bmival2 bmi_diff,over(sex bmivg52)

************************************************************
***Table S3: percentiles of self-report and measured data.
************************************************************

*Height.
_pctile htsr if sex==1 [pweight=wt_int], p(5 10 25 50 75 90 95) 
return list
_pctile htval if sex==1 [pweight=wt_int], p(5 10 25 50 75 90 95)
return list
_pctile htsr if sex==2 [pweight=wt_int], p(5 10 25 50 75 90 95)
return list
_pctile htval if sex==2 [pweight=wt_int], p(5 10 25 50 75 90 95)
return list

*Weight.
_pctile wtsr if sex==1 [pweight=wt_int], p(5 10 25 50 75 90 95) 
return list
_pctile wtval2 if sex==1 [pweight=wt_int], p(5 10 25 50 75 90 95)
return list
_pctile wtsr if sex==2 [pweight=wt_int], p(5 10 25 50 75 90 95)
return list
_pctile wtval2 if sex==2 [pweight=wt_int], p(5 10 25 50 75 90 95)
return list

*BMI.
_pctile bmisr if sex==1 [pweight=wt_int], p(5 10 25 50 75 90 95) 
return list
_pctile bmival2 if sex==1 [pweight=wt_int], p(5 10 25 50 75 90 95)
return list
_pctile bmisr if sex==2 [pweight=wt_int], p(5 10 25 50 75 90 95)
return list
_pctile bmival2 if sex==2 [pweight=wt_int], p(5 10 25 50 75 90 95)
return list

********************************************************************
*Figure S1: BMI compressed distributions (self-report vs measured).
********************************************************************

twoway kdensity bmisr if sex==1, xtitle("BMI (kg/m{sup:2})") ytitle(Density) color(black) lcolor(black) || ///
kdensity bmival2 if sex==1, color(black) lcolor(black) lpattern(dash) ///
legend(order(1 "Self-reported" 2 "Measured") row(1)) xscale(range(10 70)) xlabel(10(10)70)
graph save "N:\Temp\graph1.gph", replace

twoway kdensity bmisr if sex==2, xtitle("BMI (kg/m{sup:2})") ytitle(Density) color(black) lcolor(black) || ///
kdensity bmival2 if sex==2, color(black) lcolor(black) lpattern(dash) ///
legend(order(1 "Self-reported" 2 "Measured") row(1)) xscale(range(10 70)) xlabel(10(10)70)
graph save "N:\Temp\graph2.gph", replace

*Figures inserted into word document.
grc1leg "N:\Temp\Graph1.gph" "N:\Temp\Graph2.gph", legendfrom("N:\Temp\Graph1.gph") 
graph save "Graph" "N:\Temp\S1.gph", replace
graph use "N:\Temp\S1.gph"
graph export "N:\Temp\S1.tif", width(800) height(600) replace

************************************.
*Table S4
*Figure S2:BA plot (unwt)
*https://blog.uvm.edu/tbplante/2018/07/11/making-scatterplots-and-bland-altman-plots-in-stata/
************************************

*unweighted data.
*LOA for all 3 variables (difference + 2SD): Table S4.

sum ht_diff if sex==1 
di r(sd)
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'
sum wt_diff if sex==1 
di r(sd)
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'
sum bmi_diff if sex==1 
di r(sd)
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'

sum ht_diff if sex==2 
di r(sd)
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'
sum wt_diff if sex==2 
di r(sd)
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'
sum bmi_diff if sex==2 
di r(sd)
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'

*Figure S2 (LOA for BMI)
gen mean_bmi = (bmisr + bmival2)/2

preserve
keep if sex==1 & mean_bmi>10
sum bmi_diff if sex==1 
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
graph twoway scatter bmi_diff mean_bmi, ///
legend(off) mcolor(black) msymbol(Oh) msize(medsmall)  ///
ytitle("Difference in BMI", m(l=0)) /// 
xtitle("Average BMI") /// 
title("Men", size(medsmall) color(black) pos(11)) /// 
yline(`mean1', lpattern(shortdash) lcolor(gray)) ///
yline(`lowerCL1', lpattern(dash) lcolor(gray)) /// 
yline(`upperCL1', lpattern(dash) lcolor(gray)) /// 
graphregion(color(white)) ylabel(, grid glcolor(gs14)) /// white background
graphr(m(l-100)) graphr(m(r-100)) plotr(m(l+0)) plotr(m(r+0)) ///
ylabel(-30(10)50,ang(hor)) xlabel(10(10)70)  /// 
aspectratio(0.4)    
graph save "Graph" "N:\Temp\Graph1.gph", replace
restore

preserve
keep if sex==2 & mean_bmi>10
sum bmi_diff if sex==2 
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
graph twoway scatter bmi_diff mean_bmi, ///
legend(off) mcolor(black) msymbol(Oh) msize(medsmall) ///
ytitle("Difference in BMI", m(l=0)) /// 
xtitle("Average BMI") /// 
title("Women", size(medsmall) color(black) pos(11)) /// 
yline(`mean1', lpattern(shortdash) lcolor(gray)) ///
yline(`lowerCL1', lpattern(dash) lcolor(gray)) /// 
yline(`upperCL1', lpattern(dash) lcolor(gray)) /// 
graphregion(color(white)) ylabel(, grid glcolor(gs14)) /// white background
graphr(m(l-100)) graphr(m(r-100)) plotr(m(l+0)) plotr(m(r+0)) ///
ylabel(-30(10)50,ang(hor)) xlabel(10(10)70)  /// 
aspectratio(0.4)    
graph save "Graph" "N:\Temp\Graph2.gph", replace
restore

graph combine "N:\Temp\Graph1.gph" "N:\Temp\Graph2.gph", rows(2)  imargin(0 0 0 0) ///
plotregion(margin(l=20 r=20)) graphregion(margin(l=-40 r=-40)) iscale(0.8) graphregion(color(white))

graph save "Graph" "N:\Temp\S2.gph", replace
graph use "N:\Temp\S2.gph"
graph export "N:\Temp\S2.tif", replace

********************************************
***Table S4: Distribution of reporting error
********************************************

svy: mean ht_diff,over(sex)
estat sd
svy: mean wt_diff,over(sex)
estat sd
svy: mean bmi_diff,over(sex)
estat sd

*Height.
summ ht_diff if sex==1 [aweight=wt_int]
di r(min)
di r(max)
_pctile ht_diff if sex==1 [pweight=wt_int], p(5 25 50 75 95) 
return list
summ ht_diff if sex==2 [aweight=wt_int]
di r(min)
di r(max)
_pctile ht_diff if sex==2 [pweight=wt_int], p(5 25 50 75 95) 
return list

*Weight.
summ wt_diff if sex==1 [aweight=wt_int]
di r(min)
di r(max)
_pctile wt_diff if sex==1 [pweight=wt_int], p(5 25 50 75 95) 
return list
summ wt_diff if sex==2 [aweight=wt_int]
di r(min)
di r(max)
_pctile wt_diff if sex==2 [pweight=wt_int], p(5 25 50 75 95) 
return list

*BMI.
summ bmi_diff if sex==1 [aweight=wt_int]
di r(min)
di r(max)
_pctile bmi_diff if sex==1 [pweight=wt_int], p(5 25 50 75 95) 
return list
summ bmi_diff if sex==2 [aweight=wt_int]
di r(min)
di r(max)
_pctile bmi_diff if sex==2 [pweight=wt_int], p(5 25 50 75 95) 
return list

*********************************
*Fig S3
*Misreporting by BMI (boxplot).
*********************************

svy: mean bmi_diff,over(sex bmivg52)
estat sd

label define sexlbl 1 "Men" 2 "Women"
label values sex sexlbl 
label variable bmi_diff "Self-report minus measured BMI"
label define bmilbl 1 "Under      weight" 2 "Normal" 3 "Overweight but not obese" 4 "Obese I & II" 5 "Obese III" 
label values bmivg52 bmilbl
splitvallabels bmivg52, length(11) recode
graph box bmi_diff [pweight=wt_int], over(bmivg52, relabel(`r(relabel)'))  nooutsides by(sex,note("")) 

*Graph editor: text small.
graph save "Graph" "N:\Temp\S3.gph", replace
graph use "N:\Temp\S3.gph"
graph export "N:\Temp\S3.tif", replace

***************************************************************
***Table 2:cross-classification (5 categories): SR vs measured.
***************************************************************

svy: tab bmisrg5 bmivg52 if sex==1, col
svy: tab bmisrg5 bmivg52 if sex==2, col
svy: tab bmisrg5 bmivg52 if sex==1, count format(%9.2f)
svy: tab bmisrg5 bmivg52 if sex==2, count format(%9.2f)

****************************************
***Sensitivity & specificity (obesity)
****************************************

svy:tab obese_m obese_sr if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])

svy:tab obese_m obese_sr if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])

*Adults.
svy:tab obese_m obese_sr, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])

****************************************
***Sensitivity & specificity (overwt)
****************************************

svy:tab overwt_m overwt_sr if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])

svy:tab overwt_m overwt_sr if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])

*Adults.
svy:tab overwt_m overwt_sr, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])


**************************************************
*Outliers to exclude from prediction equations.
*************************************************

egen ht_diffz = std(ht_diff)
egen wt_diffz = std(wt_diff)

gen flag1 = abs(ht_diffz)>4 & (ht_diffz!=.)
gen flag2 = abs(wt_diffz)>4 & (wt_diffz!=.)

summ ht_diff if flag1==1
summ wt_diff if flag2==1
tab1 flag1 flag2 

drop if (flag1==1)   // 189 (height)
drop if (flag2==1)  //  276 (weight)

summ ht_diffz wt_diffz 
histogram ht_diff
histogram wt_diff
count 

***********************************
***Analysis by split-samples
***********************************

use "S:\FPHS_EPH_HSE_Shared\2 HSE 2016-19\HSE 2020\Report\Predicting Ht & Wt\Paper\GitHub\HSR 2011-16 Height and Weight_withSPLIT.dta", clear
*drop 2 cases with measured weight = self-reported weight.
*n=38,475.
drop if wtok==5

tab split sex
*A (n=27,033); B (n=11,442).

*Trim age at 90y (n=115).
replace age=90 if age>90
gen agesq=(age^2)
gen agecub=(age^3)


*************************************************
*Identify significant predictors of misreporting
*************************************************.

preserve
keep if split==0

*Difference (height: men)
*Table S5a.
svy,subpop(male):regress ht_diff htsr htsr_sq i.age16g5 ib8.gor i.topqual i.origin2 i.genhelf
di e(r2)
testparm i.age16g5 
testparm i.gor     
testparm 2.topqual 3.topqual 4.topqual  
testparm 2.origin2 3.origin2 4.origin2    
testparm i.genhelf
svy,subpop(male):regress ht_diff htsr htsr_sq i.age16g5
testparm i.age16g5 

*Difference (height: women)
*Table S5b.
svy,subpop(female):regress ht_diff htsr i.age16g5 ib8.gor i.topqual i.qimd i.origin2 
testparm i.age16g5 
testparm i.gor     
testparm 2.topqual 3.topqual 4.topqual  
testparm 2.origin2 3.origin2 4.origin2    
testparm i.qimd
svy,subpop(female):regress ht_diff htsr i.age16g5 
testparm i.age16g5 

*Difference (weight: men)
*Table S6a.
svy,subpop(male):regress wt_diff wtsr wtsr_sq wtsr_cub i.age16g5 i.cigsta3 i.topqual 
testparm i.age16g5 
testparm 2.topqual 3.topqual 4.topqual  
testparm 2.cigsta3 3.cigsta3
svy,subpop(male):regress wt_diff wtsr wtsr_sq wtsr_cub i.age16g5 
testparm i.age16g5 

*Difference (weight: women)
*Table S6b.
svy,subpop(female):regress wt_diff wtsr wtsr_sq wtsr_cub i.age16g5 i.cigsta3 i.origin2 
testparm i.age16g5 
testparm 2.cigsta3 3.cigsta3
testparm 2.origin2 3.origin2 4.origin2
svy,subpop(female):regress wt_diff wtsr wtsr_sq wtsr_cub i.age16g5 
testparm i.age16g5 

******************************************************
***Prediction equations (estimated using sample A).
***Equation 2 = Reduced model.
***Equation 3 = Reduced model.
***OHID (model using continuous age).
******************************************************

*Measured height as outcome.
svy,subpop(male):regress htval htsr htsr_sq i.age16g5 ib8.gor i.topqual i.origin2 i.genhelf
svy,subpop(male):regress htval htsr htsr_sq i.age16g5 
svy,subpop(female):regress htval htsr i.age16g5 ib8.gor i.topqual i.qimd i.origin2 
svy,subpop(female):regress htval htsr i.age16g5 

*Measured weight as outcome.
svy,subpop(male):regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5 i.cigsta3 i.topqual 
svy,subpop(male):regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5
svy,subpop(female):regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5 i.cigsta3 i.origin2
svy,subpop(female):regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5 

*OHID approach (fit equivalent model).
svy,subpop(male):regress htval htsr htsr_sq htsr_cub c.age c.agesq 
svy,subpop(female):regress htval htsr htsr_sq htsr_cub c.age c.agesq 
svy,subpop(male):regress wtval wtsr wtsr_sq wtsr_cub c.age c.agesq
svy,subpop(female):regress wtval wtsr wtsr_sq wtsr_cub c.age c.agesq

************************************************************
*Equation 1 = Intercept + SR alone (linear & non-linear)
*RMSE & adjusted R-squared (Table 4)
************************************************************

svy,subpop(male):regress htval htsr htsr_sq 
svy,subpop(female):regress htval htsr  
svy,subpop(male):regress wtval2 wtsr wtsr_sq wtsr_cub
svy,subpop(female):regress wtval2 wtsr wtsr_sq wtsr_cub  

qui: regress htval htsr htsr_sq if sex==1 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui: regress htval htsr if sex==2 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui: regress wtval2 wtsr wtsr_sq wtsr_cub if sex==1 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui: regress wtval2 wtsr wtsr_sq wtsr_cub if sex==2 [aw=wt_int]
di e(rmse)
di e(r2_a) 

*************************************
*Equation 2 (reduced model: age + SR)
*RMSE & adjusted R-squared (Table 4)
*************************************

qui: regress htval htsr htsr_sq i.age16g5 if sex==1 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui: regress htval htsr i.age16g5 if sex==2 [aw=wt_int] 
di e(rmse)
di e(r2_a) 
qui: regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5 if sex==1 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui: regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5 if sex==2 [aw=wt_int] 
di e(rmse)
di e(r2_a) 

*****************************************************
*Equation 3 RMSE (full model: age + SR + other vars)
*RMSE & adjusted R-squared (Table 4)
*****************************************************

qui:regress htval htsr htsr_sq i.age16g5 ib8.gor i.topqual i.origin2 i.genhelf if sex==1 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui:regress htval htsr i.age16g5 ib8.gor i.topqual i.qimd i.origin2 if sex==2 [aw=wt_int] 
di e(rmse)
di e(r2_a) 
qui:regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5 i.cigsta3 i.topqual if sex==1 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui:regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5 i.cigsta3 i.origin2 if sex==2 [aw=wt_int] 
di e(rmse)
di e(r2_a) 

************************************************************
*Equation 4 RMSE (full model: age + SR + other vars + bmi)
*RMSE & adjusted R-squared (Table 4)
*************************************************************

svy,subpop(male):regress htval htsr htsr_sq i.age16g5 ib8.gor i.topqual i.origin2 i.genhelf c.bmisr
svy,subpop(female):regress htval htsr i.age16g5 ib8.gor i.topqual i.qimd i.origin2 c.bmisr
svy,subpop(male):regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5 i.cigsta3 i.topqual c.bmisr
svy,subpop(female):regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5 i.cigsta3 i.origin2 c.bmisr

qui:regress htval htsr htsr_sq i.age16g5 ib8.gor i.topqual i.origin2 i.genhelf c.bmisr if sex==1 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui:regress htval htsr i.age16g5 ib8.gor i.topqual i.qimd i.origin2 c.bmisr if sex==2 [aw=wt_int] 
di e(rmse)
di e(r2_a) 
qui:regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5 i.cigsta3 i.topqual c.bmisr if sex==1 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui:regress wtval2 wtsr wtsr_sq wtsr_cub i.age16g5 i.cigsta3 i.origin2 c.bmisr if sex==2 [aw=wt_int] 
di e(rmse)
di e(r2_a) 

****************************************
*Model with continuous age: RMSE (OHID)
****************************************

qui:regress htval htsr htsr_sq htsr_cub c.age c.agesq if sex==1 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui:regress htval htsr htsr_sq htsr_cub c.age c.agesq  if sex==2 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui:regress wtval wtsr wtsr_sq wtsr_cub c.age c.agesq if sex==1 [aw=wt_int]
di e(rmse)
di e(r2_a) 
qui:regress wtval wtsr wtsr_sq wtsr_cub c.age c.agesq if sex==2 [aw=wt_int]
di e(rmse)
di e(r2_a) 

restore

*******************
*Split-sample B.
*******************

keep if split==1

*****************************
***Prediction equations
*****************************

*************************
*Full model (equation 3)
*************************

generate ht_full = (101.7606*1) + (-0.0533010*htsr) + (0.0026815*htsr_sq) ///
+ (-0.4613813*age2) + (-0.1200571*age3) + (-0.0558118*age4) + (0.2460203*age5) ///
+ (0.0306223*age6) + (-0.3694480*age7) + (-0.3918746*age8) + (-0.4704585*age9) + (-0.8267949*age10) ///
+ (-1.1036310*age11) + (-1.568086*age12) + (-2.1534130*age13) + (-2.5167370*age14) + (-2.9270480*age15) + (-4.102999*age16) ///
+ (-0.3164024*gor1) + (-0.1986344*gor2) + (-0.1966955*gor3) + (-0.0226586*gor4) ///
+ (-0.2963956*gor5) + (-0.1885595*gor6) + (-0.1220853*gor7) + (-0.0703802*gor9) ///
+ (-0.1411592*educ2) + (-0.2755694*educ3) + (-0.244201*educ4) + (-0.6037786*educ5) ///
+ (0.2990378*eth2) + (-0.6052317*eth3) + (-0.385761*eth4) + (-0.1526833*eth5) + (0.2628674*eth6) ///
+ (-0.1234804*genhelf2) + (-0.459121*genhelf3) if sex==1

replace ht_full = (21.80907*1) + (0.8650312*htsr) ///
+ (0.2214808*age2) + (0.5148834*age3) + (0.5633223*age4) + (0.3101277*age5) ///
+ (0.1080282*age6) + (0.1288338*age7) + (0.1135572*age8) + (0.1465053*age9) ///
+ (-0.2675223*age10) + (-0.6755494*age11) + (-1.056695*age12) + (-1.606787*age13) ///
+ (-2.348654*age14) + (-3.526744*age15) + (-4.370194*age16) ///
+ (-0.092997*gor1) + (-0.2610506*gor2) + (-0.1577138*gor3) + (-0.0389468*gor4) ///
+ (-0.131362*gor5) + (0.0479478*gor6) + (-0.0460704*gor7) + (-0.014914*gor9) ///
+ (-0.0968758*educ2) + (-0.185866*educ3) + (-0.5193791*educ4) + (-0.3678555*educ5) ///
+ (-0.0504865*qimd2) + (-0.0996165*qimd3) + (-0.2515876*qimd4) + (-0.1536209*qimd5) ///
+ (-0.5287159*eth2) + (-1.784675*eth3) + (-0.5003584*eth4) + (-1.695959*eth5) + (0.0656194*eth6) if sex==2

generate wt_full = (10.20434*1) + (0.6758892*wtsr) + (0.0035785*wtsr_sq) + (-0.0000122*wtsr_cub) ///
+ (0.4965258*age2) + (-0.4326935*age3) + (-0.3208852*age4) + (-0.5302439*age5) + (-0.2170832*age6) ///
+ (0.054655*age7) + (0.2218698*age8) + (0.3027502*age9) + (0.4338614*age10) + (0.5565948*age11) + (0.3396468*age12) ///
+ (0.2848636*age13) + (0.233975*age14) + (-0.0250782*age15) + (0.3228306*age16) ///
+ (-0.1976686*educ2) + (-0.3050748*educ3) + (-0.3648748*educ4) + (0.668142*educ5) ///
+ (0.6292955*smoke2) + (0.6545202*smoke3) + (0.966938*smoke4) if sex==1

replace wt_full = (2.111771*1) + (0.9482425*wtsr) + (0.001364*wtsr_sq) + (-0.00000745*wtsr_cub)	///
+ (-0.8208428*age2)	+ (-0.7922089*age3)	+ (-1.081656*age4) + (-0.939252*age5) + (-0.8666134*age6)	///	
+ (-0.8245536*age7)	+ (-0.5640969*age8)	+ (-0.5512733*age9)	+ (-0.6850259*age10) + (-0.6509221*age11)	///
+ (-0.5922547*age12) + (-0.953013*age13) + (-0.5620425*age14) + (-0.7627991*age15) + (-1.377856*age16)	///
+ (0.2548852*smoke2) + (0.178014*smoke3) + (0.3590164*smoke4) ///		
+ (0.8706528*eth2) + (-0.0112801*eth3) + (-0.2439044*eth4) + (-0.3709003*eth5) + (0.7157895*eth6) if sex==2

****************************
*Reduced model (equation 2)
*****************************

generate ht_reduced = (93.8731*1) + (0.0231991*htsr) + (0.0024861*htsr_sq) ///
+ (-0.4039306*age2) + (-0.035466*age3) + (0.0551268*age4) + (0.3571877*age5) + (0.1395165*age6) ///
+ (-0.253083*age7) + (-0.3117672*age8) + (-0.3896362*age9) + (-0.7345478*age10) + (-1.0011080*age11) ///
+ (-1.462859*age12) + (-2.088562*age13) + (-2.451744*age14) + (-2.85031*age15) + (-4.025489*age16) if sex==1

replace ht_reduced = (18.14952*1) + (0.8832393*htsr) ///
+ (0.3286347*age2) + (0.6443697*age3) + (0.7124351*age4) + (0.387677*age5) + (0.2369764*age6) ///
+ (0.280114*age7) + (0.3177175*age8) + (0.3664216*age9) + (-0.0076389*age10) + (-0.4492031*age11) ///
+ (-0.848106*age12) + (-1.397796*age13) + (-2.171494*age14) + (-3.361917*age15) + (-4.179113*age16) if sex==2

generate wt_reduced = (9.383598*1) + (0.7093852*wtsr) + (0.0032741*wtsr_sq) + (-0.0000113*wtsr_cub) ///
+ (0.4426946*age2) + (-0.4775178*age3) + (-0.3852971*age4) + (-0.5712218*age5) + (-0.2291233*age6) ///
+ (0.0328016*age7) + (0.1757247*age8) + (0.2547102*age9) + (0.3938076*age10) + (0.5283512*age11) ///
+ (0.3278975*age12) + (0.2533954*age13) + (0.2165129*age14) + (-0.0332042*age15) + (0.3258359*age16) if sex==1

replace wt_reduced = (2.072887*1) + (0.9551913*wtsr) + (0.0013055*wtsr_sq)	+ (-0.0000073*wtsr_cub) ///
+ (-0.821515*age2) + (-0.833694*age3) + (-1.127133*age4) + (-0.9624883*age5) + (-0.8909782*age6) ///
+ (-0.8519458*age7)	+ (-0.594578*age8) + (-0.5842548*age9)	+ (-0.7201148*age10) + (-0.6756675*age11) ///
+ (-0.611651*age12)	+ (-0.960789*age13)	+ (-0.5648435*age14) + (-0.7683348*age15) + (-1.3772*age16)	if sex==2

**********************************
*OHID (model with continuous age)
**********************************

generate ht_OHID = 292.2272 + (-3.376333*htsr) + (.0217649*htsr_sq) + (-.0000364*htsr_cub) + (.0788123*age) + (-.001256*agesq) if sex==1
replace ht_OHID = 160.34684 + (-1.7360242*htsr) + (.01590043*htsr_sq) + (-.00003211*htsr_cub) + (.11196812*age) + (-.00160464*agesq) if sex==2

generate wt_OHID = 9.912568 + (.6653765*wtsr) + (.0036857*wtsr_sq) + (-0.0000125*wtsr_cub) + (.0319996*age) + (-.0001959*agesq) if sex==1
replace wt_OHID = 1.281195 + (.9487828*wtsr) + (.001376*wtsr_sq) + (-0.00000755*wtsr_cub) + (.0083899*age) + (-.0000739*agesq) if sex==2

*********************************************************************************
*Correction for an all-mean/reference individual: estimated from the full model.
*See code at the bottom.
*(Constant for the difference) * -1.
*Pages 9 and 12 of Supplemental data
**********************************************************************************

generate mc_ht = htsr + (0.954*-1) if sex==1  		
replace mc_ht = htsr + (-0.214*-1) if sex==2 

generate mc_wt = wtsr + (-0.952*-1) if sex==1 		
replace mc_wt = wtsr + (-2.617*-1) if sex==2 

*Equation 1: SR alone (incl. non-linear.
generate eq1_ht = 90.0101 + (.0373125*htsr) + (0.0025108*htsr_sq) if sex==1     
replace eq1_ht = 13.16088 + (.9127864*htsr) if sex==2

generate eq1_wt = 9.10517 + (0.7147987*wtsr) + (0.0032576*wtsr_sq) + (-0.0000113*wtsr_cub) if sex==1      
replace eq1_wt =  1.33507 + (0.9533398*wtsr) + (0.0013344*wtsr_sq) + (-0.00000743*wtsr_cub) if sex==2 

*******************************
*Equation 4: Full model + BMI
*******************************

generate eq4_ht = (98.67712*1) + (-0.0300754*htsr) + (0.0026198*htsr_sq) ///
+ (-0.5078881*age2) + (-0.2272704*age3) + (-0.2177201*age4) + (0.0533942*age5) ///
+ (-0.172164*age6) + (-0.5972308*age7) + (-0.6307092*age8) + (-0.7168566*age9) + (-1.061243*age10) ///
+ (-1.326523*age11) + (-1.774754*age12) + (-2.343951*age13) + (-2.694919*age14) + (-3.068414*age15) + (-4.202901*age16) ///
+ (-0.3230207*gor1) + (-0.2070022*gor2) + (-0.2028142*gor3) + (-0.0305691*gor4) ///
+ (-0.3119264*gor5) + (-0.1912949*gor6) + (-0.1095217*gor7) + (-0.0781588*gor9) ///
+ (-0.1661716*educ2) + (-0.3016161*educ3) + (-0.2725106*educ4) + (-0.6147789*educ5) ///
+ (0.2928679*eth2) + (-0.5681657*eth3) + (-0.3757874*eth4) + (-0.1996606*eth5) + (0.2561701*eth6) ///
+ (-0.1820896*genhelf2) + (-0.4974663*genhelf3) + (0.0428221*bmisr) if sex==1

replace eq4_ht = (20.887320*1) + (0.867191*htsr) ///
+ (0.200961*age2) + (0.454219*age3) + (0.473330*age4) + (0.209916*age5) ///
+ (-0.008911*age6) + (0.014854*age7) + (-0.018394*age8) + (0.018977*age9) ///
+ (-0.400452*age10) + (-0.810310*age11) + (-1.177833*age12) + (-1.723426*age13) ///
+ (-2.462256*age14) + (-3.604324*age15) + (-4.424119*age16) ///
+ (-0.099183*gor1) + (-0.254628*gor2) + (-0.158167*gor3) + (-0.040837*gor4) ///
+ (-0.136609*gor5) + (0.049560*gor6) + (-0.018553*gor7) + (-0.004780*gor9) ///
+ (-0.118811*educ2) + (-0.222691*educ3) + (-0.553346*educ4) + (-0.346344*educ5) ///
+ (-0.062286*qimd2) + (-0.119954*qimd3) + (-0.279462*qimd4) + (-0.195382*qimd5) ///
+ (-0.569707*eth2) + (-1.753472*eth3) + (-0.497232*eth4) + (-1.696200*eth5) + (-0.039348*eth6) + (0.027576*bmisr) if sex==2

generate eq4_wt = (10.17686*1) + (0.6743142*wtsr) + (0.0035808*wtsr_sq) + (-0.0000122*wtsr_cub) ///
+ (0.4967057*age2) + (-0.4346752*age3) + (-0.3244171*age4) + (-0.5350454*age5) + (-0.2232918*age6) ///
+ (0.0472204*age7) + (0.2145416*age8) + (0.2937064*age9) + (0.4242794*age10) + (0.5477569*age11) + (0.3301064*age12) ///
+ (0.2761483*age13) + (0.2238295*age14) + (-0.0345266*age15) + (0.3135592*age16) ///
+ (-0.1991842*educ2) + (-0.3075669*educ3) + (-0.3700028*educ4) + (0.6666487*educ5) ///
+ (0.6278522*smoke2) + (0.6531416*smoke3) + (0.9650958*smoke4) + (0.0061256*bmisr) if sex==1

replace eq4_wt = (2.4911050*1) + (0.9523166*wtsr) + (0.0014777*wtsr_sq) + (-0.0000078*wtsr_cub)	///
+ (-0.8223317*age2)	+ (-0.7949627*age3)	+ (-1.0744930*age4) + (-0.9210492*age5) + (-0.8466874*age6)	///	
+ (-0.7959954*age7)	+ (-0.5298759*age8)	+ (-0.5099634*age9)	+ (-0.6350665*age10) + (-0.5952656*age11)	///
+ (-0.5307970*age12) + (-0.8847615*age13) + (-0.4878789*age14) + (-0.6898060*age15) + (-1.2897840*age16)	///
+ (0.2530563*smoke2) + (0.1726729*smoke3) + (0.3657567*smoke4) ///	
+ (0.8823087*eth2) + (0.0416996*eth3) + (-0.2253796*eth4) + (-0.3201485*eth5) + (0.7449050*eth6) + (-0.0429854*bmisr) if sex==2

*****************
*Derive BMI.
*****************

generate bmi_full = (wt_full)/((ht_full/100)^2)
generate bmi_reduced = (wt_reduced)/((ht_reduced/100)^2)
generate bmi_OHID = (wt_OHID)/((ht_OHID/100)^2)
generate mc_bmi = (mc_wt)/((mc_ht/100)^2)
generate eq1_bmi = (eq1_wt)/((eq1_ht/100)^2)
generate eq4_bmi = (eq4_wt)/((eq4_ht/100)^2)

*BMI status (mean-correction).
gen bmivg52_mc=0
replace bmivg52_mc=1 if inrange(mc_bmi,0,18.49999)
replace bmivg52_mc=2 if inrange(mc_bmi,18.5,24.9999)
replace bmivg52_mc=3 if inrange(mc_bmi,25,29.99999)
replace bmivg52_mc=4 if inrange(mc_bmi,30,39.99999)
replace bmivg52_mc=5 if inrange(mc_bmi,40,199.99999)
label values bmivg52_mc bmivg5lbl
tab1 bmivg52_mc

*BMI status (Equation 1).
gen bmivg52_eq1=0
replace bmivg52_eq1=1 if inrange(eq1_bmi,0,18.49999)
replace bmivg52_eq1=2 if inrange(eq1_bmi,18.5,24.9999)
replace bmivg52_eq1=3 if inrange(eq1_bmi,25,29.99999)
replace bmivg52_eq1=4 if inrange(eq1_bmi,30,39.99999)
replace bmivg52_eq1=5 if inrange(eq1_bmi,40,199.99999)
label values bmivg52_eq1 bmivg5lbl
tab1 bmivg52_eq1

*BMI status (Equation 4).
gen bmivg52_eq4=0
replace bmivg52_eq4=1 if inrange(eq4_bmi,0,18.49999)
replace bmivg52_eq4=2 if inrange(eq4_bmi,18.5,24.9999)
replace bmivg52_eq4=3 if inrange(eq4_bmi,25,29.99999)
replace bmivg52_eq4=4 if inrange(eq4_bmi,30,39.99999)
replace bmivg52_eq4=5 if inrange(eq4_bmi,40,199.99999)
label values bmivg52_eq4 bmivg5lbl
tab1 bmivg52_eq4

*BMI status (Full-model: equation 3).
gen bmivg52_f=0
replace bmivg52_f=1 if inrange(bmi_full,0,18.49999)
replace bmivg52_f=2 if inrange(bmi_full,18.5,24.9999)
replace bmivg52_f=3 if inrange(bmi_full,25,29.99999)
replace bmivg52_f=4 if inrange(bmi_full,30,39.99999)
replace bmivg52_f=5 if inrange(bmi_full,40,199.99999)
label values bmivg52_f bmivg5lbl
tab1 bmivg52_f

*BMI status (Reduced-model: equation 2).
gen bmivg52_r=0
replace bmivg52_r=1 if inrange(bmi_reduced,0,18.49999)
replace bmivg52_r=2 if inrange(bmi_reduced,18.5,24.9999)
replace bmivg52_r=3 if inrange(bmi_reduced,25,29.99999)
replace bmivg52_r=4 if inrange(bmi_reduced,30,39.99999)
replace bmivg52_r=5 if inrange(bmi_reduced,40,199.99999)
label values bmivg52_r bmivg5lbl
tab1 bmivg52_r

*BMI status (OHID: model with continuous age)
gen bmivg52_OHID=0
replace bmivg52_OHID=1 if inrange(bmi_OHID,0,18.49999)
replace bmivg52_OHID=2 if inrange(bmi_OHID,18.5,24.9999)
replace bmivg52_OHID=3 if inrange(bmi_OHID,25,29.99999)
replace bmivg52_OHID=4 if inrange(bmi_OHID,30,39.99999)
replace bmivg52_OHID=5 if inrange(bmi_OHID,40,199.99999)
label values bmivg52_OHID bmivg5lbl
tab1 bmivg52_OHID

*Binary variables for overweight (excess weight).
gen overwt_f= inlist(bmivg52_f,3,4,5)
gen overwt_r= inlist(bmivg52_r,3,4,5)
gen overwt_mc= inlist(bmivg52_mc,3,4,5)
gen overwt_eq1= inlist(bmivg52_eq1,3,4,5)
gen overwt_eq4= inlist(bmivg52_eq4,3,4,5)
gen overwt_OHID= inlist(bmivg52_OHID,3,4,5)

*Binary variables for obese.
gen obese_f= inlist(bmivg52_f,4,5)
gen obese_r= inlist(bmivg52_r,4,5)
gen obese_mc= inlist(bmivg52_mc,4,5)
gen obese_eq1= inlist(bmivg52_eq1,4,5)
gen obese_eq4= inlist(bmivg52_eq4,4,5)
gen obese_OHID= inlist(bmivg52_OHID,4,5)

*********************************************
*Table 3: self-reported, measured, corrected.
*********************************************

svy:mean htsr htval ht_full ht_reduced ht_OHID,over(sex)
svy:mean wtsr wtval2 wt_full wt_reduced wt_OHID,over(sex)
svy:mean bmisr bmival2 bmi_full bmi_reduced bmi_OHID,over(sex)
svy:mean overwt_sr overwt_m overwt_f overwt_r overwt_OHID,over(sex)
svy:mean obese_sr obese_m obese_f obese_r obese_OHID,over(sex)

*BMI status (not shown in final).
*svy:tab bmisrg5 sex, col ci percent format(%9.1f)           /* SR */
*svy:tab bmivg52 sex, col ci percent format(%9.1f)           /* Measured */
*svy:tab bmivg52_f sex, col ci percent format(%9.1f)  		/* Full */
*svy:tab bmivg52_r sex, col ci percent format(%9.1f)         /* Reduced */
*svy:tab bmivg52_OHID sex, col ci percent format(%9.1f)      /* OHID (continuous age) */

*means by single year of age (Figures for the Methodology report)
*collapse (mean) htsr ht_full htval wtsr wt_full wtval bmisr bmi_full bmival2 [pweight=wt_int], by(sex age)


*Cross-classification (not shown in final).

*Compare SR and measured
*svy: tab bmisrg5 bmivg52 if sex==1, col
*svy: tab bmisrg5 bmivg52 if sex==2, col
*svy: tab bmisrg5 bmivg52 if sex==1, count format(%9.3f)
*svy: tab bmisrg5 bmivg52 if sex==2, count format(%9.3f)

*Compare Full and measured
*svy: tab bmivg52_f bmivg52 if sex==1, col
*svy: tab bmivg52_f bmivg52 if sex==2, col
*svy: tab bmivg52_f bmivg52 if sex==1, count format(%9.3f)
*svy: tab bmivg52_f bmivg52 if sex==2, count format(%9.3f)

*Compare Reduced and measured
*svy: tab bmivg52_r bmivg52 if sex==1, col
*svy: tab bmivg52_r bmivg52 if sex==2, col
*svy: tab bmivg52_r bmivg52 if sex==1, count format(%9.3f)
*svy: tab bmivg52_r bmivg52 if sex==2, count format(%9.3f)

*Compare OHID and measured
*svy: tab bmivg52_OHID bmivg52 if sex==1, col
*svy: tab bmivg52_OHID bmivg52 if sex==2, col
*svy: tab bmivg52_OHID bmivg52 if sex==1, count format(%9.3f)
*svy: tab bmivg52_OHID bmivg52 if sex==2, count format(%9.3f)


******************.
***Table 5
******************.

******************
***SR vs measured
******************

svy:tab overwt_m overwt_sr if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab overwt_m overwt_sr if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_sr if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab overwt_m overwt_sr if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_sr if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

*Adults (Meth Report).

svy:tab overwt_m overwt_sr, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_sr, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

****************************
*Mean-correction vs measured
*****************************

svy:tab overwt_m overwt_mc if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_mc if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab overwt_m overwt_mc if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_mc if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

******************************************
*Equation 1 (only SR as predictor) vs measured.
******************************************

svy:tab overwt_m overwt_eq1 if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_eq1 if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab overwt_m overwt_eq1 if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_eq1 if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])


**********************************
*Equation 2 reduced vs measured.
**********************************

svy:tab overwt_m overwt_r if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_r if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab overwt_m overwt_r if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_r if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

*****************************
*Equation 3: Full vs measured.
*****************************

svy:tab overwt_m overwt_f if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_f if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab overwt_m overwt_f if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_f if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

*Adults (Meth report)

svy:tab overwt_m overwt_f, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_f, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

*************************************
*Equation 4: (full + BMISR) vs measured.
*************************************

svy:tab overwt_m overwt_eq4 if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_eq4 if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab overwt_m overwt_eq4 if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_eq4 if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])


*************************************
*(continuous age) vs measured.
*************************************

svy:tab overwt_m overwt_OHID if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_OHID if sex==1, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab overwt_m overwt_OHID if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_OHID if sex==2, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

* Adults (Meth report)

svy:tab overwt_m overwt_OHID, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

svy:tab obese_m obese_OHID, count format(%11.2f)
mat A = e(b)
di "sensitivity=" A[1,4]/(A[1,3] + A[1,4])
di "specificity=" A[1,1]/(A[1,1] + A[1,2])
di "cc=" (A[1,1] + A[1,4]) / (A[1,1] + A[1,2] + A[1,3] + A[1,4])

***********************************************************************
*Table 4: mean (SD) difference in height and weight (predicted - measured)
*Table 5: mean (SD) difference in BMI (predicted - measured)
***********************************************************************

*Error in height.
generate ht1_error = htsr - htval
generate ht2_error = mc_ht - htval
generate ht3_error = eq1_ht - htval
gen ht4_error = ht_reduced - htval
gen ht5_error = ht_full - htval
generate ht6_error = eq4_ht - htval
gen ht7_error = ht_OHID - htval

foreach var of varlist ht1_error ht2_error ht3_error ht4_error ht5_error ht6_error ht7_error {
svy: mean `var',over(sex) noheader
estat sd
}

*Error in weight.
generate wt1_error = wtsr - wtval2
generate wt2_error = mc_wt - wtval2
generate wt3_error = eq1_wt - wtval2
gen wt4_error = wt_reduced - wtval2
gen wt5_error = wt_full - wtval2
generate wt6_error = eq4_wt - wtval2
gen wt7_error = wt_OHID - wtval2

foreach var of varlist wt1_error wt2_error wt3_error wt4_error wt5_error wt6_error wt7_error {
svy: mean `var',over(sex) noheader
estat sd
}

*Error in BMI.
generate bmi1_error = bmisr - bmival2
generate bmi2_error = mc_bmi - bmival2
generate bmi3_error = eq1_bmi - bmival2
gen bmi4_error = bmi_reduced - bmival2
gen bmi5_error = bmi_full - bmival2
generate bmi6_error = eq4_bmi - bmival2
gen bmi7_error = bmi_OHID - bmival2

foreach var of varlist bmi1_error bmi2_error bmi3_error bmi4_error bmi5_error bmi6_error bmi7_error {
svy: mean `var',over(sex) noheader
estat sd
}


******************************************
*Height (Error and distribution of error)
*LOA (Table 4)
*****************************************

foreach var of varlist ht1_error ht2_error ht3_error ht4_error ht5_error ht6_error ht7_error  {
*Min-Max: percentiles of error (wtd).
*summ `var' if sex==1 [aweight=wt_int]
*di r(min)
*di r(max)
*_pctile `var' if sex==1 [pweight=wt_int], p(5 25 50 75 95) 
*return list
*summ `var' if sex==2 [aweight=wt_int]
*di r(min)
*di r(max)
*_pctile `var' if sex==2 [pweight=wt_int], p(5 25 50 75 95) 
*return list

*LOA (unweighted).
sum `var' if sex==1 
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'
sum `var' if sex==2 
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'
}



******************************************
*Weight (Error and distribution of error)
*LOA (Table 4)
*****************************************

foreach var of varlist wt1_error wt2_error wt3_error wt4_error wt5_error wt6_error wt7_error  {
*Min-Max: percentiles of error (wtd).
*summ `var' if sex==1 [aweight=wt_int]
*di r(min)
*di r(max)
*_pctile `var' if sex==1 [pweight=wt_int], p(5 25 50 75 95) 
*return list
*summ `var' if sex==2 [aweight=wt_int]
*di r(min)
*di r(max)
*_pctile `var' if sex==2 [pweight=wt_int], p(5 25 50 75 95) 
*return list

*LOA (unweighted).
sum `var' if sex==1 
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'
sum `var' if sex==2 
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'
}


******************************************
*BMI (Error and distribution of error)
*LOA (Table 5)
*****************************************

foreach var of varlist bmi1_error bmi2_error bmi3_error bmi4_error bmi5_error bmi6_error bmi7_error  {
*Min-Max: percentiles of error (wtd).
*summ `var' if sex==1 [aweight=wt_int]
*di r(min)
*di r(max)
*_pctile `var' if sex==1 [pweight=wt_int], p(5 25 50 75 95) 
*return list
*summ `var' if sex==2 [aweight=wt_int]
*di r(min)
*di r(max)
*_pctile `var' if sex==2 [pweight=wt_int], p(5 25 50 75 95) 
*return list

*LOA (unweighted).
sum `var' if sex==1 
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'
sum `var' if sex==2 
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'
}

************************************************
*Discrepancy (corrected - measured).
*Table S8: measured BMI as predictor of error.
*************************************************

generate outcome_f1 = bmisr - bmival2
svy,subpop(male): regress outcome_f1 bmival2
svy,subpop(female): regress outcome_f1 bmival2

generate outcome_f2 = bmi_full - bmival2
svy,subpop(male): regress outcome_f2 bmival2
svy,subpop(female): regress outcome_f2 bmival2

generate outcome_f3 = bmi_reduced - bmival2
svy,subpop(male): regress outcome_f3 bmival2
svy,subpop(female): regress outcome_f3 bmival2

*Full model + BMI.
generate outcome_f4 = eq4_bmi - bmival2
svy,subpop(male): regress outcome_f4 bmival2
svy,subpop(female): regress outcome_f4 bmival2



**************************************
*Figure S4.
*BA plot (presented for Full model).
*************************************

sum bmi5_error if sex==1 
di r(sd)
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'
sum bmi5_error if sex==2 
di r(sd)
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
di `mean1' 
di `lowerCL1' 
di `upperCL1'

gen mean_bmi = (bmi_full + bmival2)/2

preserve
keep if sex==1 & mean_bmi>10
sum bmi5_error if sex==1 
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
graph twoway scatter bmi5_error mean_bmi, ///
legend(off) mcolor(black) msymbol(Oh) msize(medsmall)  ///
ytitle("Difference in BMI", m(l=0)) /// 
xtitle("Average BMI") /// 
title("Men", size(medsmall) color(black) pos(11)) /// 
yline(`mean1', lpattern(shortdash) lcolor(gray)) ///
yline(`lowerCL1', lpattern(dash) lcolor(gray)) /// 
yline(`upperCL1', lpattern(dash) lcolor(gray)) /// 
graphregion(color(white)) ylabel(, grid glcolor(gs14)) /// white background
graphr(m(l-100)) graphr(m(r-100)) plotr(m(l+0)) plotr(m(r+0)) ///
ylabel(-20(10)20,ang(hor)) xlabel(10(10)70)  /// 
aspectratio(0.4)    
graph save "Graph" "N:\Temp\Graph1.gph", replace
restore


preserve
keep if sex==2 & mean_bmi>10
sum bmi5_error if sex==2 
local mean1=r(mean) 
local lowerCL1=r(mean) - 2*r(sd) 
local upperCL1=r(mean) + 2*r(sd)
graph twoway scatter bmi5_error mean_bmi, ///
legend(off) mcolor(black) msymbol(Oh) msize(medsmall) ///
ytitle("Difference in BMI", m(l=0)) /// 
xtitle("Average BMI") /// 
title("Women", size(medsmall) color(black) pos(11)) /// 
yline(`mean1', lpattern(shortdash) lcolor(gray)) ///
yline(`lowerCL1', lpattern(dash) lcolor(gray)) /// 
yline(`upperCL1', lpattern(dash) lcolor(gray)) /// 
graphregion(color(white)) ylabel(, grid glcolor(gs14)) /// white background
graphr(m(l-100)) graphr(m(r-100)) plotr(m(l+0)) plotr(m(r+0)) ///
ylabel(-20(10)20,ang(hor)) xlabel(10(10)70)  /// 
aspectratio(0.4)    
graph save "Graph" "N:\Temp\Graph2.gph", replace
restore

graph combine "N:\Temp\Graph1.gph" "N:\Temp\Graph2.gph", rows(2)  imargin(0 0 0 0) ///
plotregion(margin(l=20 r=20)) graphregion(margin(l=-40 r=-40)) iscale(0.8) graphregion(color(white))

graph save "Graph" "N:\Temp\S10.gph", replace
graph use "N:\Temp\S10.gph"
graph export "N:\Temp\S4.tif", replace


*************************
*Estimates for figure 1
*************************

*SR.
drop obese_diff 
gen obese_diff = (obese_sr - obese_m)
svy:mean obese_diff,over(sex)
svy,subpop(male):mean obese_diff,over(age16g5)
svy,subpop(male):mean obese_diff,over(gor)
svy,subpop(male):mean obese_diff,over(topqual)
svy,subpop(male):mean obese_diff,over(origin2)
svy,subpop(male):mean obese_diff,over(cigsta3)
svy,subpop(male):mean obese_diff,over(genhelf)

svy,subpop(female):mean obese_diff,over(age16g5)
svy,subpop(female):mean obese_diff,over(gor)
svy,subpop(female):mean obese_diff,over(topqual)
svy,subpop(female):mean obese_diff,over(origin2)
svy,subpop(female):mean obese_diff,over(genhelf)
svy,subpop(female):mean obese_diff,over(cigsta3)
drop obese_diff

*Full.
gen obese_diff = (obese_f - obese_m)
svy:mean obese_diff,over(sex)
svy,subpop(male):mean obese_diff,over(age16g5)
svy,subpop(male):mean obese_diff,over(gor)
svy,subpop(male):mean obese_diff,over(topqual)
svy,subpop(male):mean obese_diff,over(origin2)
svy,subpop(male):mean obese_diff,over(cigsta3)
svy,subpop(male):mean obese_diff,over(genhelf)

svy,subpop(female):mean obese_diff,over(age16g5)    /* perfect agreement at young age */
svy,subpop(female):mean obese_diff,over(gor)
svy,subpop(female):mean obese_diff,over(topqual)
svy,subpop(female):mean obese_diff,over(origin2)
svy,subpop(female):mean obese_diff,over(genhelf)
svy,subpop(female):mean obese_diff,over(cigsta3)
drop obese_diff

**********************
**Figure 1 (men)
***********************

clear
input id	SR_EST	SR_LL	SR_UL		Full_EST	Full_LL	Full_UL
1	-0.065	-0.073	-0.057		-0.008	-0.015	-0.001
2	-0.034	-0.076	0.009		-0.025	-0.065	0.014
3	-0.030	-0.066	0.005		-0.001	-0.019	0.016
4	-0.027	-0.047	-0.007		0.000	-0.022	0.022
5	-0.032	-0.054	-0.009		-0.010	-0.030	0.011
6	-0.025	-0.051	0.002		-0.009	-0.036	0.017
7	-0.047	-0.071	-0.022		-0.021	-0.044	0.002
8	-0.041	-0.065	-0.017		0.009	-0.013	0.031
9	-0.088	-0.123	-0.053		-0.013	-0.045	0.019
10	-0.082	-0.112	-0.051		-0.020	-0.050	0.010
11	-0.075	-0.103	-0.047		0.009	-0.018	0.036
12	-0.085	-0.113	-0.057		0.005	-0.021	0.030
13	-0.105	-0.135	-0.076		-0.014	-0.036	0.008
14	-0.127	-0.165	-0.088		-0.021	-0.051	0.009
15	-0.111	-0.148	-0.074		-0.006	-0.041	0.028
16	-0.102	-0.151	-0.052		-0.005	-0.049	0.038
17	-0.167	-0.245	-0.088		-0.030	-0.096	0.036
18	-0.089	-0.120	-0.058		-0.022	-0.050	0.006
19	-0.064	-0.085	-0.044		0.008	-0.012	0.027
20	-0.043	-0.068	-0.018		0.004	-0.019	0.027
21	-0.059	-0.084	-0.034		0.002	-0.022	0.025
22	-0.077	-0.104	-0.050		0.001	-0.023	0.025
23	-0.073	-0.093	-0.053		-0.020	-0.040	0.000
24	-0.031	-0.048	-0.015		-0.008	-0.024	0.008
25	-0.072	-0.093	-0.052		-0.022	-0.040	-0.005
26	-0.090	-0.120	-0.060		-0.013	-0.041	0.015
27	-0.052	-0.066	-0.038		-0.003	-0.016	0.010
28	-0.065	-0.080	-0.051		-0.010	-0.023	0.002
29	-0.071	-0.088	-0.054		-0.024	-0.040	-0.007
30	-0.076	-0.094	-0.057		0.009	-0.008	0.026
31	-0.070	-0.079	-0.062		-0.009	-0.017	-0.001
32	-0.008	-0.056	0.040		0.013	-0.033	0.059
33	-0.021	-0.040	-0.003		0.006	-0.014	0.027
34	-0.046	-0.091	-0.001		-0.015	-0.057	0.027
35	-0.056	-0.065	-0.048		-0.007	-0.015	0.001
36	-0.097	-0.120	-0.074		-0.012	-0.034	0.011
37	-0.093	-0.127	-0.060		-0.012	-0.043	0.019
38	-0.041	-0.058	-0.024		-0.015	-0.031	0.001
39	-0.089	-0.105	-0.074		-0.010	-0.024	0.004
40	-0.062	-0.073	-0.052		-0.004	-0.014	0.006
end
renvars, lower

foreach var of varlist sr_est sr_ll sr_ul full_est full_ll full_ul {
	replace `var'=`var'*100
}

mkmat sr_est, matrix(A)
mkmat sr_ll, matrix(B)
mkmat sr_ul, matrix(C)
mkmat full_est, matrix(A1)
mkmat full_ll, matrix(B1)
mkmat full_ul, matrix(C1)

matrix coln A = AME
matrix rown A = "All" "16-17y" "18-19y" "20-24y" "25-29y" "30-34y" "35-39y" "40-44y" "45-49y" "50-54y" "55-59y" "60-64y" ///
"65-69y" "70-74y" "75-79y" "80-84y" "85y+" "North East" "North West" "Yorkshire & The Humber" "East Midlands" "West Midlands" "East of England" ///
"London" "South East" "South West" ///
"Degree" "Below degree" "O level" "None" "White" "Black" ///
"Asian" "Mixed" "Very good/good health" "Fair" "Bad/very bad health" "Current smoker" "Ex-regular" "Never smoker"

matrix coln A1 = AME
matrix rown A1 = "All" "16-17y" "18-19y" "20-24y" "25-29y" "30-34y" "35-39y" "40-44y" "45-49y" "50-54y" "55-59y" "60-64y" ///
"65-69y" "70-74y" "75-79y" "80-84y" "85y+" "North East" "North West" "Yorkshire & The Humber" "East Midlands" "West Midlands" "East of England" ///
"London" "South East" "South West" ///
"Degree" "Below degree" "O level" "None" "White" "Black" ///
"Asian" "Mixed" "Very good/good health" "Fair" "Bad/very bad health" "Current smoker" "Ex-regular" "Never smoker"

* GraphEditor: text small; triangle for corrected; white background
coefplot (matrix(A[,1]), ci((B[,1] C[,1]))) ///
(matrix(A1[,1]), ci((B1[,1] C1[,1]))), xline(0,lcolor(black)) ///
title("",size(medium)) note("") ///
legend(order(2 "Self-reported" 4 "Corrected") cols(1) pos(11) ring(0)) xlabel(-25(5)10) ///
xtitle("{bf}Difference to measured obesity (95% CI)",size(small)) 

graph save "Graph" "N:\Temp\F1a.gph", replace


clear

**********************
**Figure 1 (women)
***********************

input id	SR_EST	SR_LL	SR_UL		Full_EST	Full_LL	Full_UL
1	-0.052	-0.059	-0.046		-0.008	-0.014	-0.002
2	-0.013	-0.039	0.013		0.000	0.000	0.000
3	-0.046	-0.085	-0.006		0.001	-0.034	0.036
4	-0.016	-0.030	-0.001		-0.001	-0.015	0.014
5	-0.009	-0.025	0.007		0.006	-0.008	0.021
6	-0.042	-0.064	-0.019		-0.018	-0.039	0.004
7	-0.043	-0.062	-0.024		-0.007	-0.027	0.014
8	-0.046	-0.066	-0.026		-0.011	-0.027	0.005
9	-0.054	-0.075	-0.032		-0.028	-0.048	-0.008
10	-0.055	-0.078	-0.031		-0.004	-0.027	0.019
11	-0.083	-0.111	-0.055		-0.014	-0.034	0.006
12	-0.061	-0.089	-0.034		-0.004	-0.029	0.021
13	-0.093	-0.120	-0.065		-0.031	-0.054	-0.007
14	-0.096	-0.128	-0.064		-0.004	-0.035	0.026
15	-0.061	-0.094	-0.029		0.005	-0.029	0.039
16	-0.086	-0.131	-0.042		0.029	-0.008	0.066
17	-0.067	-0.114	-0.019		-0.006	-0.063	0.051
18	-0.068	-0.092	-0.044		-0.007	-0.027	0.014
19	-0.053	-0.070	-0.036		-0.010	-0.026	0.005
20	-0.049	-0.069	-0.028		-0.005	-0.023	0.013
21	-0.053	-0.076	-0.029		-0.013	-0.031	0.006
22	-0.056	-0.078	-0.034		-0.006	-0.026	0.014
23	-0.049	-0.067	-0.031		-0.009	-0.028	0.009
24	-0.057	-0.075	-0.039		-0.010	-0.026	0.005
25	-0.047	-0.060	-0.033		-0.006	-0.020	0.008
26	-0.050	-0.069	-0.031		-0.006	-0.023	0.010
27	-0.044	-0.055	-0.033		-0.010	-0.020	0.000
28	-0.047	-0.059	-0.034		-0.008	-0.019	0.003
29	-0.047	-0.059	-0.035		-0.007	-0.018	0.004
30	-0.080	-0.096	-0.063		-0.008	-0.024	0.008
31	-0.050	-0.057	-0.044		-0.007	-0.013	-0.001
32	-0.118	-0.175	-0.061		0.002	-0.045	0.050
33	-0.076	-0.104	-0.048		-0.032	-0.058	-0.007
34	0.003	-0.032	0.037		0.014	-0.013	0.041
35	-0.048	-0.055	-0.041		-0.007	-0.014	-0.001
36	-0.062	-0.080	-0.044		-0.002	-0.019	0.015
37	-0.081	-0.110	-0.052		-0.035	-0.060	-0.010
38	-0.043	-0.059	-0.027		-0.010	-0.025	0.005
39	-0.063	-0.077	-0.049		-0.008	-0.021	0.005
40	-0.052	-0.060	-0.044		-0.008	-0.015	-0.001
end
renvars, lower

foreach var of varlist sr_est sr_ll sr_ul full_est full_ll full_ul {
	replace `var'=`var'*100
}

mkmat sr_est, matrix(A)
mkmat sr_ll, matrix(B)
mkmat sr_ul, matrix(C)
mkmat full_est, matrix(A1)
mkmat full_ll, matrix(B1)
mkmat full_ul, matrix(C1)

matrix coln A = AME
matrix rown A = "All" "16-17y" "18-19y" "20-24y" "25-29y" "30-34y" "35-39y" "40-44y" "45-49y" "50-54y" "55-59y" "60-64y" ///
"65-69y" "70-74y" "75-79y" "80-84y" "85y+" "North East" "North West" "Yorkshire & The Humber" "East Midlands" "West Midlands" "East of England" ///
"London" "South East" "South West" ///
"Degree" "Below degree" "O level" "None" "White" "Black" ///
"Asian" "Mixed" "Very good/good health" "Fair" "Bad/very bad health" "Current smoker" "Ex-regular" "Never smoker"

matrix coln A1 = AME
matrix rown A1 = "All" "16-17y" "18-19y" "20-24y" "25-29y" "30-34y" "35-39y" "40-44y" "45-49y" "50-54y" "55-59y" "60-64y" ///
"65-69y" "70-74y" "75-79y" "80-84y" "85y+" "North East" "North West" "Yorkshire & The Humber" "East Midlands" "West Midlands" "East of England" ///
"London" "South East" "South West" ///
"Degree" "Below degree" "O level" "None" "White" "Black" ///
"Asian" "Mixed" "Very good/good health" "Fair" "Bad/very bad health" "Current smoker" "Ex-regular" "Never smoker"

*Different symbol (can do in GraphEditor).
* GraphEditor: text small; triangle for corrected; white background
coefplot (matrix(A[,1]), ci((B[,1] C[,1]))) ///
(matrix(A1[,1]), ci((B1[,1] C1[,1]))), xline(0,lcolor(black)) ///
title("",size(medium)) note("") ///
legend(order(2 "Self-reported" 4 "Corrected") cols(1) pos(11) ring(0)) xlabel(-25(5)10) ///
xtitle("{bf}Difference to measured obesity (95% CI)",size(small)) 
graph save "Graph" "N:\Temp\F1b.gph", replace

graph combine "N:\Temp\F1a.gph" "N:\Temp\F1b.gph" 
graph save "Graph" "N:\Temp\F1.gph", replace

graph use "N:\Temp\F1.gph"
graph export "N:\Temp\F1.tif", width(1000) height(800) replace


********************************************************
*Correction factor for an all-mean/reference individual.
********************************************************

clear
use "S:\FPHS_EPH_HSE_Shared\2 HSE 2016-19\HSE 2020\Report\Predicting Ht & Wt\Paper\GitHub\HSR 2011-16 Height and Weight_withSPLIT.dta", clear

drop if wtok==5
drop htsr_sq

*trim age at 90 (n=115).
replace age=90 if age>90
gen agesq=(age^2)
gen agecub=(age^3)

*Reference values (age 45-49: age16g5==8)
summ htsr if sex==1
summ htsr if sex==2
summ wtsr if sex==1
summ wtsr if sex==2

*180cm(men); 160cm(women)
*80kg(men); 70kg(women)

replace htsr = (htsr-180) if sex==1
replace htsr = (htsr-160) if sex==2
replace wtsr = (wtsr-80) if sex==1
replace wtsr = (wtsr-70) if sex==2

gen htsr2 = htsr*htsr
gen wtsr2 = wtsr*wtsr
gen wtsr3 = wtsr*wtsr*wtsr

*Correction factor given by the constant * -1 (presented by the full model).

preserve
keep if split==0
svy,subpop(male):regress ht_diff htsr htsr2 i.age16g5 ib8.gor i.topqual i.origin2 i.genhelf
margins,at(htsr=0 htsr2=0 age16g5=1 gor=8 topqual=1 origin2=1 genhelf=1)

svy,subpop(female):regress ht_diff htsr i.age16g5 ib8.gor i.topqual i.qimd i.origin2
margins,at(htsr=0 age16g5=1 gor=8 topqual=1 qimd=1 origin2=1) 

svy,subpop(male):regress wt_diff wtsr wtsr2 wtsr3 i.age16g5 i.cigsta3 i.topqual 
margins,at(wtsr=0 wtsr2=0 wtsr3=0 age16g5=1 cigsta3=1 topqual=1) 

svy,subpop(female):regress wt_diff wtsr wtsr2 wtsr3 i.age16g5 i.cigsta3 i.origin2 
margins,at(wtsr=0 wtsr2=0 wtsr3=0 age16g5=1 cigsta3=1 origin2=1) 
restore






















































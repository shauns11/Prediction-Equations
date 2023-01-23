* Encoding: UTF-8.
*******************************.
*GitHub
*Dataset for prediction equations.
********************************.

dataset close all.
GET FILE='S:\FPHS_EPH_HSE_Shared\2 HSE 2016-19\HSE 2020\Data\HSE_Trends_2011_2019.sav'
/keep tserial hseyr wt_int sex age ag16g10
Height Weight EHtCh EHtm EHtFt EHtIn EWtCh
EWtkg EWtSt EWtL HTSR WTSR
htok wtok bmiok htval wtval wtval2
bmi bmival bmival2 bmivg5
bmivg52 BMIvg53 bmi_group bmivg3 BMIOwgt
bmisr bmisrg5 gor1 cpsu Origin origin2 GenHelf2 topqual3 ill12m cigsta3 qimd.

select if age>=16 & (hseyr=2011|hseyr=2012|hseyr=2013|hseyr=2014|hseyr=2015|hseyr=2016).
EXECUTE.
missing values all ().
exe.
*N=49,817.

compute pregnant=0.
if wtok=-90 pregnant=1.
fre pregnant.
select if pregnant=0.
fre pregnant.

*Leaves N=49,346.
 
*====================.
*Analysis of response.
*=====================.

*2 cases with wtsr=0 (1 is missing measured height).

compute Response=-2.
if ((wtsr<0)|(htsr<0)) & ((wtval2<0)|(htval<0)) Response=1.
if ((wtsr>0) & (htsr>0)) & ((wtval2<0)|(htval<0)) Response=2.
if ((wtsr<0)|(htsr<0)) & ((wtval2>0) & (htval>0)) Response=3.
if ((wtsr>0) & (htsr>0)) & ((wtval2>0) & (htval>0)) Response=4.
val labels response
1 "Neither"
2 "SR but not M"
3 "M but not SR"
4 "Both".
fre response.

TEMPORARY.
sel if response=-2.
list tserial hseyr wtsr htsr wtval2 htval.

*consider wtsr = 0 as missing.
*1 person only lacks SR (assign to 3).
*1 person lacks SR and M (assign to 1).

if (tserial=100045347 & wtsr=0) response=3.
if tserial=100051615 & wtsr=0 response=1.
fre response.

*==================.
*Analysis of Bias.
*===================.

INSERT FILE = "S:\FPHS_EPH_HSE_Shared\2 HSE 2016-19\HSE 2021\Report\Physical Activity\Syntax\SPSS crosstabs macro.sps".


NCTABLES
/row = bmisrg5
/column = response
/rownest=sex
/weight=wt_int
/strata=gor1
/psu=cpsu
/sigtest=off
/rowcat = 1 thru 5
/stratatest = off.

*3-category variables.

Numeric BMIm3 (F3).
if bmivg52<1 BMIm3=-1.
if range(bmivg52,1,2) BMIm3=1.
if bmivg52=3 BMIm3=2.
if range(bmivg52,4,5) BMIm3=3.
value labels BMIm3
1 "Not overweight"
2 "Overweight"
3 "Obese".
cro/tables = bmivg52 by BMIm3.

Numeric BMIsr3 (F3).
if bmisrg5<1 BMIsr3=-1.
if range(bmisrg5,1,2) BMIsr3=1.
if bmisrg5=3 BMIsr3=2.
if range(bmisrg5,4,5) BMIsr3=3.
value labels BMIsr3
1 "Not overweight"
2 "Overweight"
3 "Obese".
cro/tables = bmisrg5 by BMIsr3.

select if (bmival2>0) & (HTSR>0) & (WTSR>0).
fre hseyr.

*========================================.
*Difference (self-reported - measured).
*=======================================.

compute wt_diff = (wtsr - wtval2).
compute ht_diff = (htsr - htval).
compute bmi_diff = (bmisr - bmival2).
variable label wt_diff "Reported - measured weight (kg)".
variable label ht_diff "Reported - measured height (cm)".
variable label bmi_diff "Reported - measured BMI".
desc var wt_diff ht_diff bmi_diff.


SAVE TRANSLATE OUTFILE= 'S:\FPHS_EPH_HSE_Shared\2 HSE 2016-19\HSE 2020\Report\Predicting Ht & Wt\Paper\GitHub\HSE 2011-16 Height and Weight.dta'
  /TYPE=STATA
  /VERSION=14
  /EDITION=SE
  /MAP
  /REPLACE.

  









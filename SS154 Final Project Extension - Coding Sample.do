***DiD Analyses***
*******Replication*******

reg LGexp_per_capita_z R2 R3 t_inel DD36_ti DD84_ti $X if (arm==2 | arm==4) [aw=ipw_84], cluster(clid) 

gen exp_education_log = log(exp_education)
reg exp_education_log R2 R3 t_inel DD36_ti DD84_ti $X if (arm==2 | arm==4) [aw=ipw_84], cluster(clid) 


*******Extension*******

use "/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/data/individual_longitudinal_bl_36_84_v1.dta", clear


gen DD36_ti = R3*t_inel
gen DD84_ti = R5*t_inel

*rec_attended_school_b whether recipient ever attended school
*clean_dwater_b whether they drink clean water
*s3q111 is the amount of minutes it takes to get to school
*shoes whether the child has shoes
global on_educ "rec_attended_school_b clean_dwater_b s3q11 shoes ssfee_b"

logit secondary R3 R5 t_inel DD36_ti DD84_ti $on_educ if (arm==2 | arm==4), cluster(clid) 

predict logitpred, pr
predict logitlatent, xb
margins dydx(*)


gen secondary_1 = secondary if (arm==2 | arm==4)

*R3
diff secondary_1, t(t_inel) p(R3) cov(rec_attended_school_b clean_dwater_b s3q11 shoes ssfee_b)

*R5
diff secondary_1, t(t_inel) p(R5) cov(rec_attended_school_b clean_dwater_b s3q11 shoes ssfee_b)



*Graph 3 - Distance to Schools

hist dist_low_school, t1("Distance to Low School") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/low.gph", replace) 
hist dist_middle_school, t1("Distance to Middle School") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/middle.gph", replace) 
hist dist_upper_school, t1("Distance to Upper School") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/upper.gph", replace) 
hist dist_high_school, t1("Distance to High School") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/high.gph", replace) 

graph combine "$outreg/low.gph"  "$outreg/middle.gph"  "$outreg/upper.gph" "$outreg/high.gph", t1("Distance to Schools") saving("$outreg/Distance to School.gph", replace)


*Graph 1 - Replicate Figure 3

*Open data
use "/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/data/household_longitudinal_bl_36_84_v4.dta", clear

************balanced panel************
keep if balpanel_bl3684==1

**********************************
*MASSIVE OUTLIER ON EXP_TOTAL_AP IN TIME==6 AND QSN==719
*SET TO THE MAN OF 58
replace exp_total_ap=58 if exp_total_ap>3800 & time==6
**********************************

**********code exposure variable and create graph********
capture drop exposure
gen exposure=G3-18628
sum exposure if time==6, det
replace exposure=609 if exposure<=609 & treat==1
replace exposure=2343 if exposure>2343 & treat==1 & exposure~=.
sum exposure if time==6, det
*convert to months
replace exposure = exposure/30.5
sum exposure if time==6, det
lab var exposure "exposure since Jan 1 2011"
*******************************************
*Figure 3.3: Exposure to the CGP in Months*
*******************************************
hist exposure if time==6, xlab(18(3)75)
graph save "$outreg/Figure 3", replace

*Graph 2 - Replicate Figure 5

*Open data
use "/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/data/household_longitudinal_bl_36_84_v4.dta", clear


*Keep only if balanced panel
keep if balpanel_bl3684==1

**********************************
*MASSIVE OUTLIER ON EXP_TOTAL_AP IN TIME==6 AND QSN==719
*SET TO THE MAN OF 58
replace exp_total_ap=58 if exp_total_ap>3800 & time==6
**********************************
gen anysales=vsls>0 if vsls~=.
gen anylvstkinc=income_livestock>0 if income_livestock~=.
*also use lndop

****type mismatch issue so create value labels****
label drop time
label define time 1 "'10" 4 "'13" 6 "'17"
label values time time
label define treat 0 "Original C" 1 "Original T"
label values treat treat


****ineligibles*****
preserve
keep if arm==2 | arm==4


***first generate individual figures, save them somewhere so rename paths in the 'saving' option***
graph bar exp_per_capita, over(time) over(treat) bar(1, color(black)) t1("Consumption PC") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in1.gph", replace) 
graph bar foodexp_per, over(time) over(treat) bar(1, color(black)) t1("food consumption pc") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in2.gph", replace)
graph bar hfiasP, over(time) over(treat) bar(1, color(black)) t1("food security") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in3.gph", replace)
graph bar s10q6_NR, over(time) over(treat) bar(1, color(black)) t1("not worried about food") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in4.gph", replace)
graph bar asst_idx, over(time) over(treat) bar(1, color(black)) t1("asset index") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in5.gph", replace)
graph bar lvstk_idx, over(time) over(treat) bar(1, color(black)) t1("livestock index") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in6.gph", replace)
graph bar prod_idx, over(time) over(treat) bar(1, color(black)) t1("production index") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in7.gph", replace)
graph bar any_savings, over(time) over(treat) bar(1, color(black)) t1("any savings") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in8.gph", replace)
graph bar hrv_value, over(time) over(treat) bar(1, color(black)) t1("value of harvest") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in9.gph", replace)
graph bar exp_total_ap, over(time) over(treat) bar(1, color(black)) t1("expenditure agric inputs") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in10.gph", replace)
graph bar fut_exp, over(time) over(treat) bar(1, color(black)) t1("life better in future") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in11.gph", replace)
graph bar better_off, over(time) over(treat) bar(1, color(black)) t1("better off than 1 year ago") saving("/Users/ericagonzales/Documents/SS154 Final Project/CCT in Zambia Replication copy/outreg/in12.gph", replace)

****now combine the graphs into one figure and save, so change pathname here*******
*#delimit ;
graph combine "$outreg/in1.gph"  "$outreg/in2.gph"  "$outreg/in3.gph" "$outreg/in4.gph" "$outreg/in5.gph" "$outreg/in6.gph" "$outreg/in7.gph" "$outreg/in8.gph" "$outreg/in9.gph" "$outreg/in10.gph" "$outreg/in11.gph" "$outreg/in12.gph" 
	, t1("SCT Ineligible") saving("$outreg/Figure 5.gph", replace);
*#delimit cr
*restore
********************

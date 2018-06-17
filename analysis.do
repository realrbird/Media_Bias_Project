cd "/Users/robert/Google Drive/Projects/Dissertation/media_bias_project"
use final_data_for_analysis.dta,clear


*******************************************************************************
* Table_5 
*******************************************************************************

* clustered standard errors
reg republican_percentage ib5.network i.time_slot i.day_of_week ///
israel_palestine-orlando_shooting, cluster(date)
estimates store cse_model


* clustered standard errors with fixed effects by day
xtset date
xtreg republican_percentage ib5.network i.time_slot, cluster(date) fe
estimates store cse_fe_model
xtset, clear


*******************************************************************************
* Figure_2 Data
*******************************************************************************

quietly estimates restore cse_model
pwcompare network, pveffects ci
quietly estimates restore cse_fe_model
pwcompare network, pveffects ci











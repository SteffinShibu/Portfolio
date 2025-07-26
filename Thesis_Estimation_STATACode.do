clear 

cd "/Users/steffinshibu/Desktop/Thesis Work/InvMigUndUncert/Data"

************************** WINDOW PPML ESTIMATION ******************************
use "Window_Panel.dta", clear

gen rwui = ln(wui_shift_des)/ln(wui_shift_ori)
gen rcitation = ln(citation_shift_des)/ln(citation_shift_ori)
gen rcollaboration = ln(collaboration_shift_des)/ln(collaboration_shift_ori)
gen routput = ln(output_shift_des)/ln(output_shift_ori)
gen rscopus = ln(scopus_shift_des)/ln(scopus_shift_ori)
gen ropenalex = ln(openalex_shift_des)/ln(openalex_shift_ori)
gen rorcid = ln(orcid_shift_des)/ln(openalex_shift_ori)
gen rgdppc = ln(gdppc_shift_des)/ln(gdppc_shift_ori)
gen rgdppcus = ln(gdppcus_shift_des)/ln(gdppcus_shift_ori)
gen rpop = ln(pop_shift_des)/ln(pop_shift_ori)

label variable flow "Bilateral Inventor Migration Flow"
label variable rwui "Relative Log Uncertainty"
label variable rcitation "Relative Log Citation"
label variable rcollaboration "Relative Log Int. Collaboration"
label variable routput "Relative Log Output"
label variable rscopus "Relative Log Num of Inventors"
label variable ropenalex "Relative Log Num of Inventors OpenAlex"
label variable rorcid "Relative Log Num of Inventors Orcid"
label variable rgdppc "Relative Log GDP Per Capita PPP"
label variable rgdppcus "Relative Log GDP Per Capita US$"
label variable rpop "Relative Log Population"

egen dyad = group(iso2_ori iso2_des)
egen des_time_fe = group(iso2_des window_label)
egen ori_time_fe = group(iso2_ori window_label)
encode window_label, gen(window_label_num)
xtset dyad window_label_num

correlate flow rwui rcitation rcollaboration routput rscopus rgdppc rpop
asdoc correlate flow rwui rcitation rcollaboration routput rscopus rgdppc rpop, replace label ctitle (Correlation Matrix)


summarize flow rwui rcitation rcollaboration routput rscopus rgdppc rpop

asdoc summarize flow rwui rcitation rcollaboration routput rscopus rgdppc rpop, replace label ctitle (Descriptive Statistics)

*------------------------------------------------------------------------------*
*                        CHECKING WHY GDPPC DROPPING                           *
*------------------------------------------------------------------------------*
ppmlhdfe flow rwui rgdppc rpop, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)

reghdfe flow rwui rgdppc rgdppcus rpop, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)

reghdfe flow rwui rcollaboration rcitation rscopus routput, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)

reghdfe rgdppc, absorb(ori_time_fe des_time_fe) ///
        residuals(e_rgdppc)
summarize e_rgdppc

reghdfe rgdppcus, absorb(ori_time_fe des_time_fe) ///
        residuals(e_rgdppcus)
summarize e_rgdppcus

reghdfe rpop, absorb(ori_time_fe des_time_fe) ///
        residuals(e_rpop)
summarize e_rpop

reghdfe rwui, absorb(ori_time_fe des_time_fe) ///
	residuals(e_rwui)
summarize e_rwui

reghdfe rcollaboration, absorb(ori_time_fe des_time_fe) ///
        residuals(e_rcollaboration)
summarize e_rcollaboration

reghdfe routput, absorb(ori_time_fe des_time_fe) ///
        residuals(e_routput)
summarize e_routput

reghdfe rcitation, absorb(ori_time_fe des_time_fe) ///
        residuals(e_rcitation)
summarize e_rcitation

reghdfe rscopus, absorb(ori_time_fe des_time_fe) ///
        residuals(e_rscopus)
summarize e_rscopus

*----------------------------- FE SLOWLY INTRODUCED ---------------------------*
ppmlhdfe flow rwui, vce(cluster dyad)
outreg2 using "PPML_FE_Introduced.doc", replace label ctitle (No FE) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin-Destination FE", "No", "Origin-Time FE", "No", "Destination-Time FE", "No")

ppmlhdfe flow rwui, absorb(dyad) vce(cluster dyad)
outreg2 using "PPML_FE_Introduced.doc", append label ctitle (Single FE) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin-Destination FE", "Yes", "Origin-Time FE", "No", "Destination-Time FE", "No")

ppmlhdfe flow rwui, absorb(dyad ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_FE_Introduced.doc", append label ctitle (Two-way FE) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin-Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "No")

ppmlhdfe flow rwui, absorb(dyad ori_time_fe des_time_fe) vce(cluster dyad)
outreg2 using "PPML_FE_Introduced.doc", append label ctitle (Three-way FE) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin-Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")
*------------------------------- SCIENTIFIC CONTROLS --------------------------*
ppmlhdfe flow rwui, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_SControls.doc", replace label ctitle (No Controls) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")

ppmlhdfe flow rwui rcollaboration, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_SControls.doc", append label ctitle (Network Effect) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")

ppmlhdfe flow rwui routput, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_SControls.doc", append label ctitle (Ecosystem Strength) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")

ppmlhdfe flow rwui rscopus, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_SControls.doc", append label ctitle (Potential Movers) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")

ppmlhdfe flow rwui rcitation, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_SControls.doc", append label ctitle (Ecosystem Quality) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")

ppmlhdfe flow rwui rcollaboration routput rscopus rcitation, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_SControls.doc", append label ctitle (All) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")
*------------------------------- ALL CONTROLS ---------------------------------*
ppmlhdfe flow rwui rcollaboration routput rscopus rcitation, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_AllControls.doc", replace label ctitle (PPML) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Three-way FE", "Yes")

reghdfe flow rwui rcollaboration routput rscopus rcitation rgdppc rpop, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_AllControls.doc", append label ctitle (OLS) ///
addstat(Adjusted R^2, e(r2_a)) ///
addtext("Three-way FE", "Yes")
************************** MA PPML ESTIMATION **********************************
use "MA_Panel.dta", clear

gen rwui = ln(wui_ma5_des)/ln(wui_ma5_ori)
gen rcitation = ln(citation_ma5_des)/ln(citation_ma5_ori)
gen rcollaboration = ln(collaboration_ma5_des)/ln(collaboration_ma5_ori)
gen routput = ln(output_ma5_des)/ln(output_ma5_ori)
gen rscopus = ln(scopus_ma5_des)/ln(scopus_ma5_ori)
gen ropenalex = ln(openalex_ma5_des)/ln(openalex_ma5_ori)
gen rorcid = ln(orcid_ma5_des)/ln(orcid_ma5_ori)
gen rgdppc = ln(gdppc_ma5_des)/ln(gdppc_ma5_ori)
gen rpop = ln(pop_ma5_des)/ln(pop_ma5_ori)

label variable rwui "Relative Log Uncertainty"
label variable rcitation "Relative Log Citation"
label variable rcollaboration "Relative Log Int. Collaboration"
label variable routput "Relative Log Output"
label variable rscopus "Relative Log Num of Inventors"
label variable ropenalex "Relative Log Num of Inventors OpenAlex"
label variable rorcid "Relative Log Num of Inventors Orcid"
label variable rgdppc "Relative Log GDP Per Capita"


egen dyad = group(iso2_ori iso2_des)
egen des_time_fe = group(iso2_des year)
egen ori_time_fe = group(iso2_ori year)
xtset dyad year

correlate flow rwui rcitation rcollaboration routput rscopus rgdppc rpop

summarize rwui

ppmlhdfe flow rwui, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_MA.doc", replace label ctitle (No Controls) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")

ppmlhdfe flow rwui rcollaboration, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_MA.doc", append label ctitle (Network Effect) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")

ppmlhdfe flow rwui routput, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_MA.doc", append label ctitle (Ecosystem Strength) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")

ppmlhdfe flow rwui rscopus, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_MA.doc", append label ctitle (Potential Movers) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")

ppmlhdfe flow rwui rcitation, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_MA.doc", append label ctitle (Ecosystem Quality) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")

ppmlhdfe flow rwui rcollaboration routput rscopus rcitation rgdppc rpop, absorb(dyad des_time_fe ori_time_fe) vce(cluster dyad)
outreg2 using "PPML_Results_MA.doc", append label ctitle (All Controls) ///
addstat(Pseudo-R^2, e(r2_p)) ///
addtext("Origin_Destination FE", "Yes", "Origin-Time FE", "Yes", "Destination-Time FE", "Yes")



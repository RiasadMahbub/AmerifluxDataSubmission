List the items and list the names where they will be changed using `==`.
Filter the items using `select` function
Change the name using `rename` function

## PracticeScripts.R code
Change the column names and save them in the directory 
Some specific columns 
Check the lag and correlation with potential data

## Soil heat flux calculation
Columns

TS_mean.1.  = Tsoil_wat_mean(1)
TS_mean.2.  = Tsoil_wat_mean(2)
TS_mean.3.  = Tsoil_wat_mean(3)
TS_mean.4. = Tw

del_TS.1. = del_Tsoil_wat(1)
del_TS.2.= del_Tsoil_wat(2)
del_TS.3.= del_Tsoil_wat(3)
del_TS.4.= deltw

shf_Avg.1. == shf_Avg(1)
shf_Avg.2. == shf_Avg(2)
shf_Avg.3. == shf_Avg(3)

panel_tmpr_Avg
BattV_Avg

SWC_1_1_1_Avg == vwc
Watertable == Lvl_m_Avg



Get the non_NA dataframes for soil
Print which are non NA columns and which columns have data
Print out the columns that start with T
Filter out shf_Avg.1., shf_Avg.2., shf_Avg.3. greater than 80 and lesser than -60
Filter out Lvl_m_Avg lesser than -0.5

columns <- c("del_TS.1.", "del_TS.2.", "del_TS.3.", "del_TS.4.", "shf_Avg.1.", "shf_Avg.2.", "shf_Avg.3.")
Filter out the values these columns by three standard deviation from the mean 

calculate swcorr
calculate G1, G2, and G3

tail(sort(way3_data_non_na[[1]]$Lvl_m_Avg, decreasing = TRUE), 2300)


## Way 4
calculate G flux1, G flux2 from these columns
fw_3_Avg	fw05_Avg	Gcal	VWC3_Avg	Ts3_Avg	del_Ts3	BRP3_Avg	EC3_Avg	VWC6_Avg	Ts6_Avg	del_Ts6	BRP6_Avg	EC6_Avg	Tsoil_wat_mean(1)	Tsoil_wat_mean(2)	Tsoil_wat_mean(3)	Tsoil_wat_mean(4)	del_Tsoil_wat(1)	del_T1_filt	del_Tsoil_wat(2)	del_T2_filt	del_Tsoil_wat(3)	detT3_filt	del_Tsoil_wat(4)	del_T4_filt	shf_Avg(1)	shf_Fac(1)	shf_Avg(2)	shf_Fac(2)	shf_cal(1)	shf_cal(2)	panel_tmpr_Avg	batt_volt_Avg	process_time_Avg	process_time_Max	buff_depth_Max	WaterDepth_V_Avg	WaterDepth_Avg	WaterDepth_corr	WaterDepth>0	DifPAR_Min	DifPAR_TMn	DifPAR_Max	DifPAR_TMx	DifPARmin_Avg	dpcnt_Max	slowsequence_Tot

G flux 3 from these
WVC	WVCcorr	fw_1_Avg	fw_2_Avg	Tsoil_wat_mean(1)	Tsoil_wat_mean(2)	Tsoil_wat_mean(3)	del_Tsoil_wat(1)	det_T1_filt	del_Tsoil_wat(2)	del_T2_filt	del_Tsoil_wat(3)	del_T3_filt	shf_Avg	shf_Fac	shf_cal	wnd_spd_Avg	wnd_dir_Unit_Vec	wnd_dir_Std	panel_tmpr_Avg	batt_volt_Avg	process_time_Avg	process_time_Max	buff_depth_Max	WaterDepth_V_Avg	WaterDepth_Avg	WaterDepth_corr	WaterDepth>0	slowsequence_Tot	G flux 3	Gavg

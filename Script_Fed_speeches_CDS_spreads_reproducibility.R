library(data.table)
library(tidyverse)
library(plm)
library(lmtest)
library(broom)
library(sandwich)

##### Progress bar will tell us which table we're working on ####
pb <- txtProgressBar(min = 0, max = 10, style = 3)

# Turn warnings off
options(warn = -1)

# Suppress scientific notation
options(scipen = 999)

# Keep track of time
start_time <- Sys.time()



##################################################################
########################## Table 1 ###############################
##################################################################


message("Starting Table 1")
setTxtProgressBar(pb, 1)

# Read table 1 data (except last row)
Table1 = readr::read_csv('data_table_1.csv')

summary_dt = rbindlist(lapply(names(Table1), function(v) {
  x = Table1[[v]]
  data.table(
    Variable = v,
    Min = min(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
}))

summary_dt = summary_dt[, (names(summary_dt)[-1]) := lapply(.SD, function(x) round(x, 2)), .SDcols = -1]

# Read table 1 data (last row)
Table1_CDS = readr::read_csv('data_table_CDS.csv')

summary_CDS_dt = data.table(
  Variable = "CDS Spread",
  Min = round(min(Table1_CDS$spread5y, na.rm = TRUE), 2),
  Mean = round(mean(Table1_CDS$spread5y, na.rm = TRUE), 2),
  Median = round(median(Table1_CDS$spread5y, na.rm = TRUE), 2),
  SD = round(sd(Table1_CDS$spread5y, na.rm = TRUE), 2),
  IQR = round(IQR(Table1_CDS$spread5y, na.rm = TRUE), 2),
  Max = round(max(Table1_CDS$spread5y, na.rm = TRUE), 2)
)

summary_final_dt = rbind(summary_dt, summary_CDS_dt)

model_table_1_summary <- summary_final_dt


#############################################################################
####################### Table 2 #############################################
#############################################################################

message("Starting Table 2")
setTxtProgressBar(pb, 2)

# Read Table 2 data
Table2 = readr::read_csv('data_table_2.csv') %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

# Panel data formatting
data_panel_T2 <- plm::pdata.frame(Table2, index = c("country", "date"))

########### Panel regressions

### Formulas

formula_RHS_tone <- CDS_spread ~ Tone + factor(month) + factor(weekday)
formula_RHS_tone_all <- CDS_spread ~ Tone + factor(month) + factor(weekday) +
  pct_CW + AWPS + US_VIX + US_Term_Spread + US_Bond_10Y + US_Return +
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap
formula_RHS_speech_dummy <- CDS_spread ~ Speech_Dummy + factor(month) + 
  factor(weekday)
formula_RHS_speech_dummy_no_text_all_else <- CDS_spread ~ Speech_Dummy + 
  factor(month) + factor(weekday) + US_VIX + US_Term_Spread + 
  US_Bond_10Y + US_Return + Debt_Ratio + Inflation + 
  ToT_Vol + Reserves + Market_Cap
formula_RHS_speech_dummy_interaction_only <- CDS_spread ~ Speech_Dummy + 
  Speech_Dummy:Tone_Dummy + factor(month) + factor(weekday)
formula_RHS_speech_dummy_interaction_all_controls <- CDS_spread ~ Speech_Dummy + 
  Speech_Dummy:Tone_Dummy + factor(month) + factor(weekday) + pct_CW_Dummy + 
  AWPS_Dummy + US_VIX + US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + 
  Inflation + ToT_Vol + Reserves + Market_Cap

# T2 C1
formula_T2_C1 <- formula_RHS_tone
model_T2_C1 <- plm::plm(formula = formula_T2_C1, data = data_panel_T2, 
                        model = 'within', effect = 'individual')
model_T2_C1_summary <- summary(model_T2_C1, vcov = vcovDC)

# T2 C2
formula_T2_C2 <- formula_RHS_tone_all
model_T2_C2 <- plm::plm(formula = formula_T2_C2, data = data_panel_T2, 
                        model = 'within', effect = 'individual')
model_T2_C2_summary <- summary(model_T2_C2, vcov = vcovDC)

# T2 C3
formula_T2_C3 <- formula_RHS_speech_dummy
model_T2_C3 <- plm::plm(formula = formula_T2_C3, data = data_panel_T2,
                        model = 'within', effect = 'individual')
model_T2_C3_summary <- summary(model_T2_C3, vcov = vcovDC)

# T2 C4
formula_T2_C4 <- formula_RHS_speech_dummy_no_text_all_else
model_T2_C4 <- plm::plm(formula = formula_T2_C4,
                        data = data_panel_T2,
                        model = 'within', effect = 'individual')
model_T2_C4_summary <- summary(model_T2_C4, vcov = vcovDC)

# T2 C5
formula_T2_C5 <- formula_RHS_speech_dummy_interaction_only
model_T2_C5 <- plm::plm(formula = formula_T2_C5, data = data_panel_T2,
                        model = 'within', effect = 'individual')
model_T2_C5_summary <- summary(model_T2_C5, vcov = vcovDC)

# T2 C6
formula_T2_C6 <- formula_RHS_speech_dummy_interaction_all_controls
model_T2_C6 <- plm::plm(formula = formula_T2_C6, data = data_panel_T2,
                        model = 'within', effect = 'individual')
model_T2_C6_summary <- summary(model_T2_C6, vcov = vcovDC)

#########################################################################
######################### Table 3 #######################################
#########################################################################

message("Starting Table 3")
setTxtProgressBar(pb, 3)

# Read Table 3 data (Panels A and B)
Table3 = readr::read_csv('data_table_3.csv')  %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

# Read Table 3 panel C data on CDS risk premiums
Table3_C <- readr::read_csv('data_table_CDS_RP.csv') %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

### Merge with the CDS risk premium data (for panel C)
Table3 <- Table3 %>% dplyr::full_join(., Table3_C, by = c('country', 'date'))

# Panel data formatting
data_panel_T3 <- plm::pdata.frame(Table3, index = c("country", "date"))

### Formulas

# Panels A and B (LHS = CDS_spread_change)

formula_change_spread_macro <- CDS_spread_change ~ Macro_Tone * IR_Shock_Dummy +
  factor(month) + factor(weekday) + pct_CW_Macro + AWPS_Macro + 
  US_VIX_change + US_Term_Spread_change + US_Bond_10Y_change + 
  US_Return_change + Debt_change + Inflation_change + ToT_Vol_change + 
  Reserves_change + Market_Cap_change

formula_change_spread_fin <- CDS_spread_change ~ Fin_Tone * EQ_Shock_Dummy + 
  factor(month) + factor(weekday) + pct_CW_Fin + AWPS_Fin + US_VIX_change + 
  US_Term_Spread_change + US_Bond_10Y_change + US_Return_change + Debt_change + 
  Inflation_change + ToT_Vol_change + Reserves_change + Market_Cap_change

formula_change_spread_macrofin <- CDS_spread_change ~ Macro_Tone * IR_Shock_Dummy + 
  Fin_Tone * EQ_Shock_Dummy + factor(month) + factor(weekday) + pct_CW_Macro + 
  AWPS_Macro + pct_CW_Fin + AWPS_Fin + US_VIX_change + US_Term_Spread_change + 
  US_Bond_10Y_change + US_Return_change + Debt_change + Inflation_change + 
  ToT_Vol_change + Reserves_change + Market_Cap_change



############## Panel Regressions

### Panel A

# T3 C1
formula_T3_C1_A <- formula_change_spread_macro
model_T3_C1_A <- plm::plm(formula = formula_T3_C1_A, data = data_panel_T3, 
                        model = 'within', effect = 'individual')
model_T3_C1_A_summary <- summary(model_T3_C1_A, vcov = vcovDC)

# T3 C2
formula_T3_C2_A <- formula_change_spread_fin
model_T3_C2_A <- plm::plm(formula = formula_T3_C2_A, data = data_panel_T3, 
                        model = 'within', effect = 'individual')
model_T3_C2_A_summary <- summary(model_T3_C2_A, vcov = vcovDC)



# T3 C3
formula_T3_C3_A <- formula_change_spread_macrofin
model_T3_C3_A <- plm::plm(formula = formula_T3_C3_A, data = data_panel_T3, 
                        model = 'within', effect = 'individual')
model_T3_C3_A_summary <- summary(model_T3_C3_A, vcov = vcovDC)


### Panel B

data_panel_T3_chair <- data_panel_T3 %>%
  dplyr::filter(Position == 'Chairman' | Position == 'Chair')

set.seed(123)

# T3 C1
formula_T3_C1_B <- formula_change_spread_macro
model_T3_C1_B <- plm::plm(formula = formula_T3_C1_B, 
                          data = data_panel_T3_chair, 
                          model = 'within', effect = 'individual')
model_T3_C1_B_summary <- summary(model_T3_C1_B, vcov = vcovBS)

# T3 C2
formula_T3_C2_B <- formula_change_spread_fin
model_T3_C2_B <- plm::plm(formula = formula_T3_C2_B, data = data_panel_T3_chair, 
                          model = 'within', effect = 'individual')
model_T3_C2_B_summary <- summary(model_T3_C2_B, vcov = vcovBS)

# T3 C3
formula_T3_C3_B <- formula_change_spread_macrofin
model_T3_C3_B <- plm::plm(formula = formula_T3_C3_A, data = data_panel_T3_chair, 
                          model = 'within', effect = 'individual')
model_T3_C3_B_summary <- summary(model_T3_C3_B, vcov = vcovBS)


### Panel C (LHS = CDS_risk_premium_change)

formula_change_RP_macro <- CDS_risk_premium_change ~ Macro_Tone * IR_Shock_Dummy +
  factor(month) + factor(weekday) + pct_CW_Macro + AWPS_Macro + 
  US_VIX_change + US_Term_Spread_change + US_Bond_10Y_change + 
  US_Return_change + Debt_change + Inflation_change + ToT_Vol_change + 
  Reserves_change + Market_Cap_change

formula_change_RP_fin <- CDS_risk_premium_change ~ Fin_Tone * EQ_Shock_Dummy + 
  factor(month) + factor(weekday) + pct_CW_Fin + AWPS_Fin + US_VIX_change + 
  US_Term_Spread_change + US_Bond_10Y_change + US_Return_change + Debt_change + 
  Inflation_change + ToT_Vol_change + Reserves_change + Market_Cap_change

formula_change_RP_macrofin <- CDS_risk_premium_change ~ Macro_Tone * IR_Shock_Dummy + 
  Fin_Tone * EQ_Shock_Dummy + factor(month) + factor(weekday) + pct_CW_Macro + 
  AWPS_Macro + pct_CW_Fin + AWPS_Fin + US_VIX_change + US_Term_Spread_change + 
  US_Bond_10Y_change + US_Return_change + Debt_change + Inflation_change + 
  ToT_Vol_change + Reserves_change + Market_Cap_change


# T3 C1
formula_T3_C1_C <- formula_change_RP_macro
model_T3_C1_C <- plm::plm(formula = formula_T3_C1_C, data = data_panel_T3, 
                          model = 'within', effect = 'individual')
model_T3_C1_C_summary <- summary(model_T3_C1_C, vcov = vcovDC)

# T3 C2
formula_T3_C2_C <- formula_change_RP_fin
model_T3_C2_C <- plm::plm(formula = formula_T3_C2_C, data = data_panel_T3, 
                          model = 'within', effect = 'individual')
model_T3_C2_C_summary <- summary(model_T3_C2_C, vcov = vcovDC)

# T3 C3
formula_T3_C3_C <- formula_change_RP_macrofin
model_T3_C3_C <- plm::plm(formula = formula_T3_C3_C, data = data_panel_T3, 
                          model = 'within', effect = 'individual')
model_T3_C3_C_summary <- summary(model_T3_C3_C, vcov = vcovDC)


############################################################################
############################## Table 4 #####################################
############################################################################

message("Starting Table 4")
setTxtProgressBar(pb, 4)

########## Panel A

#set.seed(123)

# Read Table 4 Panel A data
Table4_A = readr::read_csv('data_table_4_PA.csv')  %>%
  dplyr::select(country, year_quarter, everything()) %>%
  dplyr::arrange(country)

# Panel data formatting
data_panel_T4_A <- plm::pdata.frame(Table4_A, index = c("country", "year_quarter"))

### Formulas

formula_crossborder_tone_controls  <- log(Cross_Border_Flows) ~ Tone + 
  pct_CW + AWPS + US_VIX + US_Term_Spread + US_Bond10Y + Debt_Ratio + 
  Inflation + ToT_Vol + Reserves + US_Return 
formula_CDS_spread_crossborder_controls <- CDS_Spread ~ log(Cross_Border_Flows) + 
  pct_CW + AWPS + US_VIX + US_Term_Spread + US_Bond10Y + Debt_Ratio + 
  Inflation + ToT_Vol + Reserves + US_Return 


# T4 C1
formula_T4_C1_A <- formula_crossborder_tone_controls
model_T4_C1_A <- plm::plm(formula = formula_T4_C1_A, data = data_panel_T4_A,
                        model = 'within', effect = 'individual')
model_T4_C1_A_summary <- summary(model_T4_C1_A, vcov = vcovDC)



# T4 C2
formula_T4_C2_A <- formula_CDS_spread_crossborder_controls
model_T4_C2_A <- plm::plm(formula = formula_T4_C2_A, data = data_panel_T4_A,
                        model = 'within', effect = 'individual')
set.seed(123)
model_T4_C2_A_summary <- summary(model_T4_C2_A, vcov = vcovBS)

############ Panel B

# Read Table 4 Panel B data
Table4_B = readr::read_csv('data_table_4_PB.csv')  %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

# Panel data formatting
data_panel_T4_B <- plm::pdata.frame(Table4_B, index = c("country", "date"))


#### Formulas

formula_forex_tone_controls <- Forex_Return ~ Tone + factor(month) + 
  factor(weekday) + pct_CW + AWPS + US_VIX + US_Term_Spread + 
  US_Bond_10Y + US_Return + Debt_Ratio + Inflation + ToT_Vol + 
  Reserves + Market_Cap
formula_forex_speech_dummy_controls <- Forex_Return ~ Speech_Dummy + 
  factor(month) + factor(weekday) + pct_CW_Dummy + AWPS_Dummy + 
  US_VIX + US_Term_Spread + US_Bond_10Y + US_Return + 
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap
formula_forex_speech_dummy_interaction_controls <- Forex_Return ~ Speech_Dummy + 
  Speech_Dummy:Tone_Dummy + factor(month) + factor(weekday) + 
  pct_CW_Dummy + AWPS_Dummy + + US_VIX + US_Term_Spread + 
  US_Bond_10Y + US_Return + Debt_Ratio + Inflation + ToT_Vol + 
  Reserves + Market_Cap

### Panel regressions

# T4 C1
formula_T4_C1_B <- formula_forex_tone_controls
model_T4_C1_B <- plm::plm(formula = formula_T4_C1_B, data = data_panel_T4_B, 
                          model = 'within', effect = 'individual')
model_T4_C1_B_summary <- summary(model_T4_C1_B, vcov = vcovDC)

# T4 C2
formula_T4_C2_B <- formula_forex_speech_dummy_controls
model_T4_C2_B <- plm::plm(formula = formula_T4_C2_B, data = data_panel_T4_B, 
                          model = 'within', effect = 'individual')
model_T4_C2_B_summary <- summary(model_T4_C2_B, vcov = vcovDC)

# T4 C3
formula_T4_C3_B <- formula_forex_speech_dummy_interaction_controls
model_T4_C3_B <- plm::plm(formula = formula_T4_C3_B, data = data_panel_T4_B, 
                          model = 'within', effect = 'individual')
model_T4_C3_B_summary <- summary(model_T4_C3_B, vcov = vcovDC)

###### Panel C

# Read Table 4 Panel C data
Table4_C = readr::read_csv('data_table_4_PC.csv')  %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

# Panel data formatting
data_panel_T4_C <- plm::pdata.frame(Table4_C, index = c("country", "date"))

#### Formula

formula_USD_minus_EUR <- USD_minus_EUR_CDS ~ US_minus_ECB_Tone + 
  pct_CW_US + AWPS_US + pct_CW_ECB + AWPS_ECB + 
  US_VIX + US_Term_Spread + US_Bond_10Y + US_Return + 
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap

### Panel regression

# T4 C1
formula_T4_C1_C <- formula_USD_minus_EUR
model_T4_C1_C <- plm::plm(formula = formula_T4_C1_C, data = data_panel_T4_C, 
                          model = 'within', effect = 'individual')
model_T4_C1_C_summary <- summary(model_T4_C1_C, vcov = vcovDC)

############################################################################
##################### Table 5 ##############################################
############################################################################

message("Starting Table 5")
setTxtProgressBar(pb, 5)

# Read Table 5 data
Table5 = readr::read_csv('data_table_5.csv')  %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

# Panel data formatting
data_panel_T5 <- plm::pdata.frame(Table5, index = c("country", "date"))

### Formulas

formula_cds_spread_tone_emerging_dev_no_controls <- CDS_spread ~ Tone + 
  factor(classification) + Tone:factor(classification) + factor(month) + 
  factor(weekday) + pct_CW + AWPS

formula_cds_spread_tone_emerging_dev_all_controls <- CDS_spread ~ Tone + 
  factor(classification) + Tone:factor(classification) + factor(month) + 
  factor(weekday) + pct_CW + AWPS + US_VIX + US_Term_Spread +
  US_Bond_10Y + US_Return + Debt_Ratio + Inflation + ToT_Vol + 
  Reserves + Market_Cap

formula_cds_spread_speech_dummy_emerging_dev_no_controls <- CDS_spread ~ Speech_Dummy + 
  factor(classification) + Speech_Dummy:factor(classification) + 
  factor(month) + factor(weekday) + pct_CW_Dummy + AWPS_Dummy

formula_cds_spread_speech_dummy_emerging_dev_all_controls <- CDS_spread ~ Speech_Dummy + 
  factor(classification) + Speech_Dummy:factor(classification) + factor(month) + 
  factor(weekday) + pct_CW_Dummy + AWPS_Dummy + US_VIX + US_Term_Spread + 
  US_Bond_10Y + US_Return + Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap

### Panel regressions

# T5 C1
formula_T5_C1 <- formula_cds_spread_tone_emerging_dev_no_controls
model_T5_C1 <- plm::plm(formula = formula_T5_C1, data = data_panel_T5, 
                          model = 'within', effect = 'individual')
model_T5_C1_summary <- summary(model_T5_C1, vcov = vcovDC)

# T5 C2
formula_T5_C2 <- formula_cds_spread_tone_emerging_dev_all_controls
model_T5_C2 <- plm::plm(formula = formula_T5_C2, data = data_panel_T5, 
                        model = 'within', effect = 'individual')
model_T5_C2_summary <- summary(model_T5_C2, vcov = vcovDC)

# T5 C3
formula_T5_C3 <- formula_cds_spread_speech_dummy_emerging_dev_no_controls
model_T5_C3 <- plm::plm(formula = formula_T5_C3, data = data_panel_T5, 
                        model = 'within', effect = 'individual')
model_T5_C3_summary <- summary(model_T5_C3, vcov = vcovDC)

# T5 C4
formula_T5_C4 <- formula_cds_spread_speech_dummy_emerging_dev_all_controls
model_T5_C4 <- plm::plm(formula = formula_T5_C4, data = data_panel_T5, 
                        model = 'within', effect = 'individual')
model_T5_C4_summary <- summary(model_T5_C4, vcov = vcovDC)

############################################################################
###################### Table 6 #############################################
############################################################################


message("Starting Table 6")
setTxtProgressBar(pb, 6)

# Read Table 6 data
Table6 = readr::read_csv('data_table_6_7.csv')  %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

# Panel data formatting
data_panel_T6 <- plm::pdata.frame(Table6, index = c("country", "date"))

### Formulas

formula_tone_FC_interaction <- CDS_spread ~ Tone + factor(FC) + 
  Tone:factor(FC) + factor(month) + factor(weekday) + pct_CW + 
  AWPS + US_VIX + US_Term_Spread + US_Bond_10Y + US_Return + 
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap

formula_speech_dummy_FC_interaction <- CDS_spread ~ Speech_Dummy + 
  factor(FC) + Speech_Dummy:factor(FC) + factor(month) + 
  factor(weekday) + pct_CW_Dummy + AWPS_Dummy + US_VIX + 
  US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + 
  Inflation + ToT_Vol + Reserves + Market_Cap

formula_tone_tight_interaction <- CDS_spread ~ Tone + factor(Tight) + 
  Tone:factor(Tight) + factor(month) + factor(weekday) + pct_CW + 
  AWPS + US_VIX + US_Term_Spread + US_Bond_10Y + US_Return + 
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap

formula_speech_dummy_tight_interaction <- CDS_spread ~ Speech_Dummy + 
  factor(Tight) + Speech_Dummy:factor(Tight) + factor(month) + 
  factor(weekday) + pct_CW_Dummy + AWPS_Dummy + US_VIX + 
  US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + 
  Inflation + ToT_Vol + Reserves + Market_Cap

### Panel regressions

# T6 C1
formula_T6_C1 <- formula_tone_FC_interaction
model_T6_C1 <- plm::plm(formula = formula_T6_C1, data = data_panel_T6, 
                        model = 'within', effect = 'individual')
model_T6_C1_summary <- summary(model_T6_C1, vcov = vcovDC)

# T6 C2
formula_T6_C2 <- formula_speech_dummy_FC_interaction
model_T6_C2 <- plm::plm(formula = formula_T6_C2, data = data_panel_T6, 
                        model = 'within', effect = 'individual')
model_T6_C2_summary <- summary(model_T6_C2, vcov = vcovDC)

# T6 C3
formula_T6_C3 <- formula_tone_tight_interaction
model_T6_C3 <- plm::plm(formula = formula_T6_C3, data = data_panel_T6, 
                        model = 'within', effect = 'individual')
model_T6_C3_summary <- summary(model_T6_C3, vcov = vcovDC)

# T6 C4
formula_T6_C4 <- formula_speech_dummy_tight_interaction
model_T6_C4 <- plm::plm(formula = formula_T6_C4, data = data_panel_T6, 
                        model = 'within', effect = 'individual')
model_T6_C4_summary <- summary(model_T6_C4, vcov = vcovDC)


############################################################################
###################### Table 7 #############################################
############################################################################


message("Starting Table 7")
setTxtProgressBar(pb, 7)

# Panel data formatting
data_panel_T7 <- data_panel_T6

### Formulas


formula_pos_speech_dummy_FC_interaction <- CDS_spread ~ factor(FC) + 
  factor(Pos_Speech_Dummy) + factor(FC):factor(Pos_Speech_Dummy) + 
  factor(month) + factor(weekday) + pct_CW + AWPS + US_VIX + 
  US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + 
  Inflation + ToT_Vol + Reserves + Market_Cap


formula_pos_speech_dummy_tight_interaction <- CDS_spread ~ factor(Tight) + 
  factor(Pos_Speech_Dummy) + factor(Tight):factor(Pos_Speech_Dummy) + 
  factor(month) + factor(weekday) + pct_CW + AWPS + US_VIX + 
  US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + 
  Inflation + ToT_Vol + Reserves + Market_Cap

### Panel regressions

# T7 C1
formula_T7_C1 <- formula_pos_speech_dummy_FC_interaction
model_T7_C1 <- plm::plm(formula = formula_T7_C1, data = data_panel_T7, 
                        model = 'within', effect = 'individual')
model_T7_C1_summary <- summary(model_T7_C1, vcov = vcovDC)

# T7 C2
formula_T7_C2 <- formula_pos_speech_dummy_tight_interaction
model_T7_C2 <- plm::plm(formula = formula_T7_C2, data = data_panel_T7, 
                        model = 'within', effect = 'individual')
model_T7_C2_summary <- summary(model_T7_C2, vcov = vcovDC)

############################################################################
###################### Table 8 #############################################
############################################################################


message("Starting Table 8")
setTxtProgressBar(pb, 8)

# Read Table 8 data
Table8 = readr::read_csv('data_table_8.csv')  %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

# Panel data formatting
data_panel_T8 <- plm::pdata.frame(Table8, index = c("country", "date"))


### Formulas


formula_cds_spread_5_day_interval <- CDS_spread_change_5_day ~ Tone +
  factor(month) + factor(weekday) + pct_CW + AWPS + US_VIX_5_day +
  US_Term_Spread_5_day + US_Bond_10Y_5_day + US_Return_5_day +
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap_change_5_day

formula_cds_spread_10_day_interval <- CDS_spread_change_10_day ~ Tone +
  factor(month) + factor(weekday) + pct_CW + AWPS + US_VIX_10_day +
  US_Term_Spread_10_day + US_Bond_10Y_10_day + US_Return_10_day +
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap_change_10_day

formula_cds_spread_15_day_interval <-CDS_spread_change_15_day ~ Tone +
  factor(month) + factor(weekday) + pct_CW + AWPS + US_VIX_15_day +
  US_Term_Spread_15_day + US_Bond_10Y_15_day + US_Return_15_day +
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap_change_15_day

formula_cds_spread_20_day_interval <- CDS_spread_change_20_day ~ Tone +
  factor(month) + factor(weekday) + pct_CW + AWPS + US_VIX_20_day +
  US_Term_Spread_20_day + US_Bond_10Y_20_day + US_Return_20_day +
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap_change_20_day



### Panel regressions

# T8 C1
formula_T8_C1 <- formula_cds_spread_5_day_interval
model_T8_C1 <- plm::plm(formula = formula_T8_C1, data = data_panel_T8,
                        model = 'within', effect = 'individual')
model_T8_C1_summary <- summary(model_T8_C1, vcov = vcovDC)

# T8 C2
formula_T8_C2 <- formula_cds_spread_10_day_interval
model_T8_C2 <- plm::plm(formula = formula_T8_C2, data = data_panel_T8,
                        model = 'within', effect = 'individual')
model_T8_C2_summary <- summary(model_T8_C2)

# T8 C3
formula_T8_C3 <- formula_cds_spread_15_day_interval
model_T8_C3 <- plm::plm(formula = formula_T8_C3, data = data_panel_T8,
                        model = 'within', effect = 'individual')
model_T8_C3_summary <- summary(model_T8_C3, vcov = vcovDC)

# T8 C4
formula_T8_C4 <- formula_cds_spread_20_day_interval
model_T8_C4 <- plm::plm(formula = formula_T8_C4, data = data_panel_T8,
                        model = 'within', effect = 'individual')
model_T8_C4_summary <- summary(model_T8_C4, vcov = vcovDC)

############################################################################
###################### Table 9 #############################################
############################################################################


message("Starting Table 9")
setTxtProgressBar(pb, 9)

Table9_2w <- readr::read_csv('data_table_9_2w_new.csv')  %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

Table9_3w <- readr::read_csv('data_table_9_3w_new.csv')  %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

Table9_4w <- readr::read_csv('data_table_9_4w_new.csv')  %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

# Panel data formatting
data_panel_T9_2w <- plm::pdata.frame(Table9_2w, index = c("country", "date"))

data_panel_T9_3w <- plm::pdata.frame(Table9_3w, index = c("country", "date"))

data_panel_T9_4w <- plm::pdata.frame(Table9_4w, index = c("country", "date"))

### Formulas 

formula_2w_prior <- CDS_spread ~ Tone + Tone_FOMC + factor(month) + 
  factor(weekday) + pct_CW + AWPS + pct_CW_FOMC + AWPS_FOMC + 
  US_VIX + US_Term_Spread + US_Bond_10Y + US_Return + 
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap

formula_3w_prior <- formula_2w_prior
formula_4w_prior <- formula_2w_prior

### Panel regressions

# T9 C1
formula_T9_C1 <- formula_2w_prior
model_T9_C1 <- plm::plm(formula = formula_T9_C1, data = data_panel_T9_2w, 
                        model = 'within', effect = 'individual')
model_T9_C1_summary <- summary(model_T9_C1, vcov = vcovDC)

# T9 C2
formula_T9_C2 <- formula_3w_prior
model_T9_C2 <- plm::plm(formula = formula_T9_C2, data = data_panel_T9_3w, 
                        model = 'within', effect = 'individual')
model_T9_C2_summary <- summary(model_T9_C2, vcov = vcovDC)

# T9 C3
formula_T9_C3 <- formula_4w_prior
model_T9_C3 <- plm::plm(formula = formula_T9_C3, data = data_panel_T9_4w, 
                        model = 'within', effect = 'individual')
model_T9_C3_summary <- summary(model_T9_C3, vcov = vcovDC)

############################################################################
###################### Table 10 ############################################
############################################################################


message("Starting Table 10")
setTxtProgressBar(pb, 10)


############### Panel A

Table10_A <- readr::read_csv('data_table_10_PA.csv') %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

# Panel data formatting
data_panel_T10_A <- plm::pdata.frame(Table10_A, index = c("country", "date"))

### Formulas

formula_DH_index <- CDS_spread ~ DH_Index + factor(month) + factor(weekday) + 
  pct_CW + AWPS + US_VIX + US_Term_Spread + US_Bond_10Y + 
  US_Return + Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap

formula_Finbert <- CDS_spread ~ FinBERT_Tone + factor(month) + 
  factor(weekday) + pct_CW + AWPS + US_VIX + 
  US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + 
  Inflation + ToT_Vol + Reserves + Market_Cap

formula_LM <- CDS_spread ~ LM_Tone + factor(month) + factor(weekday) + 
  pct_CW + AWPS + US_VIX + US_Term_Spread + US_Bond_10Y + 
  US_Return + Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap

# formula_DH_index_interaction <- CDS_spread ~ DH_Index_Dummy + 
#   Speech_Dummy + DH_Index_Dummy:Speech_Dummy + factor(month) + 
#   factor(weekday) + pct_CW_Dummy + AWPS_Dummy + US_VIX + 
#   US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + 
#   Inflation + ToT_Vol + Reserves + Market_Cap
formula_DH_index_interaction <- CDS_spread ~ Speech_Dummy + 
  DH_Index_Dummy:Speech_Dummy + factor(month) + 
  factor(weekday) + pct_CW_Dummy + AWPS_Dummy + US_VIX + 
  US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + 
  Inflation + ToT_Vol + Reserves + Market_Cap

# formula_Finbert_interaction <- CDS_spread ~ FinBERT_Tone_Dummy + Speech_Dummy + 
#   FinBERT_Tone_Dummy:Speech_Dummy + factor(month) + 
#   factor(weekday) + pct_CW_Dummy + AWPS_Dummy + US_VIX + 
#   US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + 
#   Inflation + ToT_Vol + Reserves + Market_Cap
formula_Finbert_interaction <- CDS_spread ~ Speech_Dummy + 
  FinBERT_Tone_Dummy:Speech_Dummy + factor(month) + 
  factor(weekday) + pct_CW_Dummy + AWPS_Dummy + US_VIX + 
  US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + 
  Inflation + ToT_Vol + Reserves + Market_Cap


# formula_LM_interaction <- CDS_spread ~ LM_Tone_Dummy + Speech_Dummy + 
#   LM_Tone_Dummy:Speech_Dummy + factor(month) + factor(weekday) + 
#   pct_CW_Dummy + AWPS_Dummy + US_VIX + US_Term_Spread + US_Bond_10Y + 
#   US_Return + Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap
formula_LM_interaction <- CDS_spread ~ Speech_Dummy + 
  LM_Tone_Dummy:Speech_Dummy + factor(month) + factor(weekday) + 
  pct_CW_Dummy + AWPS_Dummy + US_VIX + US_Term_Spread + US_Bond_10Y + 
  US_Return + Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap


### Panel regressions

# T10 C1
formula_T10_C1_A <- formula_DH_index
model_T10_C1_A <- plm::plm(formula = formula_T10_C1_A, data = data_panel_T10_A, 
                        model = 'within', effect = 'individual')
model_T10_C1_A_summary <- summary(model_T10_C1_A, vcov = vcovDC)

# T10 C2
formula_T10_C2_A <- formula_Finbert
model_T10_C2_A <- plm::plm(formula = formula_T10_C2_A, data = data_panel_T10_A, 
                        model = 'within', effect = 'individual')
model_T10_C2_A_summary <- summary(model_T10_C2_A, vcov = vcovDC)

# T10 C3
formula_T10_C3_A <- formula_LM
model_T10_C3_A <- plm::plm(formula = formula_T10_C3_A, data = data_panel_T10_A,
                        model = 'within', effect = 'individual')
model_T10_C3_A_summary <- summary(model_T10_C3_A, vcov = vcovDC)

# T10 C4
formula_T10_C4_A <- formula_DH_index_interaction
model_T10_C4_A <- plm::plm(formula = formula_T10_C4_A,
                        data = data_panel_T10_A,
                        model = 'within', effect = 'individual')
model_T10_C4_A_summary <- summary(model_T10_C4_A, vcov = vcovDC)

# T10 C5
formula_T10_C5_A <- formula_Finbert_interaction
model_T10_C5_A <- plm::plm(formula = formula_T10_C5_A, data = data_panel_T10_A,
                        model = 'within', effect = 'individual')
model_T10_C5_A_summary <- summary(model_T10_C5_A, vcov = vcovDC)

# T10 C6
formula_T10_C6_A <- formula_LM_interaction
model_T10_C6_A <- plm::plm(formula = formula_T10_C6_A, data = data_panel_T10_A,
                        model = 'within', effect = 'individual')
model_T10_C6_A_summary <- summary(model_T10_C6_A, vcov = vcovDC)


############### Panel B

Table10_B <- readr::read_csv('data_table_10_PB_new.csv') %>%
  dplyr::select(country, date, everything()) %>%
  dplyr::arrange(country)

# Panel data formatting
data_panel_T10_B <- plm::pdata.frame(Table10_B, index = c("country", "date"))

### Formulas 

formula_DH_residual <- CDS_spread ~ DH_Index + Tone_DH_Residual + factor(month) + 
  factor(weekday) + pct_CW + AWPS + US_VIX + US_Term_Spread + US_Bond_10Y + 
  US_Return + Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap

formula_Finbert_residual <- CDS_spread ~ FinBERT_Tone + Tone_FinBERT_Residual + 
  factor(month) + factor(weekday) + pct_CW + AWPS + US_VIX + 
  US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + Inflation + 
  ToT_Vol + Reserves + Market_Cap

formula_LM_residual <- CDS_spread ~ LM_Tone + Tone_LM_Residual + 
  factor(month) + factor(weekday) + pct_CW + AWPS + US_VIX + 
  US_Term_Spread + US_Bond_10Y + US_Return + Debt_Ratio + Inflation + 
  ToT_Vol + Reserves + Market_Cap

formula_all_residual <- CDS_spread ~ DH_Index + FinBERT_Tone + LM_Tone + 
  Tone_All_Residual + factor(month) + factor(weekday) + pct_CW + 
  AWPS + US_VIX + US_Term_Spread + US_Bond_10Y + US_Return + 
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap

formula_speech_dummy_all_residual <- CDS_spread ~ Speech_Dummy + 
  Speech_Dummy:DH_Index_Dummy + Speech_Dummy:FinBERT_Tone_Dummy + 
  Speech_Dummy:LM_Tone_Dummy + Speech_Dummy:Tone_All_Residual_Dummy + 
  factor(month) + factor(weekday) + pct_CW_Dummy + AWPS_Dummy + 
  US_VIX + US_Term_Spread + US_Bond_10Y + US_Return + 
  Debt_Ratio + Inflation + ToT_Vol + Reserves + Market_Cap

### Panel regressions

# T10 C1
formula_T10_C1_B <- formula_DH_residual
model_T10_C1_B <- plm::plm(formula = formula_T10_C1_B, data = data_panel_T10_B, 
                           model = 'within', effect = 'individual')
model_T10_C1_B_summary <- summary(model_T10_C1_B, vcov = vcovDC)

# T10 C2
formula_T10_C2_B <- formula_Finbert_residual
model_T10_C2_B <- plm::plm(formula = formula_T10_C2_B, data = data_panel_T10_B, 
                           model = 'within', effect = 'individual')
model_T10_C2_B_summary <- summary(model_T10_C2_B, vcov = vcovDC)

# T10 C3
formula_T10_C3_B <- formula_LM_residual
model_T10_C3_B <- plm::plm(formula = formula_T10_C3_B, data = data_panel_T10_B,
                           model = 'within', effect = 'individual')
model_T10_C3_B_summary <- summary(model_T10_C3_B, vcov = vcovDC)

# T10 C4
formula_T10_C4_B <- formula_all_residual
model_T10_C4_B <- plm::plm(formula = formula_T10_C4_B,
                           data = data_panel_T10_B,
                           model = 'within', effect = 'individual')
model_T10_C4_B_summary <- summary(model_T10_C4_B, vcov = vcovDC)

# T10 C5
formula_T10_C5_B <- formula_speech_dummy_all_residual
model_T10_C5_B <- plm::plm(formula = formula_T10_C5_B, data = data_panel_T10_B,
                           model = 'within', effect = 'individual')
model_T10_C5_B_summary <- summary(model_T10_C5_B, vcov = vcovDC)

############################################################################

# Close progress box
close(pb)
# Print elapsed time
end_time <- Sys.time()
print(end_time - start_time)  
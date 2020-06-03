library(shinydashboard)
library(tidyverse)
library(DT)
library(dplyr)
library(ggplot2)
 
library(plotly) 
library(data.table)
library(lubridate) 
 
library(ggpubr)
library(shinydashboardPlus)


 
dci_data_eda <- readRDS('data/clean_df_for_EDA.RDS')

 
dist_plts<- readRDS('data/dist_plts.rds')

#payors
payors <-c("United Health_02371001" = '02371001',			
          # '24180402'	,			
           #'06791013'	,	
           "UPMC Health_12320609"='12320609',			
           "Humana_11371008"='11371008',
           "Wellcare_21560402"='21560402',			
           "Anthem_00410417"= '00410417',			
           "Highmark Group_16181025"='16181025',			
           "Molina_00410601"= '00410601',			
           "Blue Cross_00410825"='00410825',
           "CSV" = '00410235')




meds_data <-readRDS('data/meds_again.rds')


algo <-readRDS('data/clean_df_for_algorithm.RDS')

compare_actuals_to_pred_shiny <-readRDS('data/compare_actuals_to_pred.rds')

all_features_shiny<-as.data.frame( colnames(compare_actuals_to_pred))

colnames(all_features) = c('features')

#payor codes
pcn_payor_code_shiny<-c(
  "United Health_02371001"='pcn_payer_code_02371001',
  "CSV_00410235"='pcn_payer_code_00410235',				
  #'pcn_payer_code_00410417',				
  "Molina_00410601"='pcn_payer_code_00410601',				
  "Highmark Group_16181025"='pcn_payer_code_00410825',				
  #'pcn_payer_code_00410901',				
  #'pcn_payer_code_06771001',				
  #'pcn_payer_code_06791013',				
  #'pcn_payer_code_11371008',				
  "UPMC Health_12320609"='pcn_payer_code_12320609')



modality_cost_codes_shiny <- c(
                          "HEMO_1110"="modality_cost_code_1110",
                          "CAPD HOME TRAINING_1220"="modality_cost_code_1220",
                          "CCPD HOME TRAINING_1230" ="modality_cost_code_1230",
                          "CAPD HOME TREATMENT_1320" =	"modality_cost_code_1320",
                          "CCPD HOME TREATMENT_1330" = "modality_cost_code_1330")
 

  
  hcpc_code_shiny <- c("FLUZONE QUADRIVALENT"="hcpc_code_90686",
                      "ENGERIX-B"= "hcpc_code_90747",
                      "CEFTRIAXONE SODIUM"= "hcpc_code_J0696",
                      "VANCOMYCIN HCL" ="hcpc_code_J3370",
                      "PNEUMOVAX" = "hcpc_code_90732",
                      "CEFTAZIDIME" = "hcpc_code_J0713",
                      "CEFAZOLIN SODIUM" = "hcpc_code_J0690",
                      "HEPARIN" ="hcpc_code_J1644",
                      "FLUZONE" = "hcpc_code_90662",
                      "TUBERSOL" ="hcpc_code_86580",
                      "ARANESP/DARBEPOETIN ALFA" ="hcpc_code_J0882",
                      "FLUZONE QUADRIVALENT" = "hcpc_code_90688"
                        )

		

	


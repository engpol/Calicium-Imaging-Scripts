
library('tidyverse')
library('reshape2')
library('xts')
library('zoo')
library('readxl')
library('purrr')
library('qpcR')
library('ggforce')
library('Hmisc')
library('cowplot')
library('lme4')
library('texreg')
library('DescTools')
library('splines')
source("C:/Users/olikc/Desktop/MPhil Research/R_Scripts/Background_Subtraction_Func.R")
##MASTER CODE FOR ANALYSING CALCIUM IMAGING DATA


## LOADING IN RAW DATA - THIS iS WHAT YOU CHANGE in EXCEL_FILE

library('readxl')


#Extremely useful function found online - lets you use dplyr mutate based on subset of rows


mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

##SEMAGLUTIDE = C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/Sema_200nM/ALL_SEMA_PLUS_VEHICLE_CERITINIB_MASTER_DOC.xlsx

#TIRZEPATIDE = C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/Tirzepatide_200nM/ALL_TIRZ_DATA_NEW_FORMAT_Updated_22_05_23.xlsx

#LIRAGLUTIDE = C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/Liraglutide/Lira_200nM_ALL_DATA_NEW_FORMAT.xlsx"

#GLP-1 = "C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/GLP-1_200nM_CAL-590/ALL_GLP_1_Data_PLUS_VEHICLES_MASTER_DOCUMENT.xlsx"

#ALL GLP-1 AGONISTS = "C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/ALL_GLP_1_AGONISTS.xlsx"

#ALL GLP-1 AGONISTS + TIRZA = "C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/ALL_GLP_1_AGONISTS_AND_TIRZ.xlsx"

#OREXIN2A =  "C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/Orexin_YNT/ORX2A_11_05_23.xlsx"

#Fast Responders ("C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/Fast_Responders.xlsx")

#Ceritnib Data = "C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/Ceritinib_Data.xlsx"

#Biased_Agonists = "C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/Biased_Agonist.xlsx"

excel_file <-  "C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/Orexin_YNT/ORX2A_11_05_23.xlsx"

Intensity_Raw_Data <-  read_excel(excel_file, 
                                     sheet = "Intensity")

GFP_Raw_Data <-  read_excel(excel_file, 
                                sheet = "GFP", n_max = 3)


Drug_Times_Raw_Data <-  read_excel(excel_file, 
                                       sheet = "Drug_Times")

##
##
##          PREPARING DATA FOR ANALYSIS - Background Sub function declared elsewhere
##
## 

data_loaded <- Background_Subtract(Intensity_Raw_Data,GFP_Raw_Data,Drug_Times_Raw_Data)

##
##
## EXTRA FOR CERITINIB DATA - CHOOSE ONE OF THESE DEPENDING ON WHICH DATA YOU WANT - RE RUN PREVIOUS CODE IF CHANGING !!!!
##
##

data_FINAL <- data_loaded


data_FINAL <- data_loaded %>%
  filter(ALK_OR_SEMA == "SEMA") %>%
  filter(LENGTH_INCUBATION == "24H") %>%
  filter(Synaptic_Blockers != "No")


data_FINAL <- data_loaded %>%
  filter(ALK_OR_SEMA == "SEMA") %>%
  filter(LENGTH_INCUBATION == "48H")


data_FINAL <- data_loaded %>%
  filter(ALK_OR_SEMA == "ALK") 


##DRUG APP Times Vary, Change Variable Names Accordingly below!
## I.E DrugApp1 = Time of application for first drug, DrugTime1= length of time applied,
## ONLY CHANGE THE STRING VARIABLES!!! 


DrugApp1_string <- "Sema_1"
DrugTime1_string <- "Sema_1_Time"

DrugApp2_string <- "Sema_2"
DrugTime2_string <- "Sema_2_Time"

Glu_string <- "Glu_Gly"
GluTime_string <- "Glu_Gly_Time"

KCLapp_string <- "KCL"
KCLtime_string <- "KCL_Time"


DrugApp1_string <- "ORX2A"
DrugTime1_string <- "ORX2A_App_Time"

DrugApp2_string <- "ORX2A_2"
DrugTime2_string <- "ORX2A_2_App_Time"

Glu_string <- "Glu_Gly"
GluTime_string <- "Glu_Gly_Time"

KCLapp_string <- "KCL"
KCLtime_string <- "KCL_Time"



DrugApp1_string <- "Agonist_1"
DrugTime1_string <- "Agonist_1_App_Time"

DrugApp2_string <- "Agonist_2"
DrugTime2_string <- "Agonist_2_App_Time"

Glu_string <- "Glu_Gly"
GluTime_string <- "Glu_Gly_Time"

KCLapp_string <- "KCL"
KCLtime_string <- "KCL_Time"



DrugApp1_string <- "ALK_1"
DrugTime1_string <- "ALK_1_Time"

DrugApp2_string <- "ALK_2"
DrugTime2_string <- "ALK_2_Time"

Glu_string <- "Glu_Gly"
GluTime_string <- "Glu_Gly_Time"

KCLapp_string <- "KCL"
KCLtime_string <- "KCL_Time"





##DATA FRAMES FOR THE DATAFRAME FOR GRAPHS!

data_FINAL$DrugApp1 <- data_FINAL[,c(DrugApp1_string)]
data_FINAL$DrugTime1 <- data_FINAL[, c(DrugTime1_string)]

data_FINAL$DrugApp2 <- data_FINAL[, c(DrugApp2_string)]
data_FINAL$DrugTime2 <- data_FINAL[, c(DrugTime2_string)]

data_FINAL$KCLapp <- data_FINAL[, c(KCLapp_string)]
data_FINAL$KCLtime <- data_FINAL[, c(KCLtime_string)]

data_FINAL$GluApp <- data_FINAL[, c(Glu_string)]
data_FINAL$GluTime <- data_FINAL[, c(GluTime_string)]



##Remove all NA - If need be

data_FINAL <- data_FINAL %>%
  filter(!is.na(DrugApp1)) %>%
  filter(!is.na(GFP)) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(Mean_Intensity))



## Create data-frame for using in graphs
## Group data by Neuron, baseline ( F0) is calculated as average of intensity before first application
## FDelta is difference of mean intensity from baseline
## FDelta0 is alternative to normalised intensity to avoid normalisation to KCL
## Normalisation is performed through the standard (X - X0/Xmax - X0)



data_FDelta <- data_FINAL %>%
  group_by(Neuron_ID) %>%
  mutate(total_time = DrugTime1 + DrugApp1+ 480) %>%
  mutate(F0 = mean(Mean_Intensity[time <= DrugApp1], na.rm = TRUE)) %>%
  mutate(FDelta = Mean_Intensity - F0) %>%
  mutate(FDelta0 = FDelta/F0) %>%
  mutate(Normalised_Intensity = FDelta/ (max(Mean_Intensity, na.rm = TRUE) - F0)) %>%
  mutate(before_Application  = AUC(x = time, y = Normalised_Intensity, from = (x = 0), to = (x = DrugApp1 - 240))/240) %>%
  mutate(after_Application  = AUC(x = time, y = Normalised_Intensity, from = (x = DrugApp1), to = (x = DrugApp1+DrugTime1+ 480))/total_time) %>%
  mutate(delta_Application = after_Application - before_Application)




###
##ADVANCED FILTERING - - - - - - - - - - - - - - - - - - -
###
###NORMALISING DATA TO KCL PEAK AND CALCULATING COLUMNS FOR GRAPHS - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


##FIRST DERIVATIVE- CALCULATE FIRST DERIVATIVE FOR EACH NEURON TO SEE IF CAN BE USED TO DIFFERENTIATE BETWEEN SLOW AND FAST AND FOR KCL CORRECTION - - - - -

data_FDelta$Neuron_ID <- factor(data_FDelta$Neuron_ID)
data_FDelta$Agonist_Type <- factor(data_FDelta$Agonist_Type)

#FOR CERITINIB DATA
data_FDelta$VEH_OR_CERITINIB <- factor(data_FDelta$VEH_OR_CERITINIB)

#data_FDelta_Neuron$Neuron_ID <- factor(data_FDelta_Neuron$Neuron_ID)

#levels(data_FDelta_Neuron$Neuron_ID)

#Clean Data for testing, don't want all variables

data_for_derivation <- data_FDelta %>%
  dplyr::select(Neuron_ID, time, FDelta0, DrugApp1, KCLapp, KCLtime) %>%
  group_by(Neuron_ID) %>%
  filter(!is.na(FDelta0))

#Function for calculating the first derivative of the spline of the time series data



first_deriv <- function(my_data) {
  model <- lm(FDelta0~ns(time, 90), data= my_data) #make linear model, second number is the nth order polynomial - 90 seems to be good for my data
 X <- data.frame(time = seq(0, max(my_data$time)+10, by = 10)) # make an ordered sequence
  Y <- predict(model,newdata=X) # calculate predictions for that sequence
  my_data$dY <- diff(Y)/diff(X$time)  # the derivative of your function
  my_data$dX <- rowMeans(embed(X$time,2)) # x values for derivative
  return(my_data)
}



 
#SPLIT DATA PERFORM DIFF AND REJOIN

data_derived <- split(data_FDelta, data_FDelta$Neuron_ID) %>%
  lapply(first_deriv)

data_derived_rbind <- do.call(qpcR:::rbind.na, data_derived)

levels(data_for_derivation$Neuron_ID)


data_derived_FINAL <- data_derived_rbind %>%
  group_by(Neuron_ID) %>%
  arrange(dX) %>%
  mutate(time = (row_number()*10)-10) 
  #filter(time >= (DrugApp1), time <= (DrugApp1 + 300)) %>%
  #filter(dY > 0.008) %>%
  #distinct(Neuron_ID)
  
  
data_derived_FINAL_Neuron <- data_derived_FINAL %>%
  filter(Neuron_ID == "POMC_GFP_GLP_1_200nM_29_03_23_p3_Neuron_7")


  #PLOT TO TEST IF WORKED
  

data_derived_FINAL_Neuron_max <- data_derived_FINAL_Neuron %>%
  filter(time > Glu_Gly + 200) %>%
  mutate(maxdY = max(dY, na.rm = TRUE)) %>%
  filter(maxdY == dY)

spline_plot <-  ggplot(data_derived_FINAL_Neuron, aes(dX,dY)) + 
  geom_line() +
  geom_vline(xintercept = data_derived_FINAL_Neuron_max$dX, color = "red", linetype = "dashed") + 
    theme_bw()
  facet_wrap_paginate(~Neuron_ID, nrow = 5, ncol = 5,  scales = "free")




##CORRECT FOR KCL PEAK LATENCY SO TIME COURSE ALIGNS FOR ALL PLATES


data_KCL_corrected <- data_derived_FINAL %>%
  group_by(Neuron_ID) %>%
  filter(time > KCLapp & time < KCLapp + KCLtime + 180) %>%
  filter(dY == max(dY)) %>%
  mutate(KCL_time = time, #Find the onset of peak of KCL
         KCL_diff = KCL_time - (KCLapp + 90)
         ) %>%
  dplyr::select(Neuron_ID, KCL_diff)


#### Finally, merge data_FDelta with KCL_corrected to apply KCL correction

#Shifting Agonist Applied Time

data_KCL_combined <- merge(data_KCL_corrected, data_FDelta) %>%
  group_by(Neuron_ID) %>%
  mutate(Agonist_1 = (Agonist_1 + KCL_diff)) 

#Shifting Entire Trace - LOSES TRACES AT BEGGINING 

data_KCL_combined <- merge(data_KCL_corrected, data_FDelta) %>%
  group_by(Neuron_ID) %>%
  mutate(time = (time - KCL_diff)) %>%
  filter(time > -1)



##Excel file of Fast Responders/Slow Responders/Inhibitory Responders


Slow_responders <- read_excel("C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/Slow_responders.xlsx")

Slow_responders <- Slow_responders[,1]

Slow_Responders_FDelta <- merge(Slow_responders, data_FDelta, by = "Neuron_ID") %>%
  filter(Neuron_ID != "POMC_GFP_Tirz_200nM_11_05_23_p3_Neuron_2")

Slow_Responders_FDelta$Neuron_ID <- as.factor(Slow_Responders_FDelta$Neuron_ID)

Slow_Responders_FDelta$GFP <- as.factor(Slow_Responders_FDelta$GFP)

levels(Slow_Responders_FDelta$Neuron_ID)


data_FDelta %>%
  filter(time == 0) %>%
  #group_by(Agonist_Type) %>%
  count(GFP)

Inhibitory_Responders_FDelta %>%
 filter(GFP == "No") %>%
 distinct(Neuron_ID, Agonist_Type) %>%
  count(Agonist_Type)




Fast_Responders <- read_excel("C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/Fast_Responders.xlsx")

Fast_Responders_FDelta <- merge(Fast_Responders, data_FDelta, by = "Neuron_ID") %>%
  filter(Neuron_ID != "POMC_GFP_Lira_200nM_11_05_23_p1_Neuron_15") %>%
  filter(Neuron_ID != "POMC_GFP_72H-Veh_200nM_GLP-1_23_03_23_p2_Neuron_7") %>%
  filter(Neuron_ID != "POMC_GFP_Sema_200nM_30_03_23_p1_Neuron_20")
  

  
Fast_Responders_FDelta$Neuron_ID <- as.factor(Fast_Responders_FDelta$Neuron_ID)

levels(Fast_Responders_FDelta$Neuron_ID)



Inhibitory_responders <- read_excel("C:/Users/olikc/Desktop/MPhil Research/Calcium_Imaging/Inhibitory_Responders.xlsx")

Inhibitory_Responders_FDelta <- merge(Inhibitory_responders, data_FDelta, by = "Neuron_ID")

Inhibitory_Responders_FDelta$Neuron_ID <- as.factor(Inhibitory_Responders_FDelta$Neuron_ID)

Inhibitory_Responders_FDelta$GFP <- as.factor(Inhibitory_Responders_FDelta$GFP)

levels(Inhibitory_Responders_FDelta$Neuron_ID)


data_check_slow <-  which(Slow_responders$Neuron_ID %in% data_t_test_filtered$Neuron_ID)
  
  
data_check_fast <-  which(Fast_Responders$Neuron_ID %in% data_t_test_filtered$Neuron_ID)


data_check_inhib <-  which(Inhibitory_responders$Neuron_ID %in% data_t_test_filtered$Neuron_ID)

###
### FILTERING BASED ON P VALUE OF RESPONSE BEFORE AND AFTER APPLICATION 
###


data_FDelta <- data_FINAL %>%
  group_by(Neuron_ID) %>%
  mutate(total_time = 300) %>%
  mutate(F0 = mean(Mean_Intensity[time <= DrugApp1 & time > DrugApp1 - 240], na.rm = TRUE)) %>%
  #mutate(F0 = mean(Mean_Intensity[time <= DrugApp1], na.rm = TRUE)) %>%
  mutate(FDelta = Mean_Intensity - F0) %>%
  mutate(FDelta0 = FDelta/F0) %>%
  mutate(Normalised_Intensity = FDelta/ (max(Mean_Intensity, na.rm = TRUE) - F0)) %>%
  mutate(before_Application  = AUC(x = time, y = Normalised_Intensity, from = (x = DrugApp1-240), to = (x = DrugApp1))/240) %>%
  mutate(after_Application  = AUC(x = time, y = Normalised_Intensity, from = (x = DrugApp1 + DrugTime1), to = (x = DrugApp1+DrugTime1+ 300))/300) %>%
  mutate(delta_Application = after_Application - before_Application)



data_before_for_t_test <- data_FDelta %>%
  group_by(Neuron_ID) %>%
  filter(time > DrugApp1 - 300 & time <= DrugApp1) %>%
  mutate(Grouping = "Before") %>%
  dplyr::select(Neuron_ID, Grouping, FDelta0)


data_after_for_t_test <- data_FDelta %>%
  group_by(Neuron_ID) %>%
  filter(time > (DrugApp1 + DrugTime1) & time <= (DrugApp1 + DrugTime1 + 300)) %>%
  mutate(Grouping = "After") %>%
  dplyr::select(Neuron_ID, Grouping, FDelta0)


levels(data_after_for_t_test$Neuron_ID)


data_before_and_after_for_t_test <- rbind(data_before_for_t_test, data_after_for_t_test)


data_before_and_after_for_t_test$Grouping <- as.factor(data_before_and_after_for_t_test$Grouping)


dbfaf_split <- data_before_and_after_for_t_test %>%
  group_by(Neuron_ID) %>%
  mutate(pvalue = t.test(FDelta0 ~ Grouping, paired = F)$p.value) %>%
  dplyr::select(Neuron_ID, pvalue) %>%
  filter(pvalue < 0.0001)

dbfaf_split <- dbfaf_split %>%
  distinct(Neuron_ID,pvalue)

data_t_test_filtered <- merge(dbfaf_split, data_FDelta , by = "Neuron_ID")




data_for_boxplots <- data_t_test_filtered %>%
  group_by(Neuron_ID) %>%
  #dplyr::select(before_Application, after_Application, Neuron_ID, GFP, delta_Application, Agonist_Type, GFP_Intensity,Glutamate) %>% #COMPARING GLP-1 AGONISTS
  dplyr::select(before_Application, after_Application, Neuron_ID, GFP, delta_Application, VEH_OR_CERITINIB, GFP_Intensity) %>% #COMPARING CERITINIB DATA
  filter(GFP != "NN") %>%
  filter(GFP != "N/A") %>%
  #filter(Glutamate != "Yes") %>%
  filter(row_number() == 1) %>%
  pivot_longer(before_Application:after_Application, names_to = "Before_or_After", values_to = "Response") 
 

data_for_boxplots$Before_or_After <- factor(data_for_boxplots$Before_or_After, levels=c("before_Application", "after_Application"))
#POMC_GFP_24H_Veh_Sema_200nM_07_07_23
#POMC_GFP_24H_ALK_Sema_200nM_07_07_23


##ATTEMPTING TO REMOVE NEW DATA TO SEE IF IT INFLUENCES ANALYSIS

neuron_names <- data.frame(data_for_boxplots$Neuron_ID)



####
#### GRAPHS - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
####


## Sanity check for GFP to make sure no misspellings
data_FDelta$GFP <- as.factor(data_FDelta$GFP)
levels(data_FDelta$Neuron_ID)

#dataframe for rectangles; important as ggplot2 creates 1 layer for every observation thus
#you only want the rectangles to be drawn once for every plot, a bit of a fudge but fuck it lol

data_for_rectangles <- data_FDelta  %>%
  filter(time == 0) 

#DrugApp2 <- data_for_rectangles[, c("Neuron_ID","")]
#DrugTime2 <- data_for_rectangles[, c("Neuron_ID","")]


##DATASET FOR SELECTING INDIVIDUAL NEURONS - - - - - - - - - - -

data_FDelta_Neuron <- data_FDelta %>%
  #filter(Neuron_ID == "POMC_GFP_SEMA_1_2_2023_p1_f1_Neuron_2" | Neuron_ID == "POMC_GFP_GLP-1_200nM_12_05_23_p4_Neuron_77") %>%
  filter(Neuron_ID == "POMC_GFP_ORX2A_200nM_12_05_23_p1_Neuron_19") %>%
  filter(if_any(Mean_Intensity, ~ !is.na(.))) %>%
  filter()

data_for_rectangles_Neuron <- data_FDelta_Neuron[1,]

16/24

47/354



##NEURONS USED FOR EXEMPLARY TRACES


# POMC_GFP_SEMA_1_2_2023_p1_f1_Neuron_2 = GOOD BUMP RESPONDER
# POMC_GFP_24H_Veh_200nM_Sema_04_5_23_p3_Neuron_32
# POMC_GFP_Sema_200nM_30_03_23_p3_Neuron_18 = EXAMPLE NON RESPONDER 
# POMC_GFP_SEMA200nM_20_12_2022_p1_f1_Neuron_6 = GOOD LONG RESPONDER 
# POMC_GFP_SEMA_1_2_2023_p3_f1_Neuron_3 = GOOD NEGATIVE RESPONDER
#POMC_GFP_24H_Vehicle_200nM_Sema_10_5_23_p2_Neuron_1 = SEMA DOUBLE RESPONDER


#POMC_GFP_Tirz_200nM_31_03_23_p1_Neuron_11 = GOOD RESPONDER TIRZ
#POMC_GFP_Tirz_200nM_31_03_23_p1_Neuron_3 = TIRZ POTENTIAL FAST PLUS SLOW
#POMC_GFP_Tirz_200nM_11_05_23_p4_Neuron_7 = TIRZ DOUBLE
#POMC_GFP_Tirz_200nM_31_03_23_p3_Neuron_14 = TIRZ INHIBITORY

#POMC_GFP_Lira_200nM_11_05_23_p1_Neuron_36 = Lira GOOD RESPONDER
#POMC_GFP_Lira_200nM_11_05_23_p1_Neuron_38 = LIRA GOOD INHIBITORY
#POMC_GFP_Lira_200nM_11_05_23_p2_Neuron_30 = LIRA GOOD INHIBITORY
#POMC_GFP_Lira_200nM_11_05_23_p1_Neuron_28 = Bump responder?
#POMC_GFP_Lira_200nM_11_05_23_p1_Neuron_29 = Lira GOOD NO DOUBLE
#POMC_GFP_Lira_200nM_11_05_23_p1_Neuron_6

#GLP_1_200nM_16_02_2023_p2_f1_Neuron_2 = GLP-1 GOOD RESPONDER
#GLP_1_200nM_16_02_2023_p2_f1_Neuron_3 = GLP-1 Inhibitory
#POMC_GFP_GLP-1_200nM_12_05_23_p4_Neuron_77 = Fast and Inhibitory Response?

#GLP_1_200nM_14_02_23_p2_f1_Neuron_5 = Example of Discarded Trace

#POMC_GFP_ALK_300nM_02_03_p1_f1_Neuron_50 = Double ALK admin
#POMC_GFP_ORX2A_50nM_12_05_23_p2_Neuron_18 = ORX2A Double Response
#GLP_1_20_nM_23_02_23_p2_f1_Neuron_3
#POMC_GFP_24H_Veh_Sema_200nM_07_07_23_p6_Neuron_5

## GRAPH CONTAINING ALL TRACES - - - - - - - - - - - - - -
## combined_plot is the ggplot object, in order to have the traces be more legible
## I limit the output for a 5 x 5 column, the for loop after this creates each plot and saves it to downloads
## If want to change how many graphs in each plot, change nrow = and ncol = in "facet_wrap_paginate both in gg object and for loop


levels(data_FDelta$Neuron_ID)

combined_plot <- ggplot(data = data_FDelta, aes(x = time, y = Normalised_Intensity, color = GFP)) +
  geom_line() +
  geom_rect(data = data_for_rectangles, xmin = data_for_rectangles$DrugApp1, xmax = (data_for_rectangles$DrugApp1 + data_for_rectangles$DrugTime1), ymin =0, ymax = 1,
            alpha = .25, fill = 'blue', color = NA) +
  geom_rect(data = data_for_rectangles, xmin = data_for_rectangles$KCL, xmax = (data_for_rectangles$KCL + data_for_rectangles$KCLtime), ymin =0, ymax = 1,
            alpha = .25, fill = 'red', color = NA) +
  geom_rect(data = data_for_rectangles, xmin = data_for_rectangles$DrugApp2, xmax = (data_for_rectangles$DrugApp2 + data_for_rectangles$DrugTime2), ymin =0, ymax = 1,
            alpha = .25, fill = 'blue', color = NA) +
 # geom_rect(data = data_for_rectangles, xmin = data_for_rectangles$GluApp, xmax = (data_for_rectangles$GluApp + data_for_rectangles$GluTime), ymin =0, ymax = 1,
  #          alpha = .25, fill = 'green', color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") +
   theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size=2),
    axis.title = element_text(size = "20"),
    legend.box.background=element_rect(fill="white", color="black"),
    legend.background = element_blank(),
    #legend.position = c(0.6,0),
    legend.position = "none",
    legend.justification = c("left","bottom"),
    legend.key.width = unit(7, "cm"),
    legend.key.height = unit(0.05, "cm"),
    strip.text = element_text(size = 7),
    legend.text = element_text(size = "15")) +
    
  ylab("Normalised Intensity") +
  xlab("Time (s)") +
 facet_wrap_paginate(~Neuron_ID, ncol = 7, nrow = 7,scales = "free")



##For loop to save combined plots

for(j in 1:n_pages(combined_plot)){
p_save <- combined_plot + 
    facet_wrap_paginate(~ Neuron_ID, ncol = 5, nrow = 5, page = j)
  ggsave(plot = p_save, filename = paste0('C:/Users/olikc/Downloads/', j,"_ALL_GFLP_1", '.jpg'),  width = 18, height = 8, units = "in")
}

dev.size(units = c("in"))


##GRAPH FOR SINGLE NEURON TRACES - - - - - - - - - - - - - -

subset(data_FDelta_Neuron, time > DrugApp1-300 & time < DrugApp1+DrugTime1+300)%>%
plot_4 <- ggplot(data = data_FDelta_Neuron, aes(x = time, y = FDelta0)) +
  geom_line(size = 1) +
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, degree = 90), se = FALSE) +
  geom_rect(data = data_for_rectangles_Neuron, xmin = data_for_rectangles_Neuron$DrugApp1, xmax = (data_for_rectangles_Neuron$DrugApp1 + data_for_rectangles_Neuron$DrugTime1), ymin =-1, ymax = 7,
            alpha = .25, fill = 'blue', color = NA) +
  geom_rect(data = data_for_rectangles_Neuron, xmin = data_for_rectangles_Neuron$KCL, xmax = (data_for_rectangles_Neuron$KCL + data_for_rectangles_Neuron$KCLtime), ymin =-1, ymax = 7,
            alpha = .25, fill = 'red', color = NA) +
  geom_rect(data = data_for_rectangles_Neuron, xmin = data_for_rectangles_Neuron$DrugApp2, xmax = (data_for_rectangles_Neuron$DrugApp2 + data_for_rectangles_Neuron$DrugTime2), ymin =-1, ymax = 7,
            alpha = .25, fill = 'blue', color = NA) +
 # geom_rect(data = data_for_rectangles_Neuron, xmin = data_for_rectangles_Neuron$GluApp, xmax = (data_for_rectangles_Neuron$GluApp + data_for_rectangles_Neuron$GluTime), ymin =-1, ymax = 7,
  #          alpha = .25, fill = 'green', color = NA) +
 #geom_ribbon(data=subset(data_FDelta_Neuron, time > DrugApp1-300 & time < DrugApp1+DrugTime1+300), aes(x=time,ymax=FDelta0),ymin=0,alpha=0.3, fill = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title = element_text(size = "25"),
    legend.box.background=element_rect(fill="white", color="black"),
    legend.background = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    #legend.position = c(0.6,0),
    legend.position = "none",
    legend.justification = c("left","bottom"),
    legend.key.width = unit(7, "cm"),
    legend.key.height = unit(0.05, "cm"),
    strip.text = element_text(size = 7),
    legend.text = element_text(size = "15"),
    axis.text = element_text(size = "17"),
    plot.title = element_text(hjust = 0.5, size = 19, vjust = 3),
    plot.tag = element_text(size = 18, face = "bold")) +
  scale_x_continuous(breaks = round(seq(min(data_FDelta_Neuron$time), max(data_FDelta_Neuron$time), by = 300),1)) +
  #ylab("Normalised Intensity") +
  ylab(expression("\u0394F/F"["0"])) +
  xlab("Time (s)") +
  #labs(title = "Accepted Trace", tag = "B")
  ggtitle(label = "200nM ORX2A")


plot_grid(plot_1, plot_2, plot_3, plot_4, labels = "", nrow = 2)





##AVERAGE TRACES - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


#Make a fraction of neurons to ease visualisation for average trace
#sample_neurons <- data_long_GFP_1 %>%
sample_neurons <- Slow_responders %>%
  sample_frac(size = 0.4)


data_for_neuron_lines <- merge(data_for_traces, sample_neurons, by = "Neuron_ID") %>%
  filter(Normalised_Intensity < 0.3 & Normalised_Intensity > -0.1) 


##Create Data frame for the time series data


data_for_traces <- data_t_test_filtered %>%
  group_by(Neuron_ID) %>%
  filter(time >= (DrugApp1 - 300)) %>%
  mutate(F0 = mean(Mean_Intensity[time <= DrugApp1 & time >= DrugApp1 - 60], na.rm = TRUE)) %>%
  mutate(FDelta = Mean_Intensity - F0) %>%
  mutate(FDelta0 = FDelta/F0) %>%
  mutate(Normalised_Intensity = FDelta/ (max(Mean_Intensity, na.rm = TRUE) - F0)) %>%
  mutate(AppDiff = DrugApp2 - DrugApp1) %>%
 #filter(AppDiff > 1500) %>%
  #filter(time >= (DrugApp1 - 260), time <= (DrugApp1 + 600)) %>%
  filter(time >= (DrugApp1 - 300), time <= (DrugApp1 + 550)) %>%
  arrange(time) %>%
  mutate(time = (row_number()*10)-10) %>%
  filter(GFP != "NN") %>%
  filter(delta_Application > 0)











data_for_traces_1_Agonist <- data_for_traces %>%
  filter(Agonist_Type == "Sema")


#Data frame for Normalised Intensity following application for GFP+ and GFP-ve cells

after_application_hlines <- data_for_traces %>%
  group_by(GFP) %>%
  mutate(after_Application_GFP = mean(Normalised_Intensity[time >= 500], na.rm = TRUE)) %>%
  dplyr::select(GFP, after_Application_GFP) %>%
  distinct(after_Application_GFP)

hlines_c = as.numeric(c(after_application_hlines[2,2],after_application_hlines[1,2]))


#COUNT NUMBER OF GFP POSITIVE CELLS - - - - - - - - - - - 

# These are needed for labelling plots with number of GFP+ Cells, dependent on filtering

data_for_traces_count <- data_for_traces %>%
  filter(time == 0) %>%
  group_by(GFP) %>%
  count(GFP)

GFP_Neuron_Count_str <-as.character(c(data_for_traces_count[2,2],data_for_traces_count[1,2]))


#Data frame containing number of GFP cells/Normalised Intensity and the label 

dat_text <- data.frame(
  label = c(paste("n = ",GFP_Neuron_Count_str[2]),paste("n = ",GFP_Neuron_Count_str[1])),
  GFP = c("No", "Yes")
)

dat_hline <- data.frame(
  intercept = hlines_c,
  label =  round(hlines_c, 3),
  GFP = c("No", "Yes")
)


#PLOT ITSELF - - - - REGULAT VERSION

   

##VERSION FOR COMPARING AGONISTS IN ONE PLOT - - - - - - - - - - - - - - - - - - - - - - - -- 


# These are needed for labelling plots with number of GFP+ Cells

data_for_traces_count <- data_for_traces %>%
  filter(time == 0) %>%
  group_by(GFP, VEH_OR_CERITINIB) %>%
  count(GFP)


#Data frame for Normalised Intensity following application for GFP+ and GFP-ve cells

after_application_hlines <- data_for_traces %>%
  group_by(GFP, VEH_OR_CERITINIB) %>%
  mutate(after_Application_GFP = mean(Normalised_Intensity[time >= 500], na.rm = TRUE)) %>%
  dplyr::select( after_Application_GFP, VEH_OR_CERITINIB) %>%
  distinct(GFP, after_Application_GFP) %>%
  merge(data_for_traces_count)

hlines_c = as.numeric(after_application_hlines[,3])


#Data frames containing number of GFP cells/Normalised Intensity and the label


GFP_Neuron_Count_str <- after_application_hlines$n
Agonist_Types <- after_application_hlines$VEH_OR_CERITINIB


label_combined <- c()
GFP_combined <- c()
Agonist_combined <- c()

for (i in 1:length(GFP_Neuron_Count_str)){
  j = c(paste("n = ",as.character(GFP_Neuron_Count_str[i])))
  x = as.character(after_application_hlines[i,1])
  y <- as.character(after_application_hlines[i,2])
  label_combined <- c(label_combined, j)
  GFP_combined <- c(GFP_combined, x)
  Agonist_combined <- c(Agonist_combined, y)
}

dat_text <- data.frame(
  label = label_combined,
  GFP = GFP_combined,
  Agonist_Type = Agonist_combined
)

dat_hline <- data.frame(
  intercept = hlines_c,
  label =  round(hlines_c, 3),
  GFP = GFP_combined, 
  Agonist_Type = Agonist_combined
)

chisq
##PLOT ITSELF - - - - - - --

data_for_traces$Neuron_ID <- as.factor(data_for_traces$Neuron_ID)
data_for_traces$GFP <- as.factor(data_for_traces$GFP)
levels(data_for_traces$Neuron_ID)
levels(data_for_traces$GFP)
data_for_traces <- data_for_traces %>%
  filter(GFP != "NN")




plot_trace_2 <- ggplot(data = subset(data_for_traces, !is.na(GFP)), aes(x = time, y = Normalised_Intensity)) +
  geom_line(data = data_for_traces, aes(x = time, y = Normalised_Intensity, group = Neuron_ID), alpha = 1, color = "grey") +
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, 
               alpha = 1, fill = "light blue", linetype = "solid", fun.args=(conf.int=0.683), color = "purple") +
  stat_summary(geom = "line", fun.y = mean, size = 1.2) +
  #geom_smooth(aes(y=Normalised_Intensity), alpha=0.5, color = "purple") +
  annotate(geom = "rect", xmin = 300, xmax = 420, ymin = -0.1, ymax = 0.3,
           fill = "blue", colour = "transparent", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
#  geom_hline(data = dat_hline, aes(yintercept = intercept), linetype = "dashed", size = 1, color = "red") +
  theme(
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "20"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 17),
    plot.title = element_text(hjust = 0.5, size = 19),
    legend.position = "none") +
  ylab("Normalised Intensity (AU)") +
  xlab("Time (s)") +
  coord_cartesian(ylim=c(-0.15,0.40)) + 
  #facet_wrap(~GFP) +
  geom_text(
   size    = 5,
    data    = dat_text,
    mapping = aes(x = 120, y = -0.04, label = label, color = GFP)) +
  geom_text(
    size    = 4,
    data    = dat_hline,
    mapping = aes(x = 100, y = label + 0.025, label = label),
    color = "red") +
#  geom_text(
 #   size    = 7,
  #  data    = dat_hline,
   # mapping = aes(x = 300, y = 0.3, label = "*"),
  #  color = "red") +
  #scale_y_continuous(limits = c(-0.1,0.4)) +
  #ggtitle(label = "Average Trace Exd3") +
  facet_wrap_paginate(~VEH_OR_CERITINIB+GFP, nrow = 1, ncol = 4) +
  scale_color_manual(breaks = c("No","Yes"),
                     values=c("black", "green")) 





## - - - - - - - -- - - - - - - - - - - - -
## BOXPLOTS AND DISTRIBUTION PLOTS - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - -


##GENERATING MEAN AVERAGES FOR DISTRIBUTION PLOT
data_for_vlines <- data_for_boxplots %>%
  group_by(VEH_OR_CERITINIB) %>%
  mutate(mean_delta = mean(delta_Application)) %>%
  dplyr::select(mean_delta) %>%
  distinct(mean_delta)

vlines_c = as.numeric(c(data_for_vlines[1,2],data_for_vlines[2,2]))

##DATA FRAME FOR DISTRIBUTION PLOT

dat_vline <- data.frame(
  intercept = vlines_c[1],
  label =  c(paste("\u03BC =",round(vlines_c[1],5))),
)



##DISTRIBUTION PLOT

subset(data_for_boxplots, !is.na(GFP)) %>%
  ggplot(aes(x = delta_Application)) +
  geom_histogram(aes(y = after_stat(density)), fill = "light blue", color = "black", binwidth = 0.0075) +
  geom_density( size = 1, color = "blue") +
  geom_vline(data = dat_vline, aes(xintercept = intercept), linetype = "dashed", color = "red", size = 1.2) +
  geom_text(
    size    = 4,
    data    = dat_vline,
    mapping = aes(x = 0.07, y = 15, label = label),
    color = "red") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1, color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 15)
  ) +
  ylab(label = "Density") +
  xlab(label = "\u0394 AUC") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-0.125,0.125)) +
  ggtitle("Distribution of \u0394AUC from GLP-1 Agonist Application")



##DISTRIBUTION PLOT - GFP 

mean(data_for_boxplots_positive$delta_Application)

  ggplot(data = data_for_boxplots_positive, aes(x = delta_Application)) +
  geom_histogram(aes(y = after_stat(density)), fill = "light blue", color = "black") +
  geom_density( size = 1, color = "blue") +
  #geom_vline(data = dat_vline, aes(xintercept = intercept), linetype = "dashed", color = "red", size = 1.2) +
#  geom_text(
 #   size    = 4,
  #  data    = dat_vline,
   # mapping = aes(x = 0.07, y = 15, label = label),
  #  color = "red") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1, color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 15)
  ) +
  ylab(label = "Density") +
  xlab(label = "GFP Mean Intensity") +
  #scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  #scale_x_continuous(limits = c(-0.125,0.125)) +
  ggtitle("Distribution of GFP+ Intensity")
  
  

## BOXPLOT - - - - - - - - - - - - - - - - - - - - - - -

  library('ggbeeswarm')
  
  #If wanna use IQR use func below
  iqr = function(z, lower = 0.25, upper = 0.75) {
    data.frame(
      y = median(z),
      ymin = quantile(z, lower),
      ymax = quantile(z, upper)
    )
  }

  
  
  data_FDelta <- data_FINAL %>%
    group_by(Neuron_ID) %>%
    mutate(total_time = 500) %>%
    mutate(F0 = mean(Mean_Intensity[time <= DrugApp1 & time > DrugApp1 - 240], na.rm = TRUE)) %>%
    #mutate(F0 = mean(Mean_Intensity[time <= DrugApp1], na.rm = TRUE)) %>%
    mutate(FDelta = Mean_Intensity - F0) %>%
    mutate(FDelta0 = FDelta/F0) %>%
    mutate(Normalised_Intensity = FDelta/ (max(Mean_Intensity, na.rm = TRUE) - F0)) %>%
    mutate(before_Application  = AUC(x = time, y = FDelta0, from = (x = DrugApp1-240), to = (x = DrugApp1))/240) %>%
    mutate(after_Application  = AUC(x = time, y = FDelta0, from = (x = DrugApp1 + DrugTime1), to = (x = DrugApp1+DrugTime1+ 500))/500) %>%
    mutate(delta_Application = after_Application - before_Application)
  
  
  data_for_boxplots_no_filter <- data_FDelta %>%
    group_by(Neuron_ID) %>%
    #dplyr::select(before_Application, after_Application, Neuron_ID, GFP, delta_Application, Agonist_Type, GFP_Intensity,Glutamate) %>% #COMPARING GLP-1 AGONISTS
    dplyr::select(before_Application, after_Application, Neuron_ID, GFP, delta_Application, VEH_OR_CERITINIB, GFP_Intensity) %>% #COMPARING CERITINIB DATA
    filter(GFP != "NN") %>%
    filter(GFP != "N/A") %>%
    #filter(Glutamate != "Yes") %>%
    filter(row_number() == 1) %>%
    pivot_longer(before_Application:after_Application, names_to = "Before_or_After", values_to = "Response") 
  
  
  data_for_boxplots$Before_or_After <- factor(data_for_boxplots$Before_or_After, levels=c("before_Application", "after_Application"))

 ggplot(data = subset(data_for_boxplots, !is.na(GFP)), aes(x = Before_or_After, y = Response, fill = GFP)) +
  #geom_boxplot(outlier.shape = NA, fill = "white", geom = "errobar") +
  #geom_jitter(position = position_dodge(width = .75), alpha = 0.3) +
  geom_quasirandom(aes(color = GFP), alpha = 1, height = 0, width = 0.25, color = "black", size = 3, pch = 21, cex = 3, bandwidth = 0.7, dodge.width = 0.25) +
  #geom_line(aes(group = Neuron_ID), color = 'blue', alpha = 0.1) +
  stat_summary(fun.min = function(z) {mean(z) - sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z))},
                fun.max = function(z) {mean(z) + (sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z)))},
                fun = mean, linewidth = 1, color = "red",
               geom = "errorbar", 
               width = 0.64) +
  theme_bw() +
  theme( #legend.background = element_rect(fill = "light grey"), 
         #legend.text = element_text(size = "12"),
         legend.key = element_blank(),
         legend.text = element_blank(),
         legend.position = "none",
         #legend.box.background = element_rect(fill = "black", linewidth = 1),
         legend.box.background = element_blank(),
         axis.text = element_text(size = 14),
         axis.title.y = element_text(size = 20),
         strip.text = element_blank(),
         strip.background = element_blank(),
         panel.background = element_blank(),
         panel.border = element_blank(),
         panel.grid = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, alpha = 0.25) +
  ylab("Average AUC") +
  xlab("") +
  scale_fill_manual(name="GFP",
                    labels=c("No","Yes"),
                   values=c("light gray","green")) +
   #facet_wrap(~GFP) +
  # scale_color_manual(name="GFP",
   #                   labels=c("No","Yes"),
    #                  values=c("light gray","green")) +
  scale_x_discrete(labels = c("Baseline","After Application")) +
  facet_wrap_paginate(~VEH_OR_CERITINIB + GFP, nrow = 1) +
  scale_y_continuous(limits = c(-0.1, 0.3)) +
  ggtitle(label = "Change in AUC from Semaglutide 200nM  FILTERED")

 
 
 
 data_FDelta %>%
   filter(time == 0) %>%
   group_by(time = 0) %>%
   count(GFP,VEH_OR_CERITINIB)

 data_t_test_filtered %>%
   filter(time == 0) %>%
   group_by(time = 0) %>%
   count(GFP,VEH_OR_CERITINIB)
 
 

 
 
 
 
 
 
 ## BOXPLOT - HAVING JUST GFP IN ONE ROW 
 
 
 
 plot_2 <- ggplot(data = subset(data_for_boxplots, !is.na(GFP), subset = GFP == "Yes"), aes(x = Before_or_After, y = Response)) +
   geom_boxplot(outlier.shape = NA, fill = "green") +
   geom_jitter(position = position_dodge(width = .75), alpha = 0.3) +
   geom_line(aes(group = Neuron_ID), color = 'blue', alpha = 0.15) +
   theme_bw() +
   theme( legend.background = element_rect(fill = "green"), 
          legend.text = element_text(size = "12"),
          legend.key = element_blank(),
          legend.box.background = element_rect(fill = "black", linewidth = 1),
          axis.text = element_text(size = 14),
          axis.title.y = element_blank(),
          #axis.title.y = element_text(size = 18),
          strip.text = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.background = element_blank()
   ) +
   geom_hline(yintercept = 0, linetype = "dashed", size = 1, alpha = 0.25) +
   ylab("AUC (AU)") +
   xlab("") +
   # scale_fill_manual(name="GFP",
   #                  labels=c("No","Yes"),
   #                 values=c("light gray","green")) +
   scale_x_discrete(labels = c("Before","After")) +
   facet_wrap_paginate(~Agonist_Type, ncol = 1) +
   scale_y_continuous(limits = c(-0.1, 0.1)) +
   ggtitle(label = "")
 
 
 
 plot_grid(plot_trace_1, plot_trace_2, labels = "")
 
 
 
 
 
 ## - - - - - - - - - - - - - - - - - - - - - - -
 ## STATISTICS 
 ## - - - - - - - - - - - - - - - - - - - - - - - -
 
 ##DATA FOR ANALYSING DELTA AUC - HAVE TO MAKE SURE ONLY 1 OBSERVATION PER NEURON
 
 
 data_for_model <- data_for_boxplots %>%    #FOR GLP-1 AGONIST DATA
   dplyr::select(Before_or_After, GFP, Neuron_ID,Agonist_Type,GFP_Intensity, Response, delta_Application) %>%
   distinct(Before_or_After, Neuron_ID,GFP, Neuron_ID, Agonist_Type, GFP_Intensity, Response, delta_Application) %>%
   filter(Before_or_After == "after_Application") %>%
   filter(Agonist_Type != "Liraglutide") %>%
   filter(delta_Application < 0) 
 
 data_for_model <- data_for_boxplots %>%    #FOR CERITINIB DATA
   dplyr::select(Before_or_After, GFP, Neuron_ID,VEH_OR_CERITINIB,GFP_Intensity, Response, delta_Application) %>%
   distinct(Before_or_After, Neuron_ID,GFP, Neuron_ID, VEH_OR_CERITINIB, GFP_Intensity, Response, delta_Application) %>%
   filter(Before_or_After == "after_Application") %>%
   filter(delta_Application < 0) 
 
 
 data_for_chisq_negative <- data_for_model %>%
   group_by(VEH_OR_CERITINIB, GFP) %>%
   count()

 
 data_FDelta %>%
   filter(time == 100) %>%
   group_by(VEH_OR_CERITINIB, GFP) %>%
   count()
 
 
 data_for_model %>%
   group_by(VEH_OR_CERITINIB, GFP) %>%
   count()
 
 
53/111 #Ceritinib GFP NO
 16/28#Ceritinib GFP YES
 59/122#Vehicle GFP NO
 21/28#Vehicle GFP YES
 
 
 
 data_for_chisq_total <- data_for_model %>%
   filter(GFP != "Yes") %>%
   group_by(Agonist_Type) %>%
   count() %>%
   mutate(Type = "Total")
 
 
 data_for_chisq_total[2,2] <- 134
 
 
 data_for_chisq[2,2] <- 14
 
 data_for_chisq <- rbind(data_for_chisq_negative, data_for_chisq_total)
 
chi.s

 M <- as.table(rbind(c(36, 15, 5), c(101, 134, 42)))
 dimnames(M) <- list(gender = c("Negative", "Total"),
                     party = c("GLP-1","Semaglutide", "Tirzepatide"))
 
 (Xsq <- chisq.test(M)) 
 
 chisq.test(M[, c(2, 3)], correct = TRUE)
 
 data_for_model$GFP <- as.factor(data_for_model$GFP)
 
 data_for_chi_square <- data_for_model %>%
   group_by(GFP) %>%
   count(GFP) %>%
   cbind(GFP_Neuron_Count[,2])
 
 data_for_chi_square <- data_for_chi_square[,2:3]
 
 colnames(data_for_chi_square)[1] <- "Filtered"
 colnames(data_for_chi_square)[2] <- "Total"
 
 cont_table <- as.table(rbind(c(230,33), c(967,69)))
 dimnames(cont_table) <- list(Type = c("Filtered", "Total"),
                              GFP = c("No", "Yes"))
 Xsq <- chisq.test(cont_table, correct = FALSE)
 
 chisq()
 
 
 
 data_for_model <- data_for_boxplots %>%    #FOR CERITINIB DATA
   dplyr::select(delta_Application, GFP, Neuron_ID,VEH_OR_CERITINIB,GFP_Intensity) %>%
   distinct(delta_Application, Neuron_ID,GFP, Neuron_ID, VEH_OR_CERITINIB, GFP_Intensity)
   
 
 data_for_model$GFP <- as.factor(data_for_model$GFP)
 data_for_model$VEH_OR_CERITINIB <- as.factor(data_for_model$VEH_OR_CERITINIB)
 
 ##LINEAR MODELS - AGONISTS GLP-1

model_both <- glm(data = data_for_model, delta_Application ~ GFP*Agonist_Type)

model_GFP <- lm(data = data_for_model, delta_Application ~ GFP_Intensity)

model_Agonist_Type <- lm(data = data_for_model, delta_Application ~ Agonist_Type)

model_both_no_interaction <- lm(data = data_for_model, delta_Application ~ Agonist_Type + GFP)


library('simr')




##LINEAR MODELS - CERITNIB DATA

model_application_both <- glm(data = data_for_model, delta_Application ~ GFP*VEH_OR_CERITINIB)

model_application_GFP <- lm(data = data_for_model, delta_Application ~ GFP_Intensity)

model_application_Agonist_Type <- lm(data = data_for_model, delta_Application ~ VEH_OR_CERITINIB)

model_both_no_interaction <- lm(data = data_for_model, delta_Application ~ VEH_OR_CERITINIB + GFP)



data_for_model %>%
  cohens_d(delta_Application ~ Agonist_Type + GFP, var.equal = TRUE)


mean(data_for_boxplots$delta_Application)
sd(data_for_boxplots$delta_Application)

###
### PLAYING AROUND WITH MONTE CARLO POWER ANALYSIS
###

true_data <- data_for_boxplots %>%
  filter(GFP == "No") %>%
  mean(delta_Application)

true_data <- subset(data_for_boxplots, GFP == "Yes")
true_data_delta <- true_data$delta_Application



significant_vector <- 0
not_significant_vector <- 0
sample_size <- c()
probability_vector <- c()
for (j in seq(10,500,10)) {
  sample_size <- c(sample_size,j)
for (i in seq(1,1000,1)) {
  random_data <- rnorm(j, mean = 0.04284802, sd = 0.07139745)
  p_value <- t.test(true_data_delta,random_data)$p.value
  if(p_value < 0.05) {
    significant_vector <- significant_vector + 1
  }
  else{
    not_significant_vector <- not_significant_vector + 1
  }
  
}
  
  probability <- not_significant_vector/(not_significant_vector+significant_vector)
  probability_vector <- c(probability_vector, probability )
  not_significant_vector <- 0
  significant_vector <- 1
}
data_probs <-  cbind(sample_size, probability_vector)

rnorm(10, mean = 0.04645343, sd = 0.07107179)

###
### - - - - - - - - -
###

#POMC_GFP_24H_Veh_Sema_200nM_07_07_23
#POMC_GFP_24H_ALK_Sema_200nM_07_07_23

summary(model_both_no_interaction)

anova(model_both_no_interaction)


data_Ceritinib <- data_for_model %>%
  filter(VEH_OR_CERITINIB == "CERITINIB")

data_Vehicle <- data_for_model %>%
  filter(VEH_OR_CERITINIB == "VEHICLE")


wilcox.test(data_Ceritinib$delta_Application, data_Vehicle$delta_Application, alternative = "two.sided")

library('car')
library('gtsummary')
library('xfun')

model_aov <- aov(model_both_no_interaction)
TukeyHSD(model_aov)


tbl_summary(
  data_for_model,
  include = c(delta_Application, GFP, Agonist_Type),
  by = GFP, # split table by group
  missing = "no" # don't list missing data separately
) %>%
  add_n() %>% # add column with total number of non-missing observations
  add_p() %>% # test for a difference between groups
  modify_header(label = "**Variable**") %>% # update the column header
  bold_labels()


model_both_no_interaction %>%
  tbl_regression() %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  as_gt() %>%

  gt::tab_source_note(gt::md("*This data is simulated*"))


t1 <- model_both_no_interaction %>%
  tbl_regression(estimate_fun = function(x) style_number(x, digits = 5),
                 intercept = TRUE) %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  add_difference() %>%
  as_gt()

t1

##TEXREG FOR EXPORTING MODEL TABLES

test <- wordreg( model_application_GFP, digits = 6, file = "testfile_sema.doc")

cat(test)

screenreg()

screenreg(list(model_application_both, model_application_GFP, model_application_Agonist_Type), digits = 6)

screenreg(model_application_GFP, digits = 6)




2*10/24
2*14/24
2*9/13

2*65/412
2*120/354
2*33/129




## PLOTS FOR MAKING CIRCLES BASED ON NUMBER OF NEURONS

# Define the circle; add a point at the center if the 'pie slice' if the shape is to be filled
circleFun <- function(center=c(0,0), diameter=1, npoints=1000, start=0, end=2, filled=TRUE){
  tt <- seq(start*pi, end*pi, length.out=npoints)
  df <- data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
  if(filled==TRUE) { #add a point at the center so the whole 'pie slice' is filled
    df <- rbind(df, center)
  }
  return(df)
}

#Define separate data frames for the filled and unfilled circles
quarterCircle <- circleFun(c(1,-1), diameter = 2.3, start=0, end= 0.5116279, filled=TRUE)
quarterCircle_2 <- circleFun(c(1,-1), diameter = 2.3, start=  0.5116279, end=2, filled=TRUE)
fullCircle <- circleFun(c(1, -1), 2.3, start=0, end=2, filled=FALSE)
innerCircle <- circleFun(c(1,-1), diameter = 1.6, start=0, end= 2, filled=FALSE)


p <- ggplot() + 
  geom_polygon(data=quarterCircle, aes(x,y), color= "black", fill="black", linewidth = 1.2) +
  geom_polygon(data=quarterCircle_2, aes(x,y), color= "black", fill="dark grey", linewidth = 1.2) +
  geom_polygon(data=innerCircle, aes(x,y), color="black", fill="white", linewidth = 1.2) + 
  geom_path(data=fullCircle, aes(x, y), color="black") +
  theme(
    plot.background = element_rect(fill='transparent', color=NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  coord_equal()

p <- p + theme(rect = element_rect(fill = "transparent"))

p

2*(23/454) #TOTAL RESPONDED LONG RESPONSE GLP_1

2*(6/79) #TOTAL RESPONDED LONG RESPONSE LIRAGLUTIDE

2*(48/378) #TOTAL RESPONDED LONG SEMAGLUTIDE

2*(22/142) #TOTAL RESPONDED LONG TIRZEPATIDE


2*(10/24) #SEMA GFP LONG RESPONSE/ALL SEMA GFP+

2*(3/24) #GLP-1 GFP LONG RESPONSE/ALL GLP-1 GFP+

2*(1/8) #LIRA GFP LONG RESPONSE/ALL LIRA GFP+

2*(6/13) #TIRZEPATIDE GFP LONG RESPONSE/ALL TIRZA GFP+



2*(38/354) #SEMA GFP- LONG RESPONSE/ALL SEMA GFP-

2*(17/412) #GLP-1 GFP- LONG RESPONSE/ALL GLP-1 GFP-

2*(5/71) #LIRA GFP- LONG RESPONSE/ALL LIRA GFP-

2*(16/129) #TIRZEPATIDE- GFP LONG RESPONSE/ALL TIRZA GFP-



2*(7/354) #SEMA GFP- SHORT RESPONSE/ALL SEMA GFP-

2*(7/412) #GLP-1 GFP- SHORT RESPONSE/ALL GLP-1 GFP-

2*(2/71) #LIRA GFP- SHORT RESPONSE/ALL LIRA GFP-

2*(3/129) #TIRZEPATIDE- GFP SHORT RESPONSE/ALL TIRZA GFP-


2*(3/24) #SEMA GFP+ SHORT RESPONSE/ALL SEMA GFP+

2*(1/24) #GLP-1 GFP+ SHORT RESPONSE/ALL GLP-1 GFP+

2*(0/8) #LIRA GFP+ SHORT RESPONSE/ALL LIRA GFP+

2*(0/13) #TIRZEPATIDE- GFP SHORT RESPONSE/ALL TIRZA GFP+


2*(9/454) #TOTAL RESPONDED SHORT RESPONSE GLP_1

2*(2/79) #TOTAL RESPONDED SHORT RESPONSE LIRAGLUTIDE

2*(10/378) #TOTAL RESPONDED SHORT SEMAGLUTIDE

2*(3/142) #TOTAL RESPONDED SHORT TIRZEPATIDE



2*(11/454) #TOTAL RESPONDED INH RESPONSE GLP_1

2*(5/79) #TOTAL RESPONDED INH RESPONSE LIRAGLUTIDE

2*(6/378) #TOTAL RESPONDED INH SEMAGLUTIDE

2*(1/142) #TOTAL RESPONDED INH TIRZEPATIDE


2*(6/354) #SEMA GFP- SHORT RESPONSE/ALL SEMA GFP-

2*(11/412) #GLP-1 GFP- SHORT RESPONSE/ALL GLP-1 GFP-

2*(5/71) #LIRA GFP- SHORT RESPONSE/ALL LIRA GFP-

2*(1/129) #TIRZEPATIDE- GFP SHORT RESPONSE/ALL TIRZA GFP-



2*(34/69) #TOTAL GFP RESPONDERS BASED ON TOTAL USING T TESt

2*(302/966) #TOTAL NON-GFP RESPONDERS BASED ON TOTAL USING T TESt

##EXTRA CODE IN CASE SOMETHING EVER BREAKS - - - - - - - - - - - -


17/32

 96/248

  106/239
  21/30





  
  
  
  
  
  
  
 2* 53/111 #Ceritinib GFP NO
  2*16/28#Ceritinib GFP YES
  2*59/122#Vehicle GFP NO
  2*21/28#Vehicle GFP YES
  
  
  




ptm <- proc.time()

data_f0 <- data_FINAL %>%
  filter(time <= DrugApp1) %>%
  group_by(Neuron_ID) %>%
  mutate(F0 = mean(Mean_Intensity, na.rm = TRUE)) %>%
  dplyr::select(F0,Neuron_ID) %>%
  distinct(Neuron_ID, F0)

data_normalised <- data_FINAL %>%
  mutate(total_time = DrugTime1 + DrugApp1+ 600) %>%
  group_by(Neuron_ID) %>%
  right_join(data_f0) %>%
  mutate(FDelta = Mean_Intensity - F0) %>%
  mutate(FDelta0 = FDelta/F0) %>%
  mutate(Normalised_Intensity = FDelta/ (max(Mean_Intensity, na.rm = TRUE) - F0)) 


data_before <- data_FINAL %>%
  right_join(data_normalised) %>%
  mutate(before_time = DrugApp1 - 240) %>%
  filter(time < DrugApp1) %>%
  filter(time >= before_time) %>%
  group_by(Neuron_ID) %>%
  mutate(before_Application = (sum(Normalised_Intensity, na.rm = TRUE))/240) %>%
  dplyr::select(before_Application ,Neuron_ID) %>%
  distinct(Neuron_ID, before_Application)

data_after <- data_FINAL %>%
  mutate(total_time = DrugTime1 + DrugApp1+ 600) %>%
  right_join(data_normalised) %>%
  right_join(data_before) %>%
  filter(time >= DrugApp1) %>%
  filter(time <= total_time) %>%
  group_by(Neuron_ID) %>%
  mutate(after_Application = (sum(Normalised_Intensity, na.rm = TRUE))/total_time) %>%
  dplyr::select(after_Application ,Neuron_ID) %>%
  distinct(Neuron_ID, after_Application)

data_FDelta_compare <- data_normalised %>%
  right_join(data_before) %>%
  right_join(data_after) %>%
  mutate(delta_Application = after_Application - before_Application)






head(data_FDelta_Neuron)




subset(data_FDelta) %>%
  filter(time > KCLapp + 60 & time < KCLapp + KCLtime + 60) %>%
ggplot(aes(x = Normalised_Intensity)) +
  geom_histogram(aes(y = after_stat(density)), fill = "light blue", color = "black") +
  geom_density( size = 1, color = "blue") +
  #geom_vline(data = dat_vline, aes(xintercept = intercept), linetype = "dashed", color = "red", size = 1.2) +
  #  geom_text(
  #   size    = 4,
  #  data    = dat_vline,
  # mapping = aes(x = 0.07, y = 15, label = label),
  #  color = "red") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1, color = "black"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 15)
  ) +
  ylab(label = "Density") +
  xlab(label = "DeltaF/F0") +
  #scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  #scale_x_continuous(limits = c(-0.125,0.125)) +
  ggtitle("Distribution of DeltaF/F0")


data_for_traces_KCL <- data_FINAL %>%
  group_by(Neuron_ID) %>%
  filter(time >= KCLapp-120) %>%
  mutate(F0 = mean(Mean_Intensity[time <= KCLapp], na.rm = TRUE)) %>%
  mutate(FDelta = Mean_Intensity - F0) %>%
  mutate(Normalised_Intensity = FDelta/ (max(Mean_Intensity, na.rm = TRUE) - F0)) %>%
  filter(time <= KCLapp + 300) %>%
  arrange(time) %>%
  mutate(time = (row_number()*10)-10) %>%
  filter(GFP != "NN")


data_for_traces <- data_KCL_combined %>%
  group_by(Neuron_ID) %>%
  filter(time >= (DrugApp1 - 260)) %>%
  mutate(F0 = mean(Mean_Intensity[time <= DrugApp1], na.rm = TRUE)) %>%
  mutate(FDelta = Mean_Intensity - F0) %>%
  mutate(Normalised_Intensity = FDelta/ (max(Mean_Intensity, na.rm = TRUE) - F0)) %>%
  filter(time >= (DrugApp1 - 260), time <= (DrugApp1 + 600)) %>%
  arrange(time) %>%
  mutate(time = (row_number()*10)-10) %>%
  filter(GFP != "NN")



df <- data.frame(X1 = runif(1000), X2 = runif(1000), subj = rep(c("A", "B")))

df %>% 
  {pairwise.t.test(.$X1, .$subj, paired = TRUE)}


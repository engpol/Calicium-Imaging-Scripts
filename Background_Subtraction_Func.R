

Background_Subtract <- function(mydata,mydata_GFP,mydata_drug) {

  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
  BETTER_LETTERS <- LETTERS702[27:length(LETTERS702)]
  #Calculate number of columns in Intensity Data, and collect column names
  col_number <- ncol(mydata)
  col_names <- colnames(mydata)
  
  
  #Return TRUE only for BACKGROUND columns
  background_boolean <- str_ends(colnames(mydata), "BACKGROUND")
  
  
  
  ##Create a list of the variable names in a clean and easily analysable format,
  ##Plate number is written as a letter, neuron number as number, and background is written as 'RMV'
  ##I.e. In plate 1, A1, A2, A3, A4... ARMV, then plate 2 B1, B2 B3.... BRMV etc. 
  
  plate_number <- 1
  Count_number <- 1
  Clean_names <- c()
  
  for(i in 2:col_number){
    if(background_boolean[i] == TRUE){
      background_column <- str_c(BETTER_LETTERS[plate_number],Count_number)
      Clean_names <- c(Clean_names,  background_column)
      plate_number <- plate_number + 1
      Count_number <- 1
    }
    else{
      regular_column <- str_c(BETTER_LETTERS[plate_number],Count_number)
      Clean_names <- c(Clean_names, regular_column)
      Count_number <- Count_number + 1 
    }
  } 
  
  
  ##Rename the variable names in the dataframe with our clean variable names
  
  Data_For_Subtraction <- mydata[,-1] 
  colnames(Data_For_Subtraction) <- Clean_names
  
  
  ## Perform the background subtraction;
  ## Remove all numbers, and split the data frame based on the letter (i.e. plate)
  ## Rename the last column in each new data frame to RMV
  ## Subtract RMV from every other column, row-wise, in each data frame
  
  
  
  Data_Subtracted <- Data_For_Subtraction %>%
    split.default(sub('\\d+', '', names(Data_For_Subtraction))) %>%
    lapply(function(x) {names(x)[ncol(x)] <- "RMV";x}) %>%
    map(~mutate(.x, across(1:last_col(1), ~.x - RMV))) 
  
  
  ## Merge all the dataframes back into 1, cbind.na has to be used as rbind/cbind don't work if all dataframes
  ## don't have the same number of rows
  
  df <- do.call(qpcR:::cbind.na, Data_Subtracted)
  
  ## Re-add the time column to our final wide_data frame
  
  df_subtracted_wide <- cbind(mydata[,1],df)
  
  ## Re-add normal 'messy' column names to background subtracted data
  
  colnames(df_subtracted_wide) <- col_names
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  ##ADD TEMPORARILY
#df_subtracted_wide <- df_subtracted_wide %>%
 #   dplyr::select(!starts_with("POMC_GFP_24H_Veh_Sema_200nM_07_07_23")) %>%
  #  dplyr::select(!starts_with("POMC_GFP_24H_ALK_Sema_200nM_07_07_23"))
  
  
  
  ## PIVOT ALL DATA FROM SHORT INTO LONG FORMAT - - - - -
  ## MERGE DATA OF GFP EXPRESSION  - - - - - - - - - -
  
  
  ##REMOVE BACKGROUND COLUMNS - NEEDED AS OTHERWISE PIVOT_LONGER DOESNT WORK AS BACKGROUND COLUMNS ARE A VECTOR BUT MUTATE CONVERTS NEW COLUMNS INTO DATAFRAMES
  #VERY IMPORTANT THERE ARE NO TYPOS IN BACKGROUND COLUMNS!!!!
  
  df_subtracted_wide <- df_subtracted_wide %>%
    dplyr::select(-c(ends_with("BACKGROUND")))
  
  
  data_long <- pivot_longer(df_subtracted_wide, 2:last_col(), 
                            names_to = "Neuron_ID", 
                            values_to = "Mean_Intensity")
  
  
  ##Select only GFP classification and remove BACKGROUND columns in GFP and Drug Times Data
  
  GLP_1_GFP_Binary <- mydata_GFP[1,] %>%
    dplyr::select(-c(1)) %>%
    dplyr::select(-c(ends_with("BACKGROUND"))) %>%
    dplyr::select(-c(starts_with("BACKGROUND")))
  
  
  GLP_1_GFP_Mean <- mydata_GFP[3,] %>%
    dplyr::select(-c(1)) %>%
    dplyr::select(-c(ends_with("BACKGROUND"))) %>%
    dplyr::select(-c(starts_with("BACKGROUND")))
  
  
  
  #Pivot GFP Binary to Long format
  
  data_long_GFP_1 <- pivot_longer(GLP_1_GFP_Binary, everything(),  
                                  names_to = "Neuron_ID", 
                                  values_to = "GFP")
  
  
  data_long_GFP_Mean <- pivot_longer(GLP_1_GFP_Mean, everything(),  
                                     names_to = "Neuron_ID", 
                                     values_to = "GFP_Intensity") 
  
  data_long_GFP_Mean$GFP_Intensity <- as.numeric(data_long_GFP_Mean$GFP_Intensity)
  
  data_long_GFP <- merge(data_long_GFP_1, data_long_GFP_Mean)
  
  data_subtract_GFP <- merge(data_long, data_long_GFP, 
                             by = "Neuron_ID", all = TRUE, no.dups = TRUE)
  
  
  data_subtract_GFP$Neuron_ID <- as.factor(data_subtract_GFP$Neuron_ID)
  
  
  Drug_no_background <-mydata_drug%>%
    filter(!grepl("BACKGROUND",Neuron_ID))
  
  
  data_FINAL <- merge(data_subtract_GFP,Drug_no_background, 
                      by = "Neuron_ID", all = TRUE, no.dups = TRUE)
  
  data_FINAL$Neuron_ID <- as.factor(data_FINAL$Neuron_ID)

  return(data_FINAL)
}





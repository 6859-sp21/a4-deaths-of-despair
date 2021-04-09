# PROGRAM NAME: 6.859 - A4
# DESCRIPTION: Make a Data Viz
# PROGRAMMER(S): Axelle Clochard
# DATE WRITTEN: 3/31/2021



# STEP 0. SET UP WORKSPACE AND LOAD DATA
# ---------------------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------------------#


# A. SET WORKSPACE PARAMS
# ------------------------#
rm(list = ls()) 

# Set working directory to where file is
# script.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(script.dir)
setwd('C:/Users/axell/OneDrive/Documents/MIT/6.859/A4')


# install packages if necessary
packages <- c("ggplot2",
              "plyr",
              "scales",
              "dplyr",
              'stringr',
              "tidyr",
              "purrr",
              "gridExtra",
              "grid",
              "xlsx", 
              "reshape2", 
              "stargazer", 
              "Hmisc", 
              "readxl",
              "httr",
              "Hmisc",
              "ipumsr",
              "R.utils",
              "maps",
              "rgdal",
              "ggtext",
              "data.table",
              "icd.data")

install_list <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(install_list)) install.packages(install_list)

lapply(packages, require, character.only = TRUE)






# B. READ IN DATA
# ------------------------#

## NVSS Mortality data
data <- fread("./mortality2.csv")
#data <- subset(data, select = -c(state_ocurrence, county_ocurrence, state_residence,  county_residence, underlying_cause))

# ## ICD Recode Data
# https://resdac.org/sites/datadocumentation.resdac.org/files/358%20ICD-10%20Recodes.txt
# ICD <- read.delim("358 ICD-10 Recodes.txt", sep = "=")
# ICD$Code <- gsub("\t", "", as.character(row.names(ICD)))
# names(ICD)[1] <- "Description"
# 
# # Fix errors
# ICD <- subset(ICD, ! (Description == "" | Description == " 71 "))
# ICD[nrow(ICD) + 1,] <- c("Of nasal cavity, middle ear and accessory sinuses (C30-C31)", "09100")
# ICD$Description <- trimws(ICD$Description)


# ICD Descriptionss (note: Incomplete)
ICD = icd10cm2016



# C. CATEGORIZE TYPES OF DEATH
# --------------------------------#


# C1. Find all unique death codes in mortality data
codes <- as.data.frame(table(data$ICD_Code))
codes$char <- substring(codes$Var1, 1, 1)
codes$num <- sub('.', '', codes$Var1)

codes$death_type <- "Other"



# C.2. Identify Death Types
# DEATHS OF DESPAIR:  Drug and alcohol poisonings, suicide, and chronic liver diseases and cirrhosis.

# Poisonings - "Poisoning: accidental and intent undetermined drug overdose and alcohol poisoning"
# ICD9 codes: 850-860, 980; ICD10 codes: X40-45, Y10-15, Y45, Y47, Y49
codes$death_type[codes$Var1 %in% c(paste0("X", 40:45), paste0("Y", 10:15), "Y45", "Y47", "Y49")] <- "Poisoning"

# Suicide:
# ICD9 codes: 950-959; ICD10 codes: X60-84, Y87.0
codes$death_type[codes$Var1 %in% c(paste0("X", 60:84),  "Y870")] <- "Suicide"

# Alcoholic Liver Disease and Cirrhosis:
# ICD9 codes: 571; ICD10 codes: K70, K73, K74
# Note: After investigation, I added: "K732" "K738" "K739" "K741" "K742" "K743" "K744" "K745" "K746"
codes$death_type[codes$Var1 %in% c("K70", "K73", "K74", "K700", "K730", "K740", paste0("K", 700:709), "K732", "K738", "K739", "K741", "K742", "K743", "K744", "K745", "K746")] <- "Alcoholic Liver Disease and Cirrhosis"


# OTHER LEADING KILLERS: 

# Heart Disease:
# ICD9 codes: 390-429; ICD10 codes: I00-I09, I11, I13, I20-I51
codes$death_type[codes$Var1 %in% c("I11", "I13", paste0("I0", 1:9), "I00", paste0("I", 20:51))] <- "Heart Disease"


# Cancer: malignant neoplasms
# ICD9 codes: 140-208; ICD10 codes: C00-C97
codes$death_type[codes$Var1 %in% c("C00", paste0("C0", 1:9), paste0("C", 10:97))] <- "Cancer"




# C.3. Import Specific Descriptions for DODs (Just in Case): 
DODs <- c(paste0("X", 40:45), paste0("Y", 10:15), "Y45", "Y47", "Y49", 
          paste0("X", 60:84),  "Y870", 
          "K70", "K73", "K74", "K700", "K730", "K740", paste0("K", 700:709),
          "K732", "K738", "K739", "K741", "K742", "K743", "K744", "K745", "K746")

ICD_dod <- subset(ICD, code %in% DODs)

table(codes$death_type)

# C.4. Add in Overall Categorization:
codes$Overall_Cause <- "Not a DoD"
codes$Overall_Cause[codes$Var1 %in% DODs] <- "DoD"

# C.5. Merge Data:
data <- merge(x = data,
              y = subset(codes, select = -c(Freq, char, num)),
              by.x = "ICD_Code", by.y = "Var1",
              all.x = T)








# D. AGGREGATE RESULTS
# --------------------------------#

# # Aggregate by all
# agg <- data %>%
#         group_by(year, gender, age_key, race_key, marital_status, ed_key, manner_of_death, ICD_Code) %>%
#         summarise(counts = n())



# Aggregate one at a time:
agg_one <- rbind(
  
  # ONE VAR AT A TIME
              data %>% 
                group_by(year, gender, death_type, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Gender", Var2 = NA, Var3 = NA), 
              
              data %>% 
                group_by(year, age_key, death_type, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Age", Var2 = NA, Var3 = NA), 
              
              data %>% 
                group_by(year, race_key, death_type, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Race", Var2 = NA, Var3 = NA), 
              
              data %>% 
                group_by(year, marital_status, death_type, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Marital Status", Var2 = NA, Var3 = NA), 
              
              data %>% 
                group_by(year, ed_key, death_type, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Education", Var2 = NA, Var3 = NA))
              
  # TWO VAR AT A TIME
agg_two <- rbind(
              data %>% 
                group_by(year, race_key, age_key, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Race", Var2 = "Age", Var3 = NA),
  
              data %>% 
                group_by(year, race_key, ed_key, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Race", Var2 = "Education", Var3 = NA),
  
              data %>% 
                group_by(year, race_key, gender, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Race", Var2 = "Gender", Var3 = NA),
  
  
              data %>% 
                group_by(year, race_key, marital_status, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Race", Var2 = "Marital Status", Var3 = NA),
              
              data %>% 
                group_by(year, age_key, gender, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Age", Var2 = "Gender", Var3 = NA),
  
              data %>% 
                group_by(year, age_key, ed_key, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Age", Var2 = "Education", Var3 = NA),
              
              data %>% 
                group_by(year, marital_status, ed_key, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Marital Status", Var2 = "Education", Var3 = NA))
                          

  # THREE VAR AT A TIME
agg_three <- data %>% 
                group_by(year, race_key, age_key, ed_key, Overall_Cause) %>%
                summarise(counts = n()) %>%
                mutate(Var1 = "Race", Var2 = "Age", Var3 = "Education")





# JOIN ALL 

agg <- rbind(agg_one, 
             agg_two,
             agg_three)

agg <- agg[c("Var1", "Var2", "Var3", "year", "age_key", "race_key", "gender", "ed_key", "marital_status", "death_type", "Overall_Cause", "counts")]
names(agg) <- tolower(names(agg))

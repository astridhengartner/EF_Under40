### Load packages ###
library(ggplot2)
library(tidyr)
library(dplyr)
library(plyr)
library(base)
library(car)
library(lme4)
library(lmerTest)
library(summarytools)
library(ggbeeswarm)
library(lubridate)

# Load Data
ee <- read.csv("/Users/Astrid/Desktop/CarDS/Systolic HF/DATA/OG Data/ECHO_ef_date_result_patID_wide_0330.csv")
pp <- read.csv("/Users/Astrid/Desktop/CarDS/Systolic HF/DATA/OG Data/HOSP_level_Study_Population_patID_0419.csv")


# All patients have only 1 entry (had to change a duplicate)
table(table(ee$patient_id))


# Change Hosptilzation Admission Dates to R-Readable Dates
tmp <- sapply(pp$Admission.Time, function(x){
  l0 <- strsplit(as.character(x), " ")[[1]]
  l1 <- as.Date(l0[1], format = '%m/%d/%Y')
  l2 <- paste(l1)
  return(l2)})

pp$New_Admission.Time <- as.POSIXct(tmp, format='%Y-%m-%d', tz = "EST")


# Change Hospitaliation Discharge Dates to R-Readable Dates 
tmp1 <- sapply(pp$Discharge.Time, function(x){
  l0 <- strsplit(as.character(x), " ")[[1]]
  l1 <- as.Date(l0[1], format = '%m/%d/%Y')
  l2 <- paste(l1)
  return(l2)})

pp$New_Discharge.Time <- as.POSIXct(tmp1, format='%Y-%m-%d', tz = "EST")


# Split to isolate patient ID, Admission Date, and Discharge Date
PP <- split(pp[, c("patient_id", "New_Admission.Time", "New_Discharge.Time")], pp$patient_id)


# Fill blank Echo procedure dates with "NA"
ee[ee==""] <- NA


# Find all column names of Echo procedure dates that have values
AllProcedureDates <-  colnames(ee)[grep("Procedure.Date", colnames(ee))]


# Select patient IDs present in both datasets
tmp_ee <- ee[ee$patient_id %in% pp$patient_id,]


# Change procedure dates to R-readable dates & compare Echo dates with hospitalization admission dates
for (i in 1:nrow(tmp_ee))
{ 
  # Find All Procedure dates that are available
  ProcedureDate_i <-  tmp_ee[i, AllProcedureDates]
  Ff <- !is.na(ProcedureDate_i)
  ProcedureDate_i <-  ProcedureDate_i[Ff] 
  names(ProcedureDate_i) <- AllProcedureDates[Ff]
  
  # Format as date
  LabDates <- as.Date(ProcedureDate_i, format='%m/%d/%y', tz = "EST")
  names(LabDates) <-AllProcedureDates[Ff]
  
  # Compare Dates
  Flag <- rep(FALSE, length(LabDates))
  
  for (j in 1:length(LabDates)) 
  {
    for (k in 1:nrow(PP[[i]])) 
    {
      Flag[j] <- PP[[i]]$New_Admission.Time[k] <= LabDates[j] & PP[[i]]$New_Discharge.Time[k] >= LabDates[j] 
      if (Flag[j])  break 
    }
  }
  
  Reduced_LabDates <- LabDates[Flag]
  DiscardLabs <- LabDates[!Flag]
  tmp_ee[i,names(DiscardLabs)] <- NA
}


# Rename all of the Echos that occur during an admission as tmp40
tmp40 <- tmp_ee


# Make all blanks NA
tmp40[ tmp40==""] <- NA


# Find all of the column names that have "test_result_ef" in them --> these are the EF results
AllEF40 <-  colnames(tmp40)[grep("test_result_ef", colnames(tmp40))]


# For each EF result, check if <= 40
tmp40$Valid_EF_Under40 <- apply(tmp40[,AllEF40], 1, function(x) max(x <=40 & !is.na(x)))


# How many patients have EF under 40 (for echo during admission)
table(tmp40$Valid_EF_Under40) 


# For each EF result, check if <= 50
tmp40$Valid_EF_Under50 <- apply(tmp40[,AllEF40], 1, function(x) max(x <=50 & !is.na(x)))
table(tmp40$Valid_EF_Under50)
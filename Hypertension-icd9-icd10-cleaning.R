source("R_packages.R")

htn_icd_10 <- read_csv(file = "Hypertension_icd10_LSHTM_2022.csv", col_names = TRUE)
htn_icd_9 <- read_csv(file = "Hypertension_icd9_LSHTM_2022.csv", col_names = TRUE)
glimpse(htn_icd_10)
glimpse(htn_icd_9)
##Aim: clean the icd 10 and icd 9 codes of Hypertension-removing dots

htn_icd_10 <- htn_icd_10 %>%
  mutate(icd10 = `ICD-10 codes` %>%
           str_replace_all("\\.", "") %>%  # Remove dots
           str_trim() %>%                 # Remove whitespace
           toupper())                     # Make uppercase

htn_icd_9 <- htn_icd_9 %>%
  mutate(icd9 = `ICD-9 codes` %>%
           # Round to exactly 1 decimal (e.g., 401.10 -> 401.1, 402.01 -> 402.0)
           round(digits = 1) %>% 
           #  Convert to character with 1 decimal place guaranteed
           format(nsmall = 1) %>% 
           # Remove the dot ("402.0" becomes "4020")
           str_replace_all("\\.", "") %>%  
           str_trim())

htn_icd_10$`ICD-10 codes` <- NULL
htn_icd_9$`ICD-9 codes` <- NULL

#Save the codelist in csv file
write_csv(htn_icd_9, "Hypertension_icd9_codelist.csv")
write_csv(htn_icd_10, "Hypertension_icd10_codelist.csv")


source("R_packages.R")
###Aim:create ICD 9codelist by mapping with icd10 code
#importing the master lookup file to map ICD9 to ICD10
icd9_icd10_map <- read_excel("all_lkps_maps_v4.xlsx", sheet = "icd9_icd10")
head(icd9_icd10_map)
#importing the LSHTM codelist file for Diabetes using ICD10 code
dm_icd_10 <- read_csv(file = "Diabetes_icd10_2021_LSHTM.csv", col_names = TRUE)
head(dm_icd_10)

# Clean the master lookup map
icd9_icd10_map <- icd9_icd10_map %>%
  mutate(ICD10_clean = ICD10 %>%
           str_replace_all("\\.", "") %>%  # Remove dots
           str_trim() %>%                 # Remove whitespace
           toupper())                     # Make uppercase

# Clean diabetes codelist
dm_icd_10 <- dm_icd_10 %>%
  mutate(icd_clean = icd %>%
           str_replace_all("\\.", "") %>%  # Remove dots
           str_trim() %>%                 # Remove whitespace
           toupper())                     # Make uppercase

# Perform the join using the new clean columns
merged_data_icd9 <- icd9_icd10_map %>%
  left_join(dm_icd_10, by = c("ICD10_clean" = "icd_clean"))
head(merged_data_icd9)

# Filter to see the results
icd9_code <- merged_data_icd9 %>%
  filter(!is.na(description)) 

#Save the codelist in csv file
write_csv(icd9_code, "Diabetes-icd9-mapping.csv")
write_csv(dm_icd_10, "Diabetes-icd10-cleaned.csv")


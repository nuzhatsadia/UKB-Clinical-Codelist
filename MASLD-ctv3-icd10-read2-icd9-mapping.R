source("R_packages.R")

###Aim:Validate the OpenSafely MASLD CTV3 codelist against the CTV3 Master Lookup file 
#to ensure the clinical descriptions in both datasets are consistent and accurately matched.

# Read the CTV3 lookup sheet from the all lookup map file
ctv3 <- read_excel("all_lkps_maps_v4.xlsx", sheet = "read_ctv3_lkp")

#importing the opensafely codelist file for MASLD using CTV3 code
masld_ctv3_os <- read_csv(file = "nuzhatsadia-nafldmasld-ctv3-opensafely.csv", col_names = TRUE)

# Create the merged dataset
combined_data <- ctv3 %>%
  left_join(masld_ctv3_os, by = c("read_code" = "code"))

# Filter to see only the matches and compare descriptions
verification_check <- combined_data %>%
  filter(!is.na(term)) %>%
  select(read_code, term_description, term)

View(verification_check)

#row count of "masld_ctv3_os" is lower than the matched dataset "verification_check"
#so there are multiple entries per code in"verification_check" 
#possibly due to the presence of different term in the CTV3 lookup.
# Filter to see where the descriptions don't match
verification_check %>%
  filter(term != term_description)

#however, after viewing the mismatched term descriptions, it seems none of them 
#contains any description other than our desired definition

###Aim:Validate the OpenSafely MASLD CTV3 codelist against read_ctv3 to icd10 mappings 
#to identify and include any missing relevant codes.

#importing the master lookup file to map CTV3 to ICD10
ctv3_icd10_map <- read_excel("all_lkps_maps_v4.xlsx", sheet = "read_ctv3_icd10")

#importing the opensafely codelist file for MASLD using ICD10 code
masld_icd_os <- read_csv(file = "nuzhatsadia-nafldmasld-icd-10-opensafely.csv", col_names = TRUE)

#Merging the opensafely ICD10 codes with  CTV3 to ICD10 map dataset
merged_data <- ctv3_icd10_map %>%
  left_join(masld_icd_os, by = c("icd10_code" = "code"))

#filter to see those merged only rows
verify <- merged_data %>%
  filter(!is.na(term))%>%
  select(read_code, icd10_code, term)

View(verify)

#after matching the icd10 codes with the CTV3 codes from "ctv3_icd10_map" dataset, 
#row count for "masld_ctv3_os" doesn't match with "verify" dataset
#so there might be some of the CTV3 codes from verify dataset that are not present 
#in the opensafely prepared codelist and vice versa
#let's find the mismatches between the "masld_ctv3_os" and "verify" dataset

# Codes in verify that are missing from masld_ctv3_os
only_in_verify <- verify %>%
  anti_join(masld_ctv3_os, by = c("read_code" = "code"))
View(only_in_verify)

# Codes in masld_ctv3_os that are missing from verify
only_in_os <- masld_ctv3_os %>%
  anti_join(verify, by = c("code" = "read_code"))
View(only_in_os)

#from the description of the codes, we can include all of them as they meet the definition

#Join both datasets completely and Ensure each CTV3 code appears only once
masld_ctv3_combined <- full_join(verify, masld_ctv3_os, 
                           by = c("read_code" = "code")) %>%
  distinct(read_code, .keep_all = TRUE)

# View the final comprehensive list
View(masld_ctv3_combined)
write_csv(masld_ctv3_combined, "nafldmasld-ctv3-combined_mapping.csv")

###Aim:create read2 codelist by mapping with icd10 code
#importing the master lookup file to map read2 to ICD10
read2_icd10_map <- read_excel("all_lkps_maps_v4.xlsx", sheet = "read_v2_icd10")

#Merging the opensafely ICD10 codes with read2 to ICD10 map dataset
merged_data_read2 <- read2_icd10_map %>%
  left_join(masld_icd_os, by = c("icd10_code" = "code"))

#filter to see those merged only rows
read2_code <- merged_data_read2 %>%
  filter(!is.na(term))%>%
  select(read_code, icd10_code, term)

View(read2_code)
write_csv(read2_code, "nafldmasld-read2-mapping.csv")


###Aim:create ICD 9codelist by mapping with icd10 code
#importing the master lookup file to map ICD9 to ICD10
icd9_icd10_map <- read_excel("all_lkps_maps_v4.xlsx", sheet = "icd9_icd10")

#Merging the opensafely ICD10 codes with ICD9 to ICD10 map dataset
merged_data_icd9 <- icd9_icd10_map %>%
  left_join(masld_icd_os, by = c("ICD10" = "code"))

#filter to see those merged only rows
icd9_code <- merged_data_icd9 %>%
  filter(!is.na(term))

View(icd9_code)
write_csv(icd9_code, "nafldmasld-icd9-mapping.csv")


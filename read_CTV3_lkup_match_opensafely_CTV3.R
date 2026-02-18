source("R_packages.R")

###Aim:Validate the OpenSafely Liver Cancer CTV3 codelist against the CTV3 Master Lookup file 
#to ensure the clinical descriptions in both datasets are consistent and accurately matched.

#importing the master lookup file
ctv3 <- read.csv(file = "read3lookup.csv", header = TRUE)
#importing the opensafely codelist file for liver cancer using CTV3 code
os <- read_csv(file = "nuzhatsadia-liver-cancer-CTV3_opensafely.csv", col_names = TRUE)

# Create the merged dataset
combined_data <- ctv3 %>%
  left_join(os, by = c("read_code" = "code"))

# Filter to see only the matches and compare descriptions
verification_check <- combined_data %>%
  filter(!is.na(term)) %>%
  select(read_code, term_description, term)

View(verification_check)

#row count of "os" is lower than the matched dataset "verification_check"
#so there are multiple entries per code in"verification_check" 
#possibly due to the presence of different term in the CTV3 lookup.

# Filter to see where the descriptions don't match
verification_check %>%
  filter(term != term_description)

#however, after viewing the mismatched term descriptions, it seems none of them 
#contains any description other than our desired definition


###Aim:Validate the OpenSafely Liver Cancer CTV3 codelist against read_ctv3 to icd10 mappings 
#to identify and include any missing relevant codes.

#importing the master lookup file to map CTV3 to ICD10
map <- read_csv(file = "read3_icd10.csv", col_names = TRUE)
#importing the opensafely codelist file for liver cancer using ICD10 code
os_icd <- read_csv(file = "nuzhatsadia-liver-cancer-icd10_opensafely.csv", col_names = TRUE)

#Merging the opensafely ICD10 codes with that of map dataset
merged_data <- map %>%
  left_join(os_icd, by = c("icd10_code" = "code"))

#filter to see those merged only rows
verify <- merged_data %>%
  filter(!is.na(term))%>%
  select(read3_code, icd10_code, term)

View(verify)
#after matching the icd10 codes with the CTV3 codes from "map" dataset, 
#row count for "os" doesn't match with "verify" dataset
#so there are multiple CTV3 codes against one icd10 code in "verify" dataset
table(verify$icd10_code)

#and some of the CTV3 codes are not present in the opensafely prepared codelist 

#check those missing rows in the os dataset
verify_os_merged <- verify %>%
  left_join(os, by = c("read3_code" = "code"))

verify_os_merged %>%
  filter(is.na(term.y))

#the number of missing codes doesn't match the technical count

# How many UNIQUE CTV3 codes are in ICD-10 discovery list?
unique_verify <- length(unique(verify$read3_code))

# How many of those UNIQUE codes are actually missing from OS?
missing_codes_list <- verify %>%
  filter(!(read3_code %in% os$code)) %>%
  distinct(read3_code, .keep_all = TRUE)
nrow(missing_codes_list)
#similar to the previous count instead of the technical count.so revarification done. 

#now check the terminology description of those new codes retrieved from missing list
merged_ctv3missing <- ctv3 %>%
  left_join(missing_codes_list, by = c("read_code" = "read3_code"))


merged_ctv3missing %>%
  filter(!is.na(term))%>%
  select(read_code, term_description, icd10_code, term)

#from viewing the term description, only 2 new codes meet the definition 
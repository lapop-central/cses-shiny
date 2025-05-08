# # -----------------------------------------------------------------------
# Author: Robert Vidigal, PhD
# Purpose: CSES Data Preprocessing for Shiny Data Playground
# Data In: cses_imd.rdata
# Data Out: cses_shiny_data.rds
# Prev file: None
# Status: On-going
# Machine: Windows OS
# # -----------------------------------------------------------------------
rm(list=ls()); gc()
library(dplyr)

# Loading DATA
# # -----------------------------------------------------------------------
load("./Data preprocessing/cses_imd.rdata")
names(attributes(cses_imd))

# COUNTRY LABELS CSV FILE (pais_lab) creation
# # -----------------------------------------------------------------------
table(cses_imd$IMD1006_NAM); cses_imd$pais <- cses_imd$IMD1006_NAM

table(cses_imd$IMD1006_UNALPHA2); cses_imd$pais_lab <- cses_imd$IMD1006_UNALPHA2

table(cses_imd$IMD1006_UN); cses_imd$pais_num <- cses_imd$IMD1006_UN

# --- PAIS_LAB CSV
pais_lab<-data.frame(pais_nam = cses_imd$IMD1006_NAM,
                     pais_lab = cses_imd$IMD1006_UNALPHA2,
                     pais_num = cses_imd$IMD1006_UN)

pais_lab<-unique(pais_lab)
write.csv(pais_lab, "./Data preprocessing/pais_lab.csv", row.names=F)

# YEARS VARIABLE
table(cses_imd$IMD1008_YEAR); cses_imd$wave <- cses_imd$IMD1008_YEAR

# Modules DUMMY VARS
cses_imd$IMD1008_MOD<-NA
cses_imd$IMD1008_MOD[cses_imd$IMD1008_MOD_1 == 1] <- 1
cses_imd$IMD1008_MOD[cses_imd$IMD1008_MOD_2 == 1] <- 2
cses_imd$IMD1008_MOD[cses_imd$IMD1008_MOD_3 == 1] <- 3
cses_imd$IMD1008_MOD[cses_imd$IMD1008_MOD_4 == 1] <- 4
cses_imd$IMD1008_MOD[cses_imd$IMD1008_MOD_5 == 1] <- 5

# Convert to factor (optional)
cses_imd$IMD1008_MOD <- factor(cses_imd$IMD1008_MOD, levels = 1:5,
                        labels = c("MODULE 1", "MODULE 2", "MODULE 3",
                                   "MODULE 4", "MODULE 5"))

# # -----------------------------------------------------------------------
# GROUPING VARIABLES RECODE
# # -----------------------------------------------------------------------

# --- GENDER (BINARY) - 1 MALE 2 FEMALE 3 OTHER 7/9 NR/MI
table(cses_imd$IMD2002)
cses_imd$genderm <- cses_imd$IMD2002
cses_imd$gendermc <- ifelse(cses_imd$genderm == 1, "Men",
                            ifelse(cses_imd$genderm==2, "Women", NA))
table(cses_imd$gendermc)

# --- EDUCATION 3-CAT (7 8 9 = NR/DK/NA)
# 0. NONE (NO EDUCATION)/ILLITERATE
# 1. PRIMARY EDUCATION/LOWER SECONDARY EDUCATION
# 2. HIGHER SECONDARY EDUCATION
# 3. POST-SECONDARY (NON-UNIVERSITY) EDUCATION
# 4. UNIVERSITY EDUCATION
# 6. OTHER [SEE STANDALONE CSES MODULE CODEBOOK]
table(cses_imd$IMD2003)

cses_imd$edrer <- cses_imd$IMD2003
cses_imd$edrer <- ifelse(cses_imd$edrer < 2, 1,
                         ifelse(cses_imd$edrer == 2 | cses_imd$edrer == 3, 2,
                                ifelse(cses_imd$edrer == 4, 3, NA)))

cses_imd$edrerf <- factor(cses_imd$edrer,
                    levels = c(1, 2, 3),
                    labels = c("None/Primary", "Secondary", "Superior"))
table(cses_imd$edrerf)

# --- AGE
# 01. YOUNGEST - 24 YEARS
# 02.       25 - 34 YEARS
# 03.       35 - 44 YEARS
# 04.       45 - 54 YEARS
# 05.       55 - 64 YEARS
# 06. 65 YEARS - OLDEST
table(cses_imd$IMD2001_2)

cses_imd$age <- cses_imd$IMD2001_2
cses_imd$age <- ifelse(cses_imd$age>6, NA, cses_imd$age)

cses_imd$age <- factor(cses_imd$age,
                       levels = c(1:6),
                       labels = c("Youngest-24", "25-34", "35-44", "45-54",
                                  "55-64", "65-Oldest"))

# --- HOUSEHOLD INCOME (7 8 9 = NR/DK/MI)
# 1. LOWEST HOUSEHOLD INCOME QUINTILE
# 2. SECOND HOUSEHOLD INCOME QUINTILE
# 3. THIRD HOUSEHOLD INCOME QUINTILE
# 4. FOURTH HOUSEHOLD INCOME QUINTILE
# 5. HIGHEST HOUSEHOLD INCOME QUINTILE
table(cses_imd$IMD2006)

cses_imd$wealth <- cses_imd$IMD2006
cses_imd$wealth[cses_imd$wealth > 5] <- NA

cses_imd$wealthf <- factor(cses_imd$wealth,
                     levels = c(1, 2, 3, 4, 5),
                     labels = c("Low", "2", "3", "4", "High"))

# --- URBAN-RURAL (7 8 9 = NR/DK/MI)
# 1. RURAL AREA OR VILLAGE
# 2. SMALL OR MIDDLE-SIZED TOWN
# 3. SUBURBS OF LARGE TOWN OR CITY
# 4. LARGE TOWN OR CITY
table(cses_imd$IMD2007)

cses_imd$ur <- cses_imd$IMD2007
cses_imd$ur <- ifelse(cses_imd$ur == 3 | cses_imd$ur == 4, 1,
                      ifelse(cses_imd$ur == 1 | cses_imd$ur == 2, 2, NA))

cses_imd$ur <- factor(cses_imd$ur, levels=c(1,2), labels = c("Urban", "Rural"))
table(cses_imd$ur)

# IDEOLOGY (0 LEFT 10 = RIGHT)
# 96. OTHER: NOT SPECIFIED (CODEBOOK STATES AS 96 CODE BUT DATA SHOWS 95 CODE)
# 97. NR 98. DK 99. MI
table(cses_imd$IMD3006)

cses_imd$l1 <- cses_imd$IMD3006
cses_imd$l1 <- ifelse(cses_imd$l1 > 10, NA, cses_imd$l1)
cses_imd$l1 = factor(cses_imd$l1, levels = c(0:10),
                     labels = c("Left/liberal",
                                1,2,3,4,5,6,7,8,9,
                                "Right/conservative"))
table(cses_imd$l1)

# RACE
# 01. WHITE
# 02. ASIAN (INCLUDING SOUTH ASIAN INDIAN, CHINESE, ETC.)
# 03. BLACK
# 04. PACIFIC ISLANDER (MELANESIAN, MICRONESIAN, POLYNESIAN)
# 05. INDIGENOUS / FIRST PEOPLES
# table(cses_imd$IMD2010)

# EMPLOYMENT/LABOR
cses_imd$labor_force <- case_when(
  cses_imd$IMD2014 %in% c(0, 1, 2, 3, 4, 5) ~ 1,
  cses_imd$IMD2014 %in% c(6, 7, 8, 9, 10) ~ 2,
  cses_imd$IMD2014 %in% c(11, 12) ~ 3,
  TRUE ~ NA_real_  # Use NA_real_ for numeric NA values
)

# Create a factor with the three categories
cses_imd$labor_force <- factor(cses_imd$labor_force, levels = c(1,2,3),
                               labels = c("In Labor Force",
                                          "Not in Labor Force",
                                          "Other"))

# COMPULSORY VOTE
cses_imd$compulsory_vote <- case_when(
  cses_imd$IMD5007 %in% c(1, 2, 3) ~ 1,
  cses_imd$IMD5007 %in% c(5) ~ 0,
  TRUE ~ NA_real_  # Use NA_real_ for numeric NA values
)

# REMOVING SOME SPECIFIC CATEGORIES FOR SIMPLICITY OF ATA PRESENTATION
# # -----------------------------------------------------------------------
#cses_imd$IMD3010[cses_imd$IMD3010==6]<-3;
#cses_imd$IMD2004[cses_imd$IMD2004==5]<-NA
#cses_imd$IMD5007[cses_imd$IMD5007==5]<-0; # 4-point
#cses_imd$IMD5052_2<-round(cses_imd$IMD5052_2, 1)

# WEIGHTS
# # -----------------------------------------------------------------------
cses_imd$no_weight <- 1 # UNWEIGHTED (RAW DATA)
table(cses_imd$no_weight)

table(cses_imd$IMD1010_1) # SAMPLE (SELECTION BIAS)
cses_imd$weight_sample <- cses_imd$IMD1010_1

table(cses_imd$IMD1010_2) # DEMOGRAPHIC (NON-RESPONSE BIAS)
cses_imd$weight_demographic <- cses_imd$IMD1010_2 # PREFERRED WEIGHT (DEFAULT)

#table(cses_imd$IMD1010_3) # POLITICAL (VOTING)- NOT USING IT
#cses_imd$weight_political <- cses_imd$IMD1010_3

# SELECTING VARIABLES FOR DATA PLAYGROUND
# # -----------------------------------------------------------------------
vars <- c(
  "gendermc",
  "wealthf",
  "edrerf",
  "age",
  "ur",
  "labor_force",
  "compulsory_vote",
  "pais",
  "wave",
  "pais_num",
  "pais_lab",
  "no_weight",
  "weight_sample",
  "weight_demographic",
  "IMD1008_MOD"
)

# Variable Labels
vars_labels <- read.csv("./Data preprocessing/cses_variable_labels_raw.csv",
                        encoding = "latin1")

# Variable Display for Dropdown Menu (category + question + name)
vars_labels$display_en <- paste0(vars_labels$category_short_en, ": ",
                                 vars_labels$question_short_en,
                                 " (", vars_labels$column_name, ")", sep = "")

vars2 <- vars_labels$column_name
vars3 <- c(vars2, vars)

# Check if all variables are present and subset dataset for shinyapp
vars3 %in% names(cses_imd)
cses_out <- cses_imd[vars3]

# CHECKING INDIVIDUAL VARIABLES
#table(cses_out$IMD2005_1) # remove 789; 6-point
#table(cses_out$IMD2005_2) # remove 789; 4-point
#table(cses_out$IMD2016) # remove 7:9 values; 5-point
#table(cses_out$IMD2019_1) # remove 7:9 values; 2-point
#table(cses_out$IMD3001_PR_1) # remove  9999993 9999995 9999996 9999997 9999998 9999999; 2-point
#table(cses_out$IMD3001_PR_2) # remove  9999993 9999995 9999996 9999997 9999998 9999999; 2-point
#table(cses_out$IMD3001_LH) # remove  9999993 9999995 9999996 9999997 9999998 9999999; 2-point
#table(cses_out$IMD3001_UH) # remove       9999993 9999995 9999996 9999997 9999998 9999999; 2-point
#table(cses_out$IMD3001_TS) # remove 9; 6-point
#table(cses_out$IMD3002_OUTGOV) # remove  9999996 9999997 9999998 9999999; 2-point
#table(cses_out$IMD3002_VS_1) # remove 9; 2-point
#table(cses_out$IMD3002_LR_CSES) # remove  9; 3-point
#table(cses_out$IMD3005_1) # remove  7      8      9; 2-point
#table(cses_out$IMD3010) # remove 789, 6 should be recoded to 3; 5-point
#table(cses_out$IMD3011) # remove 789; 5-point
#table(cses_out$IMD3012) # remove 7 8 9; 5-point
#table(cses_out$IMD3013_1) # remove 789; 3-point with 1,3,5
#table(cses_out$IMD3013_2) # remove 7 8 9; 2-point with 1,2
#table(cses_out$IMD3013_3) # remove 789; 2-point with 4 and 5
#table(cses_out$IMD3014) # remove 6789; 4-point
#table(cses_out$IMD5006_1) # remove 999; TOO MANY
#table(cses_out$IMD5006_2) # remove 999; TOO MANY
#table(cses_out$IMD5013) # ok; 3-point
#table(cses_out$IMD5014) # remove 7; 5-point
#table(cses_out$IMD5032_4) # remove 9; 3-point
#table(cses_out$IMD5033) # remove 9; 3-point
#table(cses_out$IMD5034_2) # remove 6 9; 2-point
#table(cses_out$IMD5035) # remove 999; TOO MANY
#table(cses_out$IMD5048) # ok; 3-point
#table(cses_out$IMD5049) # remove 999; TOO MANY
#table(cses_out$IMD5050_1) # ok; 7-point
#table(cses_out$IMD5051_1) # remove 99 and -88; 10-point
#table(cses_out$IMD5052_2) # remove 99; TOO MANY
#table(cses_out$IMD5053_1) # remove 999999; TOO MANY
#table(cses_out$IMD5054_2) # remove 999; TOO MANY
#table(cses_out$IMD5055_1) # remove 999; TOO MANY
#table(cses_out$IMD5056_2) # remove 99999; TOO MANY
#table(cses_out$IMD5057_1) # remove 9999999999; TOO MANY
#(cses_out$IMD5058_1) # remove 997   999; TOO MANY


# REMOVING NAs/NRs/DKs
# # -----------------------------------------------------------------------
# 95. VOLUNTEERED: HAVEN'T HEARD OF LEFT-RIGHT
# 97. VOLUNTEERED: REFUSED
# 98. VOLUNTEERED: DON'T KNOW WHERE TO PLACE
# 99. MISSING

cses_out <- cses_out %>%
  mutate(across(c(IMD2014, IMD3006,
                  IMD5051_1, IMD5052_2,
                  IMD2014, IMD3006), ~
                  replace(.x, .x %in% c(-88, -77, -66, 95:99), NA)))

# FIX NEGATIVE VALUES
#cses_out$IMD5051_1<-(cses_out$IMD5051_1+10)/2

cses_out <- cses_out %>%
  mutate(across(c(IMD3001, IMD3001_PR_1, IMD3001_PR_2,
                   IMD3001_LH, IMD3001_UH, IMD2001_2,
                   IMD3002_OUTGOV, IMD3002_LR_CSES), ~
                  replace(.x, .x %in% c(9999993:9999999, 9997:9999), NA)))

cses_out <- cses_out %>%
  mutate(across(c(IMD5006_1, IMD5006_2, IMD5035,
                   IMD5049, IMD5053_1, IMD5054_2,
                   IMD5045_1, IMD5055_1, IMD5056_2,
                   IMD5057_1, IMD5058_1), ~
                  replace(.x, .x %in% c(9999999999, 999999, 99999, 997, 998, 999), NA)))


cses_out <- cses_out %>%
  mutate(across(c(IMD3001_TS, IMD3002_VS_1, IMD5032_4,
                   IMD5033, IMD3002_LR_CSES), ~ replace(.x, .x %in% c(9), NA)))

cses_out <- cses_out %>%
  mutate(across(c(IMD2005_1, IMD2005_2, IMD2016,
                   IMD2019_1, IMD3005_1, IMD3011,
                   IMD3012, IMD3013_1, IMD3013_2,
                  IMD3010, IMD3013_3, IMD2002, IMD5036_3,
                  IMD2004, IMD2007), ~ replace(.x, .x %in% c(7:9), NA)))

cses_out <- cses_out %>%
  mutate(across(c(IMD3014, IMD5014, IMD5034_2, IMD2003, IMD2006), ~
                  replace(.x, .x %in% c(6:9), NA)))

# # -----------------------------------------------------------------------
# OUTCOME VARIABLES RECODE (NUMERIC TO LABELS)
# # -----------------------------------------------------------------------
# Function to convert labels to sentence case
to_sentence_case <- function(x) {
  sapply(x, function(s) {
    s <- tolower(s)
    # Capitalize first letter after ) or .
    s <- gsub("([).]\\s*)([a-z])", "\\1\\U\\2", s, perl = TRUE)
    # Also capitalize the very first character if needed
    sub("^(\\w)", "\\U\\1", s, perl = TRUE)
  }, USE.NAMES = FALSE)
}

#  Label output dataset to include the labels through haven package
label_all_for_haven <- function(data, data_out) {
  label_table <- attr(data, "label.table", exact = TRUE)
  data_labeled <- data_out  # Use the manipulated dataset as base

  for (var in names(data_out)) {
    if (!is.null(label_table[[var]])) {
      values <- label_table[[var]]

      # Clean labels: remove number prefixes, apply sentence case
      clean_labels <- sub("^\\d+\\.\\s*", "", names(values))
      clean_labels <- to_sentence_case(clean_labels)

      # Build named vector: values = numeric codes, names = labels
      labelled_vec <- setNames(as.numeric(values), clean_labels)

      # Preserve original variable
      var_data <- data_out[[var]]

      # Convert to haven::labelled
      data_labeled[[var]] <- labelled(var_data, labels = labelled_vec)
    }
  }

  return(data_labeled)
}

cses_out_labels<-label_all_for_haven(cses_imd, cses_out)

# FIXES FOR NOW
# # -----------------------------------------------------------------------
require(labelled)
cses_out_labels$IMD2004[cses_out_labels$IMD2004==5]<-NA

cses_out_labels$IMD3010 <- labelled(
  x = replace(cses_out_labels$IMD3010, cses_out_labels$IMD3010 == 6, 3),
  labels = c(
    "Very satisfied"         = 1,
    "Satisfied"              = 2,
    "Neither"                = 3,
    "Not very satisfied"     = 4,
    "Not all satisfied"      = 5
  )
)

cses_out_labels$IMD5007 <- labelled(
  x = replace(cses_out_labels$IMD5007, cses_out_labels$IMD5007 == 5, 0),
  labels = c(
    "No"                     = 0,
    "Yes; no sanctions"      = 3,
    "Yes; weakly enforced"   = 2,
    "Yes; strictly enforced" = 1
  )
)

cses_out_labels$IMD5052_2<-round(cses_out_labels$IMD5052_2, 1)

# Exporting DATA (.rds lighter file storage)
# # -----------------------------------------------------------------------
# MERGE PAIS_LAB TO CSES_OUT BEFORE EXPORT
pais_lab_merge<-subset(pais_lab, select=c("pais_lab", "pais_nam"))
cses_out_labels <- merge(cses_out_labels, pais_lab_merge, by = "pais_lab")
str(cses_out_labels)

# REMOVING CONTINUOUS VARS FOR NOW...
cses_out_labels<-subset(cses_out_labels, select=-c(IMD3001_TS, IMD5054_2, IMD5057_1, IMD5035,
                                     IMD5056_2, IMD5055_1, IMD5053_1, IMD5052_2,
                                     IMD5006_2, IMD5006_1, IMD5058_1, IMD5049))


# EXPORT
saveRDS(cses_out_labels, "./cses_shiny_data.rds")

# # -----------------------------------------------------------------------
# EXTRACTING RESPONSE OPTIONS FOR VARS_LABELS DATA
# # -----------------------------------------------------------------------
# Define the format_labels function to extract and format ROs
format_labels <- function(label_table) {
  valid_labels <- label_table[label_table > -15 & label_table < 90]  # Exclude special codes / DK / NA / NR / ETC
  formatted <- sapply(seq_along(valid_labels), function(i) {
    paste0("(", valid_labels[i], ") ", names(valid_labels)[i])
  })
  paste(formatted, collapse = " ")
}

# Extract Response Options from Dataset Attributes (label.table)
for (var in names(cses_imd)) {
  if (var %in% vars_labels$column_name) {
    label_table <- attr(cses_imd, "label.table", exact=T)[[var]]
    if (!is.null(label_table)) {
      vars_labels$responses_en[vars_labels$column_name == var] <- format_labels(label_table)
    }
  }
}

table(vars_labels$responses_en=="" | is.na(vars_labels$responses_en))

# FILLING "Question Wording" & "Response Options" for Variables.
# # -----------------------------------------------------------------------
qword_ro<-read.csv("./Data preprocessing/cses_qwording.csv", header=T)

vars_labels <- vars_labels %>%
  left_join(qword_ro, by = "column_name", suffix = c(".old", "")) %>%
  # Select columns from qword_ro and non-duplicated columns from vars_labels
  select(-contains(".old"))


# REMOVING NAs/NRs/DKs from ROs and other CLEANING (mojibake/encoding errors)

#vars_labels$responses_en<-gsub("â€“", ":", vars_labels$responses_en)
#vars_labels$responses_en<-gsub("00.", "0.", vars_labels$responses_en)
#vars_labels$question_short_en<-gsub("ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â€šÂ¬Ã…â€œ", ":", vars_labels$question_short_en)
#vars_labels$question_short_en<-gsub("ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã¢â‚¬Å“", "", vars_labels$question_short_en)
vars_labels$responses_en<-trimws(vars_labels$responses_en)
vars_labels$responses_en<-gsub(" \\(7\\) 7. VOLUNTEERED: REFUSED", "", vars_labels$responses_en)
vars_labels$responses_en<-gsub(" \\(8\\) 8. VOLUNTEERED: DON'T KNOW", "", vars_labels$responses_en)
vars_labels$responses_en<-gsub(" \\(9\\) 9. MISSING", "", vars_labels$responses_en)
vars_labels$responses_en<-gsub(" \\(6\\) 6. OTHER", "", vars_labels$responses_en)
vars_labels$responses_en<-gsub(" \\(6\\) 6. \\[SEE ELECTION STUDY NOTES\\]", "", vars_labels$responses_en)
vars_labels$responses_en<-gsub(" \\(7\\) 7. NOT APPLICABLE \\[NO ALLIANCES PERMITTED\\]", "", vars_labels$responses_en)
vars_labels$responses_en<-gsub(" \\(6\\) 6. NO INTERNATIONAL ELECTION OBSERVERS", "", vars_labels$responses_en)
vars_labels$responses_en<-gsub(" \\(7\\) 7. NOT APPLICABLE", "", vars_labels$responses_en)
vars_labels$responses_en<-gsub("\\(1\\) 1. VERY SATISFIED \\(2\\) 2. FAIRLY SATISFIED \\(4\\) 4. NOT VERY SATISFIED \\(5\\) 5. NOT AT ALL SATISFIED \\(6\\) 6. NEITHER SATISFIED NOR DISSATISFIED",
                               "\\(1\\) 1. VERY SATISFIED \\(2\\) 2. FAIRLY SATISFIED \\(3\\) 3. NEITHER SATISFIED NOR DISSATISFIED \\(4\\) 4. NOT VERY SATISFIED \\(5\\) 5. NOT AT ALL SATISFIED",
                               vars_labels$responses_en)
vars_labels$responses_en<-trimws(vars_labels$responses_en)

# Remove Extra  and WhiteSpaces respectivelly
vars_labels$responses_en <- gsub("(\\s|^)-?\\d+\\.\\s", "\\1", vars_labels$responses_en)
vars_labels$responses_en <- gsub("([).])\\s+", "\\1 ", vars_labels$responses_en)
vars_labels$responses_en<-gsub("\\(5\\) NO", "\\(0\\) NO", vars_labels$responses_en) # COMPULSORY VOTE IMD5007

# Create responses_en_rec
vars_labels$responses_en_rec<-to_sentence_case(vars_labels$responses_en)
vars_labels$question_short_en<-to_sentence_case(vars_labels$question_short_en)
vars_labels$question_short_en <- gsub("([).-])\\s+", "\\1 ", vars_labels$question_short_en)

# EXPORT CSES LABELS
# # -----------------------------------------------------------------------
# REMOVING CONTINUOUS VARS FOR NOW...
vars_labels <- vars_labels[!vars_labels$column_name %in%
                                   c("IMD3001_TS", "IMD5054_2", "IMD5057_1", "IMD5035",
                                     "IMD5056_2", "IMD5055_1", "IMD5053_1", "IMD5052_2",
                                     "IMD5006_2", "IMD5006_1", "IMD5058_1", "IMD5049"), ]

write.csv(vars_labels, "./cses_variable_labels.csv", row.names=F)

# # -----------------------------------------------------------------------
# LABS VECTOR
# # -----------------------------------------------------------------------
labs <- vars_labels$column_name
names(labs) <- vars_labels$display_en
labs[order(names(labs))]
names(vars_labels$column_name) <- vars_labels$display_en
vars_labels$labs2 <- labs
vars_labels$question_en_comp <- paste0(vars_labels$question_en,
                                       vars_labels$responses_en_rec,
                                       sep = " ")

# FINAL EXPORT OF LABELS
# # -----------------------------------------------------------------------
# REMOVING CONTINUOUS VARS FOR NOW...
labs <- labs[!labs %in% c("IMD3001_TS", "IMD5054_2", "IMD5057_1", "IMD5035",
                                   "IMD5056_2", "IMD5055_1", "IMD5053_1", "IMD5052_2",
                                   "IMD5006_2", "IMD5006_1", "IMD5058_1", "IMD5049")]
saveRDS(labs, "./cses_labs.rds")

# # -----------------------------------------------------------------------
# END
# # -----------------------------------------------------------------------
message("Code ended succesfully")


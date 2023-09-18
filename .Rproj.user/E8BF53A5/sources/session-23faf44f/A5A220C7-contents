
.packages = c("readr",        # Reading and writing data
              "tidyr",        # Data tidying and reshaping
              "stringr",      # String manipulation
              "ggplot2",      # Data visualization
              "dplyr",        # Data manipulation and transformation
              "tidyverse",    # Collection of packages for data science
              "caret",        # Classification and regression training
              "reshape2",     # Data reshaping and melting
              "randomForest", # Random forest algorithm for regression
              "keras",        # High-level neural networks in R
              "xgboost",      # Extreme Gradient Boosting
              "reticulate",   # R interface to Python
              "tensorflow"    # Machine learning framework
)
# Install packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session 
lapply(.packages, require, character.only=TRUE)


library(readr)
library(tidyr)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(reshape2)
library(randomForest)
library(keras)
library(xgboost)
library(reticulate)
library(tensorflow)

##### Here are some notes on installing miniconda for the tensorflow ML part of the script
## Unquote below

# reticulate::install_miniconda()
#### I would Try this one bellow first but your system my not be completely compatible 
### I had to do an alternate build
# Unquote below 

# reticulate::conda_create(envname = "r-tensorflow", python = "3.9")
####This one is specific to my computer build 
##### reticulate::conda_install(envname = "r-tensorflow", packages = "tensorflow=2.6.0")


#api downlaod it will block you after a few calls
#url <- "https://data.nola.gov/api/views/9mnw-mbde/rows.csv?accessType=DOWNLOAD"
nola_use_of_force <- read_csv("NOPD_Use_of_Force_Incidents.csv")

#View(nola_use_of_force)
head(nola_use_of_force)
colnames(nola_use_of_force)

length(nola_use_of_force)
nrow(nola_use_of_force)


## rename columns 
colnames(nola_use_of_force) <-  c('PIB_File_Number', 'Date_Occurred', 'Originating_Bureau', 'Division_level', 'Division', 'Unit', 'Working_Status', 
                                  'Shift', 'Investigation_status', 'Disposition', 'Service_Type', 'Light_Condition', 'Weather_Condition', 
                                  'Use_of_Force_Type', 'Use_of_Force_Level', 'Subject_Influencing_Factors', 'Use_of_Force_Effective', 'Officer_Race', 
                                  'Officer_Gender', 'Officer_Age', 'Officer_Years_of_Service', 'Distance_Between', 'Subject_Gender', 'Subject_Ethnicity',          
                                  'Subject_Age', 'Subject_Build', 'Subject_Height', 'Subject_Injured', 'Subject_Hospitalized', 'Subject_Arrested',            
                                  'Subject_Arrest_Charges', 'Use_of_Force_Reason', 'Officer_Injured')


# Remove the specified row for subject splitting discrepancy, officer discrepancy splits remove done inline below
rows_to_remove <- c(669, 1114, 1222, 2833, 3021, 3057, 257, 482, 609, 1086, 1449, 1757, 1939, 2744, 2879)
nola_use_of_force <- nola_use_of_force[-rows_to_remove, ]
# Officer Gender Removes
 pibs <- c( "FTN2017-0026" , "UF2014-0979" , "FTN2019-0400", "FTN2016-0064", "FTN2022-0419")
 nola_use_of_force <- nola_use_of_force[!nola_use_of_force$PIB_File_Number %in% pibs, ]

#Subject injured remove
nola_use_of_force <- subset(nola_use_of_force, nola_use_of_force$PIB_File_Number != "FTN2021-0378")

# before I split the subjects I'm going to create a separate table dividing all the police columns 

# Function to check if all elements in a character vector have the same length
all_same_length <- function(vec) {
  lengths <- sapply(strsplit(vec, "\\s*\\|\\s*"), length)
  all(lengths == lengths[1])
}

# Find rows where entries have different lengths
rows_to_remove <- which(!apply(select(nola_use_of_force,
                                      Use_of_Force_Type, Use_of_Force_Level,
                                      Use_of_Force_Effective,
                                      Officer_Race, Officer_Gender,
                                      Officer_Age, Officer_Years_of_Service, Officer_Injured),
                               1, all_same_length))
print(rows_to_remove)
# Remove rows with discrepancies
nola_use_of_force <- nola_use_of_force[-rows_to_remove, ]
rows_to_remove <- c(35, 124, 3089, 165)
nola_use_of_force <- nola_use_of_force[-rows_to_remove, ]

# Continue with separating rows and mutating as needed
police_table_nola_use_of_force <- nola_use_of_force %>%
  separate_rows(Use_of_Force_Type, Use_of_Force_Level, 
                Use_of_Force_Effective, Officer_Race, Officer_Gender, Officer_Age, Officer_Injured,
                Officer_Years_of_Service, sep = " \\| ") %>%
  mutate(across(c(Use_of_Force_Type, Use_of_Force_Level, 
                  Use_of_Force_Effective, Officer_Race, Officer_Gender, Officer_Age, Officer_Injured,
                  Officer_Years_of_Service),
                ~ ifelse(nchar(.) == 0, first(.), .)))
# View(police_table_nola_use_of_force)

# Separating out all the subjects but keeping them on the main table

subject_table_use_of_force <- nola_use_of_force %>%
  separate_rows(Distance_Between, Subject_Gender, Subject_Ethnicity, Subject_Influencing_Factors,        
                Subject_Age, Subject_Build, Subject_Height, Subject_Injured, Subject_Hospitalized, Subject_Arrested, sep = " \\| ") %>%
  mutate(across(c(Distance_Between, Subject_Gender, Subject_Ethnicity, Subject_Influencing_Factors,          
                  Subject_Age, Subject_Build, Subject_Height, Subject_Injured, Subject_Hospitalized, Subject_Arrested), 
                ~ ifelse(nchar(.) == 0, first(.), .)))


# subject injured change yes interaction to a count
nola_use_of_force$Subject_Injured <- str_count(nola_use_of_force$Subject_Injured, pattern = "Yes")
sum(is.na(nola_use_of_force$Subject_Injured))


# Subject Hospitalized
subject_table_use_of_force$Subject_Injured <- str_count(subject_table_use_of_force$Subject_Injured, pattern = "Yes")
# 0 N/A
sum(is.na(subject_table_use_of_force$Subject_Injured))

# Subject Hospitalized
subject_table_use_of_force$Subject_Hospitalized <- str_count(subject_table_use_of_force$Subject_Hospitalized, pattern = "Yes")
# 0 N/A
sum(is.na(subject_table_use_of_force$Subject_Hospitalized))


# Subject Arrested
subject_table_use_of_force$Subject_Arrested <- str_count(subject_table_use_of_force$Subject_Arrested, pattern = "Yes")
# 0 N/A
sum(is.na(subject_table_use_of_force$Subject_Arrested))


# Officer_Injured
police_table_nola_use_of_force$Officer_Injured <- str_count(police_table_nola_use_of_force$Officer_Injured, pattern = "Yes")
# 0 N/A
sum(is.na(police_table_nola_use_of_force$Officer_Injured))


# Use_of_Force_Effective
# make a numerical variable for effective and ineffective use of force by creating a new column 
police_table_nola_use_of_force$Use_of_Force_Effective <- str_count(police_table_nola_use_of_force$Use_of_Force_Effective, pattern = "Yes")
# 0 N/A
sum(is.na(police_table_nola_use_of_force$Use_of_Force_Effective))


# Officer_Gender
# convert officer involved into its own numerical field, separate the genders
unique(police_table_nola_use_of_force$Officer_Gender)
sum(is.na(police_table_nola_use_of_force$Officer_Gender))


# unfortunately a lot of missing data in these 3 points probably best to delete Moved this to the top
nola_use_of_force$Officer_Female <- str_count(nola_use_of_force$Officer_Gender, pattern = "Female")
nola_use_of_force$Officer_Male <- str_count(nola_use_of_force$Officer_Gender, pattern = "Male")
nola_use_of_force$Officer_Number <- str_count(nola_use_of_force$Officer_Gender, pattern = "Male") + str_count(nola_use_of_force$Officer_Gender, pattern = "Female")

hist(nola_use_of_force$Officer_Male, col = "blue", main = "Number of Officer's by Gender per Incident ", xlab = "Values")
hist(nola_use_of_force$Officer_Female, col = "red", add = TRUE)

# changing the count to various  categories of gender amalgamations of of officers by incident 
nola_use_of_force <- nola_use_of_force %>%
  mutate(Officer_Gender = case_when(
    Officer_Male / Officer_Number >= 0.51 ~ "Majority_Male",
    Officer_Female / Officer_Number >= 0.51  ~ "Majority_Female",
    Officer_Male / Officer_Number == 0.50 & Officer_Female / Officer_Number == 0.50  ~ "Male&Female",
    TRUE ~ "Mixed"
  )) %>%
  mutate(Officer_Gender = case_when(
    is.na(Officer_Male) | is.na(Officer_Female)  ~ "Mixed",
    Officer_Gender == "Majority_Male" & Officer_Male == Officer_Number ~ "Male",
    Officer_Gender == "Majority_Female" & Officer_Female == Officer_Number ~ "Female",
    TRUE ~ Officer_Gender
  ))

# Creating a numerical value for mean use of force effective in the main table
# the self reporting here may be the bias, seem like all use of for is "Effective"
nola_use_of_force$Use_of_Force_Effective <- str_count(nola_use_of_force$Use_of_Force_Effective, pattern = "Yes")
# using a mean of officers effective use of force 
nola_use_of_force$Use_of_Force_Effective <- nola_use_of_force$Use_of_Force_Effective / nola_use_of_force$Officer_Number 
  
hist(nola_use_of_force$Use_of_Force_Effective, col = "blue", 
     main = "Mean Police Reported Force \n Effectivness by Incident", xlab = "1 = Effective, 0 = None Effective ")  

# Subject_Gender
# convert subject involved into its own numerical field, separate the genders
unique(nola_use_of_force$Subject_Gender)
sum(is.na(nola_use_of_force$Subject_Gender))

#nola_check <-nola_use_of_force[is.na(nola_use_of_force$Subject_Gender), ]
# 26 accounts with a lot of missing data. Don't want to assume gender but a lot 
# of data is from 8 to 10 years ago, maybe excluding trans, nb
# people by deleting but more that just the gender descriptors are missing. Moved to the top for delete. 

# clean some errors
nola_use_of_force$Subject_Gender <- ifelse(is.na(nola_use_of_force$Subject_Gender), "Not_Specified", nola_use_of_force$Subject_Gender)
nola_use_of_force$Subject_Gender <- gsub("\\bm\\b", "Male", nola_use_of_force$Subject_Gender)
# nola_use_of_force$Subject_Gender <- gsub("\\| Male", "Male", nola_use_of_force$Subject_Gender)
nola_use_of_force$Subject_Gender <- gsub("femaale", "Female", nola_use_of_force$Subject_Gender)


# Created new fields this unfortunately seems the easiest way to clean for data science. A little confused 
# how to train models across multiple tables, would be nice to just use the subject table and then connect 
# through the key value. Maybe next time! 

 nola_use_of_force$Subject_Female <- str_count(nola_use_of_force$Subject_Gender, pattern = "Female")
 nola_use_of_force$Subject_Male <- str_count(nola_use_of_force$Subject_Gender, pattern = "Male")
 nola_use_of_force$Subject_Sex_Unk <- str_count(nola_use_of_force$Subject_Gender, pattern = "Sex-Unk")
 nola_use_of_force$Subject_Not_Specified <- str_count(nola_use_of_force$Subject_Gender, pattern = "Not_Specified")
 
 # Subject_Number
 # Create just a number of subjects involvde per incident
 nola_use_of_force$Subject_Number <- str_count(nola_use_of_force$Subject_Gender, pattern = "Male") + 
   str_count(nola_use_of_force$Subject_Gender, pattern = "Female") + 
   str_count(nola_use_of_force$Subject_Gender, pattern = "Sex-Unk") +
   str_count(nola_use_of_force$Subject_Gender, pattern = "Not_Specified")

# Making a Single column with gender frequencies probably do a % thing next time
nola_use_of_force <- nola_use_of_force %>%
  mutate(Subject_Gender = case_when(
    Subject_Male / Subject_Number >= 0.51 ~ "Majority_Male",
    Subject_Female / Subject_Number >= 0.51  ~ "Majority_Female",
    Subject_Sex_Unk / Subject_Number >= 0.51  ~ "Majority_Sex_Unk",
    Subject_Not_Specified / Subject_Number >= 0.51  ~ "Majority_Subject_Not_Specified",
    Subject_Male / Subject_Number == 0.50 & Subject_Female / Subject_Number == 0.50  ~ "Male&Female",
    Subject_Male / Subject_Number == 0.50 & Subject_Sex_Unk / Subject_Number == 0.50  ~ "Male&Sex_Unk",
    Subject_Male / Subject_Number == 0.50 & Subject_Not_Specified / Subject_Number == 0.50  ~ "Male&Not_Specified",
    Subject_Female / Subject_Number == 0.50 & Subject_Not_Specified / Subject_Number == 0.50  ~ "Female&Not_Specified",
    Subject_Female / Subject_Number == 0.50 & Subject_Sex_Unk / Subject_Number == 0.50  ~ "Female&Sex_Unk",
    Subject_Sex_Unk / Subject_Number == 0.50 & Subject_Not_Specified / Subject_Number == 0.50  ~ "Sex_Unk&Not_Specified",
    TRUE ~ "Mixed"
  )) %>%
  mutate(Subject_Gender = case_when(
    is.na(Subject_Male) | is.na(Subject_Female) | is.na(Subject_Sex_Unk) | is.na(Subject_Not_Specified) ~ "Mixed",
    Subject_Gender == "Majority_Male" & Subject_Male == Subject_Number ~ "Male",
    Subject_Gender == "Majority_Female" & Subject_Female == Subject_Number ~ "Female",
    Subject_Gender == "Majority_Sex_Unk" & Subject_Male == Subject_Number ~ "Sex_Unk",
    Subject_Gender == "Majority_Subject_Not_Specified" & Subject_Female == Subject_Number ~ "Not_Specified",
    TRUE ~ Officer_Gender
  ))

# Cleaning subject gender table
subject_table_use_of_force$Subject_Gender <- ifelse(is.na(subject_table_use_of_force$Subject_Gender), "Not_Specified", subject_table_use_of_force$Subject_Gender)
subject_table_use_of_force$Subject_Gender <- gsub("\\bm\\b", "Male", subject_table_use_of_force$Subject_Gender)
subject_table_use_of_force$Subject_Gender <- gsub("\\| Male", "Male", subject_table_use_of_force$Subject_Gender)
subject_table_use_of_force$Subject_Gender <- gsub("femaale", "Female", subject_table_use_of_force$Subject_Gender)
unique(subject_table_use_of_force$Subject_Gender)



# Officer_race
#  changing the race column to be white, black, Hispanic, pacific islander, American Indian, and various mixes 
unique(nola_use_of_force$Officer_Race)
sum(is.na(nola_use_of_force$Officer_Race))
# Change N/A to not specified 
nola_use_of_force$Officer_Race <- ifelse(is.na(nola_use_of_force$Officer_Race), "Not Specified", nola_use_of_force$Officer_Race)
# do it for the police_table_nola_use_of_force
police_table_nola_use_of_force$Officer_Race <- ifelse(is.na(police_table_nola_use_of_force$Officer_Race), "Not Specified", police_table_nola_use_of_force$Officer_Race)

# count the race per incident. Again probably do percentages next time
nola_use_of_force$Officer_White <- str_count(nola_use_of_force$Officer_Race, pattern = "White")
nola_use_of_force$Officer_Black <- str_count(nola_use_of_force$Officer_Race, pattern = "Black")
nola_use_of_force$Officer_Hispanic <- str_count(nola_use_of_force$Officer_Race, pattern = "Hispanic")
nola_use_of_force$Officer_Asian_Pacific_Islander <- str_count(nola_use_of_force$Officer_Race, pattern = "Asian/Pacific Islander")
nola_use_of_force$Officer_American_Indian <- str_count(nola_use_of_force$Officer_Race, pattern = "American Indian/Alaska Native")
nola_use_of_force$Officer_Race_Unknown <- str_count(nola_use_of_force$Officer_Race, pattern = "Not Specified")

nola_use_of_force <- nola_use_of_force %>%
  mutate(Officer_Race = case_when(
    Officer_White / Officer_Number >= 0.51 ~ "Majority_White",
    Officer_Black / Officer_Number >= 0.51  ~ "Majority_Black",
    Officer_Hispanic / Officer_Number >= 0.51  ~ "Majority_Hispanic", 
    Officer_Asian_Pacific_Islander / Officer_Number >= 0.51  ~ "Majority_Asian_Pacific_Islander",
    Officer_American_Indian / Officer_Number >= 0.51  ~ "Majority_American_Indian",
    Officer_Race_Unknown / Officer_Number >= 0.51  ~ "Majority_Race_Unknown",
    Officer_White / Officer_Number == 0.50 & Officer_Black / Officer_Number == 0.50  ~ "White&Black",
    Officer_White / Officer_Number == 0.50 & Officer_Hispanic / Officer_Number == 0.50  ~ "White&Hispanic",
    Officer_White / Officer_Number == 0.50 & Officer_Asian_Pacific_Islander / Officer_Number == 0.50  ~ "White&Asian_Pacific_Islander",
    Officer_White / Officer_Number == 0.50 & Officer_American_Indian / Officer_Number == 0.50  ~ "White&American_Indian",
    Officer_White / Officer_Number == 0.50 & Officer_Race_Unknown / Officer_Number == 0.50  ~ "White&Race_Unknown",
    Officer_Black / Officer_Number == 0.50 & Officer_Hispanic / Officer_Number == 0.50  ~ "Black&Hispanic",
    Officer_Black / Officer_Number == 0.50 & Officer_Asian_Pacific_Islander / Officer_Number == 0.50  ~ "Black&Asian_Pacific_Islander",
    Officer_Black / Officer_Number == 0.50 & Officer_American_Indian / Officer_Number == 0.50  ~ "Black&American_Indian",
    Officer_Black / Officer_Number == 0.50 & Officer_Race_Unknown / Officer_Number == 0.50  ~ "Black&Race_Unknown",
    Officer_Hispanic / Officer_Number == 0.50 & Officer_Asian_Pacific_Islander / Officer_Number == 0.50  ~ "Hispanic&Asian_Pacific_Islander",
    Officer_Hispanic / Officer_Number == 0.50 & Officer_American_Indian / Officer_Number == 0.50  ~ "Hispanic&American_Indian",
    Officer_Hispanic / Officer_Number == 0.50 & Officer_Race_Unknown / Officer_Number == 0.50  ~ "Hispanic&Race_Unknown",
    Officer_Asian_Pacific_Islander / Officer_Number == 0.50 & Officer_American_Indian / Officer_Number == 0.50  ~ "Asian_Pacific&American_Indian",
    Officer_American_Indian / Officer_Number == 0.50 & Officer_Race_Unknown / Officer_Number == 0.50  ~ "Asian_Pacific&Race_Unknown",
    Officer_American_Indian / Officer_Number == 0.50 & Officer_Race_Unknown / Officer_Number == 0.50  ~ "American_Indian&Race_Unknown",
    
    TRUE ~ "Mixed"
  )) %>%
  mutate(Officer_Race = case_when(
    is.na(Officer_White) | is.na(Officer_Black) | is.na(Officer_Hispanic) |
      is.na(Officer_Asian_Pacific_Islander) | is.na(Officer_American_Indian) |
      is.na(Officer_Race_Unknown) ~ "Mixed",
    Officer_Race == "Majority_White" & Officer_White == Officer_Number ~ "White",
    Officer_Race == "Majority_Black" & Officer_Black == Officer_Number ~ "Black",
    Officer_Race == "Majority_Hispanic" & Officer_Hispanic == Officer_Number ~ "Hispanic",
    Officer_Race == "Majority_Asian_Pacific_Islander" & Officer_Asian_Pacific_Islander == Officer_Number ~ "Asian_Pacific_Islander",
    Officer_Race == "Majority_American_Indian" & Officer_American_Indian == Officer_Number ~ "American_Indian",
    Officer_Race == "Majority_Race_Unknown" & Officer_Race_Unknown == Officer_Number ~ "Race_Unknown",
    TRUE ~ Officer_Race
  ))


# Create new column for subjects races
unique(nola_use_of_force$Subject_Ethnicity)
sum(is.na(nola_use_of_force$Subject_Ethnicity))
# nola_check <-nola_use_of_force[is.na(nola_use_of_force$Subject_Ethnicity), ]
#View(nola_check)
# there are three sex unique in the stack of 27 and some injuries so I am going to 
# create a race_unknown column like above for these rows

# Cleaning
nola_use_of_force$Subject_Ethnicity<- gsub("\\bW\\b", "White", nola_use_of_force$Subject_Ethnicity)
nola_use_of_force$Subject_Ethnicity<- gsub("\\bw\\b", "White", nola_use_of_force$Subject_Ethnicity)
nola_use_of_force$Subject_Ethnicity <- gsub("black", "Black", nola_use_of_force$Subject_Ethnicity)

nola_use_of_force$Subject_Ethnicity <- ifelse(is.na(nola_use_of_force$Subject_Ethnicity), "Race-Unknown", nola_use_of_force$Subject_Ethnicity)


nola_use_of_force$Subject_White <- str_count(nola_use_of_force$Subject_Ethnicity, pattern = "White")
nola_use_of_force$Subject_Black <- str_count(nola_use_of_force$Subject_Ethnicity, pattern = "Black")
nola_use_of_force$Subject_Hispanic <- str_count(nola_use_of_force$Subject_Ethnicity, pattern = "Hispanic")
nola_use_of_force$Subject_Asian <- str_count(nola_use_of_force$Subject_Ethnicity, pattern = "Asian")
nola_use_of_force$Subject_Indian <- str_count(nola_use_of_force$Subject_Ethnicity, pattern = "Indian")
nola_use_of_force$Subject_Race_Unknown <- str_count(nola_use_of_force$Subject_Ethnicity, pattern = "Race-Unknown")



nola_use_of_force <- nola_use_of_force %>%
  mutate(Subject_Ethnicity = case_when(
    Subject_White / Subject_Number >= 0.51 ~ "Majority_White",
    Subject_Black / Subject_Number >= 0.51  ~ "Majority_Black",
    Subject_Hispanic / Subject_Number >= 0.51  ~ "Majority_Hispanic", 
    Subject_Asian / Subject_Number >= 0.51  ~ "Majority_Asian",
    Subject_Indian / Subject_Number >= 0.51  ~ "Majority_Indian",
    Subject_Race_Unknown / Subject_Number >= 0.51  ~ "Majority_Race_Unknown",
    Subject_White / Subject_Number == 0.50 & Subject_Black / Subject_Number == 0.50  ~ "White&Black",
    Subject_White / Subject_Number == 0.50 & Subject_Hispanic / Subject_Number == 0.50  ~ "White&Hispanic",
    Subject_White / Subject_Number == 0.50 & Subject_Asian / Subject_Number == 0.50  ~ "White&Asian",
    Subject_White / Subject_Number == 0.50 & Subject_Indian / Subject_Number == 0.50  ~ "White&Indian",
    Subject_White / Subject_Number == 0.50 & Subject_Race_Unknown / Subject_Number == 0.50  ~ "White&Race_Unknown",
    Subject_Black / Subject_Number == 0.50 & Subject_Hispanic / Subject_Number == 0.50  ~ "Black&Hispanic",
    Subject_Black / Subject_Number == 0.50 & Subject_Asian / Subject_Number == 0.50  ~ "Black&Asian",
    Subject_Black / Subject_Number == 0.50 & Subject_Indian / Subject_Number == 0.50  ~ "Black&Indian",
    Subject_Black / Subject_Number == 0.50 & Subject_Race_Unknown / Subject_Number == 0.50  ~ "Black&Race_Unknown",
    Subject_Hispanic / Subject_Number == 0.50 & Subject_Asian / Subject_Number == 0.50  ~ "Hispanic&Asian",
    Subject_Hispanic / Subject_Number == 0.50 & Subject_Indian / Subject_Number == 0.50  ~ "Hispanic&Indian",
    Subject_Hispanic / Subject_Number == 0.50 & Subject_Race_Unknown / Subject_Number == 0.50  ~ "Hispanic&Race_Unknown",
    Subject_Asian / Subject_Number == 0.50 & Subject_Indian / Subject_Number == 0.50  ~ "Asian&Indian",
    Subject_Asian / Subject_Number == 0.50 & Subject_Race_Unknown / Subject_Number == 0.50  ~ "Asian&Race_Unknown",
    Subject_Indian / Subject_Number == 0.50 & Subject_Race_Unknown / Subject_Number == 0.50  ~ "Indian&Race_Unknown",
    TRUE ~ "Mixed"
  )) %>%
  mutate(Subject_Ethnicity = case_when(
    is.na(Subject_White) | is.na(Subject_Black) | is.na(Subject_Hispanic) |
      is.na(Subject_Asian) | is.na(Subject_Indian) |
      is.na(Subject_Race_Unknown) ~ "Mixed",
    Subject_Ethnicity == "Majority_White" & Subject_White == Subject_Number ~ "White",
    Subject_Ethnicity == "Majority_Black" & Subject_Black == Subject_Number ~ "Black",
    Subject_Ethnicity == "Majority_Hispanic" & Subject_Hispanic == Subject_Number ~ "Hispanic",
    Subject_Ethnicity == "Majority_Asian" & Subject_Asian == Subject_Number ~ "Asian",
    Subject_Ethnicity == "Majority_Indian" & Subject_Indian == Subject_Number ~ "Indian",
    Subject_Ethnicity == "Majority_Race_Unknown" & Subject_Race_Unknown == Subject_Number ~ "Race_Unknown",
    TRUE ~ Subject_Ethnicity
  ))

# clean subject table subject ethnicity 
subject_table_use_of_force$Subject_Ethnicity<- gsub("\\bW\\b", "White", subject_table_use_of_force$Subject_Ethnicity)
subject_table_use_of_force$Subject_Ethnicity<- gsub("\\bw\\b", "White", subject_table_use_of_force$Subject_Ethnicity)
subject_table_use_of_force$Subject_Ethnicity <- gsub("black", "Black", subject_table_use_of_force$Subject_Ethnicity)
subject_table_use_of_force$Subject_Ethnicity <- gsub("^\\s*\\|\\s*|\\s*\\|\\s*$", "", subject_table_use_of_force$Subject_Ethnicity)
subject_table_use_of_force$Subject_Ethnicity <- ifelse(is.na(subject_table_use_of_force$Subject_Ethnicity), "Race-Unknown", subject_table_use_of_force$Subject_Ethnicity)
unique(subject_table_use_of_force$Subject_Ethnicity)

## average officer age 

nola_use_of_force$Officer_Age <- strsplit(nola_use_of_force$Officer_Age, "\\|")
nola_use_of_force$Officer_Age <- lapply(nola_use_of_force$Officer_Age, as.numeric)

# Calculate the average for each row
nola_use_of_force$Officer_Age <- sapply(nola_use_of_force$Officer_Age, mean)
sum(is.na(nola_use_of_force$Officer_Age))
sum(is.na(police_table_nola_use_of_force$Officer_Age))
print(mean(nola_use_of_force$Officer_Age, na.rm = TRUE))
# There are no n/a but leaving this bellow if so going to put in the mean age of the column 
nola_use_of_force$Officer_Age <- ifelse(is.na(nola_use_of_force$Officer_Age), mean(nola_use_of_force$Officer_Age, na.rm = TRUE), nola_use_of_force$Officer_Age)
nola_use_of_force$Officer_Age <- round(nola_use_of_force$Officer_Age)


# subject age 
# A bunch of N/A's going to add the mean age
 sum(is.na(nola_use_of_force$Subject_Age))
 sum(is.na(subject_table_use_of_force$Subject_Age))
 
sum(subject_table_use_of_force$Subject_Age == 0, na.rm = TRUE)
sum(is.na(subject_table_use_of_force$Subject_Age))
unique(subject_table_use_of_force$Subject_Age)
# Cleaning 
subject_table_use_of_force$Subject_Age <- gsub("^\\s*\\|\\s*|\\s*\\|\\s*|-", "", subject_table_use_of_force$Subject_Age)

subject_table_use_of_force$Subject_Age <- as.numeric(subject_table_use_of_force$Subject_Age)

# Calculate the mean of non-N/A and non-zero values
mean_age <- mean(subject_table_use_of_force$Subject_Age[!is.na(subject_table_use_of_force$Subject_Age) & subject_table_use_of_force$Subject_Age != 0], na.rm = TRUE)
print(mean_age)


# Replace N/A and zero values with the calculated mean will error because i used the same mean for both
subject_table_use_of_force$Subject_Age[is.na(nola_use_of_force$Subject_Age) | subject_table_use_of_force$Subject_Age == 0] <- round(mean_age)

# probably my most normal looking data. But should be pretty normal to population age
# A little spike where avg. is added
hist(subject_table_use_of_force$Subject_Age, col = "blue", main = "Aggregated Subject Age ", xlab = "Age")

## Adding average subject age for 0 and NA entries in incidences table 

 nola_use_of_force$Subject_Age <- strsplit(nola_use_of_force$Subject_Age, "\\|")
 nola_use_of_force$Subject_Age <- lapply(nola_use_of_force$Subject_Age, as.numeric)
 nola_use_of_force$Subject_Age <- sapply(nola_use_of_force$Subject_Age, mean)
 # Replace NAs and 0 entries in the Subject_Age column with the calculated mean
 nola_use_of_force$Subject_Age[is.na(nola_use_of_force$Subject_Age) | nola_use_of_force$Subject_Age == 0] <- mean_age
 nola_use_of_force$Subject_Age <- round(nola_use_of_force$Subject_Age)
 hist(nola_use_of_force$Subject_Age)
 
# Years of officer service average

sum(is.na(nola_use_of_force$Officer_Years_of_Service))
nola_use_of_force <- nola_use_of_force %>%
  mutate(Officer_Years_of_Service = strsplit(Officer_Years_of_Service, "\\|")) %>%
  mutate(Officer_Years_of_Service =  lapply(Officer_Years_of_Service, as.numeric)) %>%
# Split the field into separate values and use mean of all officers in incident   
  mutate(Officer_Years_of_Service = sapply(Officer_Years_of_Service, mean))

# Applying the mean to 3 N\A's
Years_of_Service_mean <- round(mean(nola_use_of_force$Officer_Years_of_Service, na.rm = TRUE))

print(Years_of_Service_mean)
nola_use_of_force$Officer_Years_of_Service <- ifelse(is.na(nola_use_of_force$Officer_Years_of_Service), Years_of_Service_mean , nola_use_of_force$Officer_Years_of_Service)
nola_use_of_force$Officer_Years_of_Service <-round(nola_use_of_force$Officer_Years_of_Service)
hist(nola_use_of_force$Officer_Years_of_Service, col = "blue", main = "Officer's Years in Service \n Mean per Incident", xlab = "Years")
#There seems to be a bit of an abnormal spike in data for incidences involving officers between 5 and 10 years of service

# Doing the same for police table
sum(is.na(police_table_nola_use_of_force$Officer_Years_of_Service))
unique(police_table_nola_use_of_force$Officer_Years_of_Service)
# cleaning
police_table_nola_use_of_force$Officer_Years_of_Service <- gsub("^\\s*\\|\\s*|\\s*\\|\\s*|-", "", police_table_nola_use_of_force$Officer_Years_of_Service)
police_table_nola_use_of_force$Officer_Years_of_Service <- as.numeric(police_table_nola_use_of_force$Officer_Years_of_Service)
police_table_nola_use_of_force$Officer_Years_of_Service <- ifelse(is.na(police_table_nola_use_of_force$Officer_Years_of_Service), Years_of_Service_mean , police_table_nola_use_of_force$Officer_Years_of_Service)
sum(is.na(police_table_nola_use_of_force$Officer_Years_of_Service))

# Definitely a spike of between 5 and 10 years in service, probably when most cops are on the beat
hist(police_table_nola_use_of_force$Officer_Years_of_Service, col = "blue", main = "Officer's Years in Service \n Aggregated", xlab = "Years")

# Use of force Level 
# Just taking the highest use of force id you have a high level use of 
# force as a group that is the incident level use of force, there really isn't 
# that many self reported high level use of force

unique(nola_use_of_force$Use_of_Force_Level)
sum(is.na(nola_use_of_force$Use_of_Force_Level))
nola_use_of_force <- nola_use_of_force %>% 
  mutate(Use_of_Force_Level = str_replace_all(Use_of_Force_Level,c("L1" = "1", "L2" = "2", "L3" = "3", "L4" = "4"))) %>%
  mutate(Use_of_Force_Level = str_replace(Use_of_Force_Level, ".*4.*", "4")) %>%
  mutate(Use_of_Force_Level = str_replace(Use_of_Force_Level, ".*3.*", "3")) %>%
  mutate(Use_of_Force_Level = str_replace(Use_of_Force_Level, ".*2.*", "2")) %>%
  mutate(Use_of_Force_Level = str_replace(Use_of_Force_Level, ".*1.*", "1")) %>%
  mutate(Use_of_Force_Level = as.numeric(gsub("[^0-9]", "", Use_of_Force_Level)))
 
table(nola_use_of_force$Use_of_Force_Level)
hist(nola_use_of_force$Use_of_Force_Level, col = "pink", main = "Higest Level of Force \n per Incident", xlab = "4 = High, 1 = Low")

# Clean the police table level of force
unique(police_table_nola_use_of_force$Use_of_Force_Level)
sum(is.na(police_table_nola_use_of_force$Use_of_Force_Level))
police_table_nola_use_of_force <- police_table_nola_use_of_force %>% 
  mutate(Use_of_Force_Level = str_replace(Use_of_Force_Level, ".*4.*", "4")) %>%
  mutate(Use_of_Force_Level = str_replace(Use_of_Force_Level, ".*3.*", "3")) %>%
  mutate(Use_of_Force_Level = str_replace(Use_of_Force_Level, ".*2.*", "2")) %>%
  mutate(Use_of_Force_Level = str_replace(Use_of_Force_Level, ".*1.*", "1")) %>%
  mutate(Use_of_Force_Level = as.numeric(gsub("[^0-9]", "", Use_of_Force_Level)))

hist(police_table_nola_use_of_force$Use_of_Force_Level, col = "pink", main = "Level of Force \n Aggregated", xlab = "4 = High, 1 = Low")

# Distance between 
# Getting an average distance between officer and subject
sum(is.na(nola_use_of_force$Distance_Between))
sum(is.na(police_table_nola_use_of_force$Distance_Between))

nola_use_of_force <- nola_use_of_force %>% 
  mutate(Distance_Between = str_replace_all(Distance_Between, "feet to ", "|")) %>%  
  mutate(Distance_Between = str_replace_all(Distance_Between, "feet", "")) %>%
  mutate(Distance_Between = strsplit(Distance_Between, "\\|")) %>%
  mutate(Distance_Between =  lapply(Distance_Between, as.numeric)) %>%
  mutate(Distance_Between = sapply(Distance_Between, mean))


police_table_nola_use_of_force <- police_table_nola_use_of_force %>% 
  mutate(Distance_Between = str_replace_all(Distance_Between, "feet to ", "|")) %>%  
  mutate(Distance_Between = str_replace_all(Distance_Between, "feet", "")) %>%
  mutate(Distance_Between = strsplit(Distance_Between, "\\|")) %>%
  mutate(Distance_Between =  lapply(Distance_Between, as.numeric))%>%
  mutate(Distance_Between = sapply(Distance_Between, mean))



# turning (116) N/A to mean 
distance_mean <- mean(nola_use_of_force$Distance_Between, na.rm = TRUE)
distance_mean
nola_use_of_force$Distance_Between <- ifelse(is.na(nola_use_of_force$Distance_Between), distance_mean , nola_use_of_force$Distance_Between)
nola_use_of_force$Distance_Between <-round(nola_use_of_force$Distance_Between)
hist(nola_use_of_force$Distance_Between, col = "purple", main = "Mean Distance Between \n per Incident", xlab = "Feet")

# Using the same mean between tables
police_table_nola_use_of_force$Distance_Between <- ifelse(is.na(police_table_nola_use_of_force$Distance_Between), distance_mean , police_table_nola_use_of_force$Distance_Between)
police_table_nola_use_of_force$Distance_Between <-round(police_table_nola_use_of_force$Distance_Between)
hist(police_table_nola_use_of_force$Distance_Between, col = "purple", main = "Mean Distance Between \n Aggregated", xlab = "Feet")

# Subject Height
# A lot of N/A (185) going to use mean again
sum(is.na(nola_use_of_force$Subject_Height))

# creating an average subject height
convert_to_inches <- function(height) {
  feet <- as.numeric(sub("'.*", "", height))
  inches <- as.numeric(sub(".*'(\\d+)''", "\\1", height))
  total_inches <- (feet * 12) + inches
  return(total_inches)
}

unique(nola_use_of_force$Subject_Height)
# Parsing and cleaning
nola_use_of_force <- nola_use_of_force %>% 
  mutate(Subject_Height = str_replace_all(Subject_Height, ">", "")) %>%  
  mutate(Subject_Height = str_replace_all(Subject_Height, "<", "")) %>%  
  mutate(Subject_Height = str_replace_all(Subject_Height, "to", "|")) %>%  
  mutate(Subject_Height = strsplit(Subject_Height, "\\|")) %>%
  mutate(Subject_Height = sapply(Subject_Height, convert_to_inches)) %>% 
  mutate(Subject_Height =  lapply(Subject_Height, as.numeric)) %>%
  mutate(Subject_Height = sapply(Subject_Height, mean)) 


sub_height_mean <- mean(nola_use_of_force$Subject_Height, na.rm = TRUE)
nola_use_of_force$Subject_Height <- ifelse(is.na(nola_use_of_force$Subject_Height), sub_height_mean , nola_use_of_force$Subject_Height)
nola_use_of_force$Subject_Height <-round(nola_use_of_force$Subject_Height)

# Fairly normal but not very differentiated, rounded reporting.
hist(nola_use_of_force$Subject_Height, col = "yellow", main = "Mean Subject Height \n per Incident", xlab = "Inches")

# Doing the same for the subject height table (281) N/A turned into mean
unique(subject_table_use_of_force$Subject_Height)

subject_table_use_of_force <- subject_table_use_of_force %>% 
  mutate(Subject_Height = str_replace_all(Subject_Height, ">", "")) %>%  
  mutate(Subject_Height = str_replace_all(Subject_Height, "<", "")) %>%  
  mutate(Subject_Height = str_replace_all(Subject_Height, "to", "|")) %>%  
  mutate(Subject_Height = strsplit(Subject_Height, "\\|")) %>%
  mutate(Subject_Height = sapply(Subject_Height, convert_to_inches)) %>% 
  mutate(Subject_Height =  lapply(Subject_Height, as.numeric)) %>%
  mutate(Subject_Height = sapply(Subject_Height, mean)) 

sum(is.na(subject_table_use_of_force$Subject_Height))

# Didn't cross over means for this feature from incident table 
sub_height_mean <- mean(subject_table_use_of_force$Subject_Height, na.rm = TRUE)
subject_table_use_of_force$Subject_Height <- ifelse(is.na(subject_table_use_of_force$Subject_Height), sub_height_mean , subject_table_use_of_force$Subject_Height)
subject_table_use_of_force$Subject_Height <-round(subject_table_use_of_force$Subject_Height)

hist(subject_table_use_of_force$Subject_Height, col = "yellow", main = "Subject Height \n Aggregated", xlab = "Inches")

# Subject build  
# creating an average subject build
unique(nola_use_of_force$Subject_Build)
# in the reporting of multiple subjects, the build is always the same probably due to poor reporting 
# The likelihood of only the same type of build of people being encounters as subjects in an incident is questionable
# easy clean though

nola_use_of_force <- nola_use_of_force %>% 
  mutate(Subject_Build = str_replace(Subject_Build, "Small.*", "Small")) %>%
  mutate(Subject_Build = str_replace(Subject_Build, "Medium.*", "Medium")) %>%
  mutate(Subject_Build = str_replace(Subject_Build, "Large.*", "Large")) %>%
  mutate(Subject_Build = str_replace(Subject_Build, "XLarge.*", "XLarge")) 

  nola_use_of_force$Subject_Build
  sum(is.na(nola_use_of_force$Subject_Build))
 
  # adding n/a to medium
  nola_use_of_force$Subject_Build <- ifelse(is.na(nola_use_of_force$Subject_Build), "Medium" , nola_use_of_force$Subject_Build)
  
 pie(table(nola_use_of_force$Subject_Build), main = "Subject Build \n Incident Mean")

 
subject_table_use_of_force <- subject_table_use_of_force %>% 
   mutate(Subject_Build = str_replace(Subject_Build, "Small.*", "Small")) %>%
   mutate(Subject_Build = str_replace(Subject_Build, "Medium.*", "Medium")) %>%
   mutate(Subject_Build = str_replace(Subject_Build, "Large.*", "Large")) %>%
   mutate(Subject_Build = str_replace(Subject_Build, "XLarge.*", "XLarge")) 
 
 subject_table_use_of_force$Subject_Build
 sum(is.na(subject_table_use_of_force$Subject_Build))
 
 # adding n/a to medium
 subject_table_use_of_force$Subject_Build <- ifelse(is.na(subject_table_use_of_force$Subject_Build), "Medium" , subject_table_use_of_force$Subject_Build)
 
 pie(table(subject_table_use_of_force$Subject_Build), main = "Subject Build \n Aggregated")

 
 # Division
unique(nola_use_of_force$Division_level)
  
# Removing single N/A from Division_level not there any more
sum(is.na(nola_use_of_force$Division_level))
  
# Moved this piB to the top for delete
# nola_use_of_force <- subset(nola_use_of_force, nola_use_of_force$PIB_File_Number != "UF2014-0979")
 
# organizing 
unique(nola_use_of_force$Division)
sum(is.na(nola_use_of_force$Division))

nola_use_of_force <- nola_use_of_force %>% 
  mutate(Division = str_replace(Division,  "B Platoon", "BPlatoon")) %>%
  mutate(Division = str_replace(Division,  "\\bB\\b", "B Platoon")) %>%
  mutate(Division = str_replace(Division,  "\\bb\\b" , "B Platoon")) %>%
  mutate(Division = str_replace(Division,  "BPlatoon", "B Platoon")) %>%
  mutate(Division = str_replace(Division, "\\bAdmin\\b", "Administration")) %>%
  mutate(Division = str_replace(Division, "DIU Persons", "D.I.U.")) %>%
  mutate(Division = str_replace(Division, "DIU SPEC", "D.I.U.")) %>%
  mutate(Division = str_replace(Division, "DIU Property", "D.I.U.")) %>%
  mutate(Division = str_replace(Division, "DIU Staff", "D.I.U.")) %>%
  mutate(Division = str_replace(Division, "D.I.U", "District Investigations Unit")) %>%
  mutate(Division = str_replace(Division, "Task Force.*", "Task Force")) %>%
  mutate(Division = str_replace(Division, "Traffic Section", "Traffic")) %>%
  mutate(Division = str_replace(Division, "Tact 1", "Tactical Section")) %>%
  mutate(Division = str_replace(Division, "Narcotics Section", "Narcotics")) %>%
  mutate(Division = str_replace(Division, "Intelligence Section", "Intelligence")) %>%
  mutate(Division = str_replace(Division, "Homicide Section", "Homicide")) %>%
  mutate(Division = str_replace(Division, "Gen Assignment", "General Assignments")) %>%
  mutate(Division = str_replace(Division, "Day Beats", "Day Watch")) %>%
  mutate(Division = str_replace(Division, "SRO", "School Resource Officers")) %>%
  mutate(Division = str_replace(Division, "Evening Watch", "Night Watch")) 
  
nola_use_of_force$Division <- ifelse(is.na(nola_use_of_force$Division), "Other Support" , nola_use_of_force$Division)
 
# Find the counts of each unique string occurrence in the Division column
division_counts <- table(nola_use_of_force$Division)

# Get the list of Division values that appear fewer than 50 times
divisions_below_threshold <- names(division_counts[division_counts < 8])
table(divisions_below_threshold)
# Replace occurrences below the threshold with "Other Support" just to shorten this feature
nola_use_of_force$Division <- ifelse(nola_use_of_force$Division %in% divisions_below_threshold, "Other Support", nola_use_of_force$Division)


freq_table <- table(nola_use_of_force$Division)
df <- data.frame(String = names(freq_table), Frequency = freq_table)
df <- df[order(df$Frequency.Freq), ]
df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])
df

ggplot(df, aes(x = String, y = Frequency.Freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Dividion", y = "Frequency", title = "Incident table occurance by division") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Doing the same for the Police table
unique(police_table_nola_use_of_force$Division)
sum(is.na(police_table_nola_use_of_force$Division))

police_table_nola_use_of_force <- police_table_nola_use_of_force %>% 
  mutate(Division = str_replace(Division,  "B Platoon", "BPlatoon")) %>%
  mutate(Division = str_replace(Division,  "\\bB\\b", "B Platoon")) %>%
  mutate(Division = str_replace(Division,  "\\bb\\b" , "B Platoon")) %>%
  mutate(Division = str_replace(Division,  "BPlatoon", "B Platoon")) %>%
  mutate(Division = str_replace(Division, "\\bAdmin\\b", "Administration")) %>%
  mutate(Division = str_replace(Division, "DIU Persons", "D.I.U.")) %>%
  mutate(Division = str_replace(Division, "DIU SPEC", "D.I.U.")) %>%
  mutate(Division = str_replace(Division, "DIU Property", "D.I.U.")) %>%
  mutate(Division = str_replace(Division, "DIU Staff", "D.I.U.")) %>%
  mutate(Division = str_replace(Division, "D.I.U", "District Investigations Unit")) %>%
  mutate(Division = str_replace(Division, "Task Force.*", "Task Force")) %>%
  mutate(Division = str_replace(Division, "Traffic Section", "Traffic")) %>%
  mutate(Division = str_replace(Division, "Tact 1", "Tactical Section")) %>%
  mutate(Division = str_replace(Division, "Narcotics Section", "Narcotics")) %>%
  mutate(Division = str_replace(Division, "Intelligence Section", "Intelligence")) %>%
  mutate(Division = str_replace(Division, "Homicide Section", "Homicide")) %>%
  mutate(Division = str_replace(Division, "Gen Assignment", "General Assignments")) %>%
  mutate(Division = str_replace(Division, "Day Beats", "Day Watch")) %>%
  mutate(Division = str_replace(Division, "SRO", "School Resource Officers")) %>%
  mutate(Division = str_replace(Division, "Evening Watch", "Night Watch")) 

# Adding N/A (168) to other support
police_table_nola_use_of_force$Division <- ifelse(is.na(police_table_nola_use_of_force$Division), "Other Support" , police_table_nola_use_of_force$Division)

freq_table <- table(police_table_nola_use_of_force$Division)
df <- data.frame(String = names(freq_table), Frequency = freq_table)
df <- df[order(df$Frequency.Freq), ]
df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])

# Not going to shorten it for this table
ggplot(df, aes(x = String, y = Frequency.Freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Division", y = "Frequency", title = "Police table occurance by division") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Unit
# A lot of missing probably have to be fine with the division level not adding it to my models
unique(nola_use_of_force$Unit)
sum(is.na(nola_use_of_force$Unit))


# Influencing factors
unique(nola_use_of_force$Subject_Influencing_Factors)
nola_use_of_force <- nola_use_of_force %>% 
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "Alchohol.*", "Alcohol")) %>%
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "Mentally unstable.*", "Mentally unstable")) %>%
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "Drugs.*", "Drugs"))  %>%
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "Unknown.*", "Unknown")) %>%
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "None detected.*", "None detected")) %>%
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "^\\|\\s*", ""))
nola_use_of_force$Subject_Influencing_Factors <- ifelse(is.na(nola_use_of_force$Subject_Influencing_Factors), "Unknown" , nola_use_of_force$Subject_Influencing_Factors)

pie(table(nola_use_of_force$Subject_Influencing_Factors), main = "Subject Influencing Factors \n by Incident ")


unique(subject_table_use_of_force$Subject_Influencing_Factors)
subject_table_use_of_force <- subject_table_use_of_force %>% 
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "Alchohol.*", "Alcohol")) %>%
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "Mentally unstable.*", "Mentally unstable")) %>%
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "Drugs.*", "Drugs"))  %>%
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "Unknown.*", "Unknown")) %>%
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "None detected.*", "None detected")) %>%
  mutate(Subject_Influencing_Factors = str_replace(Subject_Influencing_Factors, "^\\|\\s*", ""))
subject_table_use_of_force$Subject_Influencing_Factors <- ifelse(is.na(subject_table_use_of_force$Subject_Influencing_Factors), "Unknown" , subject_table_use_of_force$Subject_Influencing_Factors)

pie(table(subject_table_use_of_force$Subject_Influencing_Factors), main = "Subject Influencing \n Aggregated")

# Use of force type
# Replacing field with highest force use. If you start smashing these field more together, i.e. putting all firearms together,
# or all canine fields together our models loose a lot of their predicting power, it seems to like this specific aggregation 
  unique(nola_use_of_force$Use_of_Force_Type)
  
  unique(police_table_nola_use_of_force$Use_of_Force_Type)
  
  nola_use_of_force <- nola_use_of_force %>% 
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Firearm \\(Discharged\\).*", "Firearm (Discharged)")) %>% 
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*VehPursuits w/Injury.*", "VehPursuits w/Injury")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Takedown \\(w/injury\\).*", "Takedown (w/injury)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Canine \\(Contact\\).*", "Canine (Contact)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Canine \\(Bite\\).*", "Canine (Contact)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*CEW Deployment.*", "CEW Deployment")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Baton/PR-24 \\(Strike\\).*", "Baton/PR-24 (Strike)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Hands \\(with injury\\).*", "Touch (with injury)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Head Strike \\(No Wep\\).*", "Head Strike (No Wep)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Force \\(Take Down\\).*", "Force (Take Down)")) %>% 
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Takedown \\(no injury\\).*", "Force (Take Down)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Force \\(Defense Tech\\).*", "Force (Defense Tech)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Takedown \\(no injury\\).*", "Force (Take Down)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Force \\(Neck Holds\\).*", "Force (Neck Holds)")) %>%   
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Baton \\(non-strike\\).*", "Baton/PR-24(NonStrk)")) %>%  
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Baton/PR-24\\(NonStrk\\).*", "Baton/PR-24(NonStrk)")) %>%  
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Hands.*", "Hands")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Force \\(Escort Tech\\).*", "Force (Escort Tech)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Firearm \\(Exhibited\\).*", "Firearm (Exhibited)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Rifle \\(Pointed\\).*", "Firearm (Exhibited)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Shotgun \\(Pointed\\).*", "Firearm (Exhibited)")) %>% 
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*CEW Exhibited/Laser.*", "CEW Exhibited/Laser")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Vehicle as Weapon.*", "VehPursuits w/Injury")) %>%  
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, "(?<!\\S)CEW(?!\\S|\\sExhibited\\/Laser)", "CEW Deployment")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, "CEW Deployment Deployment", "CEW Deployment")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Touch \\(with injury\\).*", "Hands (with injury)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Other.*", "Other")) 
   
  
 
  police_table_nola_use_of_force <- police_table_nola_use_of_force %>% 
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Firearm \\(Discharged\\).*", "Firearm (Discharged)")) %>% 
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*VehPursuits w/Injury.*", "VehPursuits w/Injury")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Takedown \\(w/injury\\).*", "Takedown (w/injury)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Canine \\(Contact\\).*", "Canine (Contact)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Canine \\(Bite\\).*", "Canine (Contact)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*CEW Deployment.*", "CEW Deployment")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Baton/PR-24 \\(Strike\\).*", "Baton/PR-24 (Strike)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Hands \\(with injury\\).*", "Touch (with injury)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Head Strike \\(No Wep\\).*", "Head Strike (No Wep)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Force \\(Take Down\\).*", "Force (Take Down)")) %>% 
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Takedown \\(no injury\\).*", "Force (Take Down)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Force \\(Defense Tech\\).*", "Force (Defense Tech)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Takedown \\(no injury\\).*", "Force (Take Down)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Force \\(Neck Holds\\).*", "Force (Neck Holds)")) %>%   
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Baton \\(non-strike\\).*", "Baton/PR-24(NonStrk)")) %>%  
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Baton/PR-24\\(NonStrk\\).*", "Baton/PR-24(NonStrk)")) %>%  
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Hands.*", "Hands")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Force \\(Escort Tech\\).*", "Force (Escort Tech)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Firearm \\(Exhibited\\).*", "Firearm (Exhibited)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Rifle \\(Pointed\\).*", "Firearm (Exhibited)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Shotgun \\(Pointed\\).*", "Firearm (Exhibited)")) %>% 
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*CEW Exhibited/Laser.*", "CEW Exhibited/Laser")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Vehicle as Weapon.*", "VehPursuits w/Injury")) %>%  
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, "(?<!\\S)CEW(?!\\S|\\sExhibited\\/Laser)", "CEW Deployment")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, "CEW Deployment Deployment", "CEW Deployment")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Touch \\(with injury\\).*", "Hands (with injury)")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Rifle \\(Discharged\\).*", "Firearm (Discharged)")) %>% 
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, "NonTrad Impact Weapon", "Other")) %>%
    mutate(Use_of_Force_Type = str_replace(Use_of_Force_Type, ".*Other.*", "Other")) 
  

 
  
  freq_table <- table(nola_use_of_force$Use_of_Force_Type)
  df <- data.frame(String = names(freq_table), Frequency = freq_table)
  df <- df[order(df$Frequency.Freq), ]
  df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])
  df
  
  ggplot(df, aes(x = String, y = Frequency.Freq)) +
    geom_bar(stat = "identity") +
    labs(x = "UOF Type", y = "Frequency", title = "UOF Type Frequency by Incident") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # we are keeping the police table unadulterated 
  freq_table <- table(police_table_nola_use_of_force$Use_of_Force_Type)
  df <- data.frame(String = names(freq_table), Frequency = freq_table)
  df <- df[order(df$Frequency.Freq), ]
  df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])
  df
  
  ggplot(df, aes(x = String, y = Frequency.Freq)) +
    geom_bar(stat = "identity") +
    labs(x = "UOF Type", y = "Frequency", title = "UOF Type by Officer Involved") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
# Use_of_Force_Reason incident only
unique(nola_use_of_force$Use_of_Force_Reason)
sum(is.na(nola_use_of_force$Use_of_Force_Reason))
# N/A go to other (4)
nola_use_of_force$Use_of_Force_Reason <- ifelse(is.na(nola_use_of_force$Use_of_Force_Reason), "Other" , nola_use_of_force$Use_of_Force_Reason)

nola_use_of_force <- nola_use_of_force %>% 
  mutate(Use_of_Force_Reason = str_replace(Use_of_Force_Reason, "room clearing", "Room Clearing")) %>% 
  mutate(Use_of_Force_Reason = str_replace(Use_of_Force_Reason, "Room CLearing" , "Room Clearing"))  %>% 
  mutate(Use_of_Force_Reason = str_replace(Use_of_Force_Reason, "Room clearing" , "Room Clearing"))  %>% 
  mutate(Use_of_Force_Reason = str_replace(Use_of_Force_Reason, ".*Resisting Officer w/Weapon.*", "Resisting Officer w/Weapon")) %>%
  mutate(Use_of_Force_Reason = str_replace(Use_of_Force_Reason, "Building clearing", "Room Clearing"))   %>% 
  mutate(Use_of_Force_Reason = str_replace(Use_of_Force_Reason, "Other-Create Distance", "Other")) %>%
  mutate(Use_of_Force_Reason = str_replace(Use_of_Force_Reason, "Other-Resisting", "Resisting Lawful Arrest")) %>%
  mutate(Use_of_Force_Reason = str_replace(Use_of_Force_Reason, "Felony Stop", "Felony Vehicle stop")) 
  
  
freq_table <- table(nola_use_of_force$Use_of_Force_Reason)
df <- data.frame(String = names(freq_table), Frequency = freq_table)
df <- df[order(df$Frequency.Freq), ]
df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])
df

ggplot(df, aes(x = String, y = Frequency.Freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Reason", y = "Frequency", title = "Use of force reason") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Service_Type incident only
unique(nola_use_of_force$Service_Type)
sum(is.na(nola_use_of_force$Service_Type))
nola_use_of_force$Service_Type <- ifelse(is.na(nola_use_of_force$Service_Type), "Other" , nola_use_of_force$Service_Type)
nola_use_of_force <- nola_use_of_force %>% 
  mutate(Service_Type = str_replace(Service_Type, ".*Call for Service.*", "Call for Service")) %>% 
mutate(Service_Type = str_replace(Service_Type, "Not Used", "Other"))


freq_table <- table(nola_use_of_force$Service_Type)
df <- data.frame(String = names(freq_table), Frequency = freq_table)
df <- df[order(df$Frequency.Freq), ]
df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])
df

# Almost 50% of incidents were calls for service 
ggplot(df, aes(x = String, y = Frequency.Freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Type", y = "Frequency", title = "Service Call Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Light conditions The few (26) N/A's are going to good
# most cases have clear conditions with good lighting.
# Do most injuries  happen in bad lighting
unique(nola_use_of_force$Light_Condition)
sum(is.na(nola_use_of_force$Light_Condition))
nola_use_of_force <- nola_use_of_force %>%
  mutate(Light_Condition = ifelse(is.na(Light_Condition) | Light_Condition == "| Good", "Good", Light_Condition)) %>%
  mutate(Light_Condition = ifelse(Light_Condition == "Good", 1, 0))
table(nola_use_of_force$Light_Condition)
hist(nola_use_of_force$Light_Condition, col = "orange", main = "Incident Lighting Conditions", xlab = "1 = Good Gonditions")

# Weather_condition N/A's going to Other
unique(nola_use_of_force$Weather_Condition)
sum(is.na(nola_use_of_force$Weather_Condition))
nola_use_of_force <- nola_use_of_force %>%
  mutate(Weather_Condition = ifelse(is.na(Weather_Condition), "Other", gsub("\\| ", "", Weather_Condition)))

table(nola_use_of_force$Weather_Condition)
freq_table <- table(nola_use_of_force$Weather_Condition)
df <- data.frame(String = names(freq_table), Frequency = freq_table)
df <- df[order(df$Frequency.Freq), ]
df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])

ggplot(df, aes(x = String, y = Frequency.Freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Type", y = "Frequency", title = "Weather Conditions") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Disposition 
# Moving (3) N/A's to other

unique(nola_use_of_force$Disposition)
sum(is.na(nola_use_of_force$Disposition))
nola_use_of_force <- nola_use_of_force %>%
  mutate(Disposition = ifelse(is.na(Disposition), "Other", Disposition))

table(nola_use_of_force$Disposition)

freq_table <- table(nola_use_of_force$Disposition)
df <- data.frame(String = names(freq_table), Frequency = freq_table)
df <- df[order(df$Frequency.Freq), ]
df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])


unique(police_table_nola_use_of_force$Disposition)
sum(is.na(police_table_nola_use_of_force$Disposition))
police_table_nola_use_of_force <- police_table_nola_use_of_force %>%
  mutate(Disposition = ifelse(is.na(Disposition), "Other", Disposition))

table(nola_use_of_force$Disposition)

freq_table <- table(police_table_nola_use_of_force$Disposition)
df <- data.frame(String = names(freq_table), Frequency = freq_table)
df <- df[order(df$Frequency.Freq), ]
df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])
# Most use of force is justified or authorized according to police,
# we need to create our own grading feature
ggplot(df, aes(x = String, y = Frequency.Freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Type", y = "Frequency", title = "Internal Police Disposition of Aggrigated") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#################################################################################################
# End of cleaning start we are now separating out data to various tables for machine learning and 
# creating a new feature to grade these incidences based of weighting the data now cleaned 

# Final incident table before grading weight
incident_nola_use_of_force = select(nola_use_of_force, c(PIB_File_Number, Date_Occurred, 
              Originating_Bureau, Division, Disposition, Service_Type, Light_Condition, 
              Weather_Condition, Use_of_Force_Type,  Use_of_Force_Level, Officer_Number, Officer_Race, 
              Officer_Gender, Officer_Age, Officer_Years_of_Service, 
              Distance_Between,Subject_Number, Subject_Gender, Subject_Ethnicity, Subject_Age,
              Subject_Build, Subject_Height, Subject_Influencing_Factors))

# Final Subject table, not adding a grade to this table
subject_table_use_of_force = select(subject_table_use_of_force, -c(Unit, Officer_Race, Officer_Gender,   
  Working_Status, Shift, Investigation_status, Originating_Bureau, Division_level, Division, Disposition, Service_Type,
 Subject_Arrest_Charges, Light_Condition, Weather_Condition, Use_of_Force_Type, Use_of_Force_Level, Use_of_Force_Effective, 
 Officer_Age, Officer_Years_of_Service, Distance_Between, Officer_Injured))

# Final police table before grading weight, we try some liner regression on this 
# but results mirror the incident table so stop after preceding to machine learning with this table

police_table_nola_use_of_force = select(police_table_nola_use_of_force, -c(Originating_Bureau, Division_level, Division,
              Unit,  Working_Status, Shift, Investigation_status, Service_Type, Light_Condition, Weather_Condition, 
              Subject_Influencing_Factors, Subject_Gender,Subject_Ethnicity, Distance_Between,
              Subject_Age, Subject_Build, Subject_Height, Subject_Injured, Subject_Hospitalized,
              Subject_Arrested, Subject_Arrest_Charges))
  
#   Here we a creating our own feature to grade the severity of each incident. We are using weights on various existing features to then create our own grading feature. Bellow we initially start with weights for in house evaluation or feature called  disposition. We weigh down uof justified a bit because it wasn't authorized,
# UOF Justified With Training seems more of a severe along with UOF Justified With Policy Violation and
# Use Of Force Not Authorized / UOF Not Justified With Policy Violations recieves the whole penalization of the weight.
# all cases are getting at least this .1 from this weight for actually being a case at all. I.E the case wasn't 
# Deescalated before needing use of force.
  weights <- c(
    "Use Of Force Authorized" =.1,
    "UOF Justified" = .3,
    "UOF Justified With Training" = 0.7,
    "Pending" = 0.1,                                         
    "UOF Not Justified" = 1,                              
    "UOF No Reportable Force Used by Officer" = 0.1,       
    "Other"  = 0.1,                                         
    "UOF Justified With Policy Violation" = .5 ,            
    "Cancel FIT FTN" = 0,                               
     "Use Of Force Not Authorized"  = 1,                    
     "UOF Complaint of Injury/No Reportable Force Used" = .1,
     "UOF Justified | Duplicate Investigation"  = .1,        
    "UOF Force Used Against Officer"  = .1,                 
    "UOF Not Justified With Policy Violations" = 1
  )
  
  # The severity of force types should be considered in the weighting grade, with 
  # greater weight assigned to those posing a higher risk of injury. Initially, 
  # I assigned a lower weight to 'Firearm (Exhibited),' but this choice significantly 
  # altered the mean of the weighting grade. Given the frequent use and potential 
  # lethality of this force type, it became necessary to assign it a heavier weight.
  # Additionally, considering the lower potential harm associated with alternative 
  # police methods, it is unreasonable for firearms to be the default, overused option.
  # 
  type_weight <- c(
    "Firearm (Discharged)" = 1,
    "VehPursuits w/Injury" = 1,
    "Takedown (w/injury)"= .4,
    "Canine (Contact)" = .7,
    "CEW Deployment" =  .3,
    "Baton/PR-24 (Strike)" =  .3,
    "Head Strike (No Wep)" = .3,
    "Force (Take Down)" = .3, 
    "Force (Defense Tech)" = .3,
    "Force (Neck Holds)" = .8,
    "Baton/PR-24(NonStrk)" = .3,
    "Hands" = .2, 
    "Force (Escort Tech)" = .1,
    "Firearm (Exhibited)" = .9,
    "CEW Exhibited/Laser" = .2,
    "Hands (with injury)" = 3,
    "Other" = 0,
    "Canine (No Bite)" = .1,
    "Handcuffed Subject" = .1,
    "Baton/PR-24 (Miss)" = .1
  )
  
  # This calculation was performed because the actual racial composition of the police 
  # force did not align closely with the racial distribution in the incidents. To 
  # account for this disparity in our new grading feature, the data in the Police 
  # Race feature was normalized with a slight weighting based on race. It's important 
  # to note that this adjustment has a minimal impact on the overall analysis, as the 
  # later sections will demonstrate that police race and gender features have 
  # limited predictive power in the model.
  
  #  race data 
  copracedata<- data.frame(
    Race = c("Black", "White"),
    Provided_Percentage = c(57, 37),
    Calculated_Percentage = c(47.22563, 52.77437)
  )
  # Calculate normalized race weights
coprace_diff <- (copracedata$Provided_Percentage - copracedata$Calculated_Percentage)
coprace_diff

  
  
  race_weight <- c(
    "White"= .16,
      "Majority_White" = .16,  
      "White&Black" = 0,
      "Hispanic" =  0, 
      "Black&Hispanic" = 0, 
      "Black&Asian_Pacific_Islander" = 0, 
      "White&Race_Unknown" = 0,
      "Majority_Asian_Pacific_Islander" = 0, 
      "Mixed" = 0,
      "White&Hispanic" = 0,
      "Hispanic&Asian_Pacific_Islander" = 0,
      "Majority_Hispanic" = 0,
      "Black&Race_Unknown" = 0,
      "Black&American_Indian" = 0, 
      "Black" = -.1,
      "Majority_Black" = -.1,
      "Asian_Pacific_Islander" = 0,
      "American_Indian" = 0, 
      "Race_Unknown" = 0,
      "White&Asian_Pacific_Islander" = 0,
      "White&American_Indian" = 0,
      "Asian/Pacific Islander" = 0,
      "Not Specified" = 0,
      "American Indian/Alaska Native" = 0,
      "Not Applicable (Non-U.S.)" = 0
  )

  # If you look below in the analysis of officer gender 
  #there is a 12% normalization between the genders too 
gender_weight <- c(
  "Male" = .12,
    "Male&Female" = 0,
    "Majority_Male" = .12,
    "Female" = -.12,
    "Majority_Female" = -.12
)

# This code is nice to check all the weight to the features 
unique_dispositions <- unique(police_table_nola_use_of_force$Disposition)
for (disp in unique_dispositions) {
  print(paste("Disposition:", disp, "Weight:", weights[disp]))
}
 
unique_type <- unique(police_table_nola_use_of_force$Use_of_Force_Type)
for (disp in unique_type) {
  print(paste("Disposition:", disp, "Weight:", type_weight[disp]))
}


unique_dispositions <- unique(police_table_nola_use_of_force$Officer_Race)
for (disp in unique_dispositions) {
  print(paste("Disposition:", disp, "Weight:", race_weight[disp]))
}    

unique_dispositions <- unique(police_table_nola_use_of_force$Officer_Gender)
for (disp in unique_dispositions) {
  print(paste("Disposition:", disp, "Weight:", gender_weight[disp]))
}    

unique_dispositions <- unique(incident_nola_use_of_force$Disposition)
for (disp in unique_dispositions) {
  print(paste("Disposition:", disp, "Weight:", weights[disp]))
}

unique_dispositions <- unique(incident_nola_use_of_force$Officer_Race)
for (disp in unique_dispositions) {
  print(paste("Disposition:", disp, "Weight:", race_weight[disp]))
}    

unique_dispositions <- unique(incident_nola_use_of_force$Officer_Gender)
for (disp in unique_dispositions) {
  print(paste("Disposition:", disp, "Weight:", gender_weight[disp]))
}    

# This is the one place where I use the PIB # across tables to get the total
# officers and subjects injured. I really could have made a feature in the incidents
# and think I in fact did but I imagine this is how you could link data sets for machine leaning
# or liner regression. 

# Aggregate police_table_nola_use_of_force by PIB_File_Number
aggregated_police <- police_table_nola_use_of_force %>%
  group_by(PIB_File_Number) %>%
  summarise(
    Officer_Injured = sum(as.numeric(Officer_Injured))
  )

# Aggregate subject_table_use_of_force by PIB_File_Number
aggregated_subject <- subject_table_use_of_force %>%
  group_by(PIB_File_Number) %>%
  summarise(
    Subject_Injured = sum(as.numeric(Subject_Injured)),
    Subject_Hospitalized = sum(as.numeric(Subject_Hospitalized))
  )

# These are the weights I finally settled on, you can do the other weights bellow but have to 
# duplicate for the police table. Mostly it is weighing disposition, which is the 
# closest thing to an in house grade. I'm trying to honor their system a bit with .28 on that.
# The use of force type is punishing th firearm usage which is the critical response to the data.
# The weights on subject injured and hospitalized were sever because that is what really determines
# a failing incident, whether or not the subject was injured. The officer injured weight is small 
# because the number of officers injured is small. And the use of force level  isn't weighted too much 
# because it pushes the data out of normal and actually over fits the models a bit.

incident_nola_use_of_force <- incident_nola_use_of_force %>%
  left_join(aggregated_police, by = "PIB_File_Number") %>%
  left_join(aggregated_subject, by = "PIB_File_Number") %>%
  mutate(
    weighted_sum = 0.28 * (weights[Disposition]) +
      0.23 * (type_weight[Use_of_Force_Type]) +
      0.12 * (race_weight[Officer_Race]) +
      0.05 * (gender_weight[Officer_Gender]) +
      0.01 * (.25 * Use_of_Force_Level) +
      0.01 * (Officer_Injured ) +
      0.1 * (Subject_Injured ) +
      0.2 * (Subject_Hospitalized)
  )

police_table_nola_use_of_force <- police_table_nola_use_of_force %>%
  left_join(aggregated_subject, by = "PIB_File_Number") %>%
  mutate(
    weighted_sum = 0.28 * (weights[Disposition]) +
      0.23 * (type_weight[Use_of_Force_Type]) +
      0.12 * (race_weight[Officer_Race]) +
      0.05 * (gender_weight[Officer_Gender]) +
      0.01 * (.25 * Use_of_Force_Level) +
      0.01 * (Officer_Injured ) +
      0.1 * (Subject_Injured ) +
      0.2 * (Subject_Hospitalized)
  )

  
  
  
  
# 1: R-squared:  0.35406  on the dl model Most balanced data set visually plotted
  # Also is a combo of heavy weights on in house reporting an penalizing use of force type
  # So seemingly less biased, also just a hint of use of force level because it was so 
  # powerful a variable in prediction
  
  #                Model        MSE      RMSE        MAE R_squared
          #    modelcombo 0.01767880 0.1329617 0.09948939   0.41835
          #      modeltop 0.01752359 0.1323767 0.09740995   0.42345
          # Random Forest 0.01765492 0.1328718 0.09812313   0.41913
          #       Xboost 0.01936695 0.1391652 0.10173171   0.36280
          #    Tensorflow 0.01967656 0.1402732 0.10196456   0.35406
   
  # incident_nola_use_of_force <- incident_nola_use_of_force %>%
  #   left_join(aggregated_police, by = "PIB_File_Number") %>%
  #   left_join(aggregated_subject, by = "PIB_File_Number") %>%
  #   mutate(
  #     weighted_sum = 0.38 * (weights[Disposition]) +
  #       0.28 * (type_weight[Use_of_Force_Type]) +
  #      0.01 * (.25 * Use_of_Force_Level) +
  #       0.01 * (Officer_Injured ) +
  #       0.12 * (Subject_Injured ) +
  #       0.2 * (Subject_Hospitalized)
  #   )
  # 
  # police_table_nola_use_of_force <- police_table_nola_use_of_force %>%
  # left_join(aggregated_subject, by = "PIB_File_Number") %>%
  #   mutate(
  #     weighted_sum = 0.38 * (weights[Disposition]) +
  #       0.28 * (type_weight[Use_of_Force_Type]) +
  #       0.01 * (.25 * Use_of_Force_Level) +
  #       0.01 * (Officer_Injured ) +
  #       0.12 * (Subject_Injured ) +
  #       0.2 * (Subject_Hospitalized)
  #   )
  # 
  # 2nd grade wight heavy on the use of force top level 
  # R-squared:  0.50480 for dl used the top use of force level as a heavy weight 
  #  maybe we are over fitting this time to force level which, xboost has shown to be
  # a critical indicator for model success
 
   #                Model        MSE      RMSE        MAE R_squared 
            #  modelcombo 0.01663089 0.1289608 0.09332143   0.51830
            #  modeltop 0.01692154 0.1300828 0.09374248   0.50989
            #  Random Forest 0.01734867 0.1317143 0.09535165   0.49752
            # Xboost 0.01821967 0.1349803 0.09808841   0.47229
            #  Tensorflow 0.01503947 0.1226355 0.09345978   0.50480
            
  # incident_nola_use_of_force <- incident_nola_use_of_force %>%
  #   left_join(aggregated_police, by = "PIB_File_Number") %>%
  #   left_join(aggregated_subject, by = "PIB_File_Number") %>%
  #   mutate(
  #     weighted_sum = 0.15 * (weights[Disposition]) +
  #                0.2 * (type_weight[Use_of_Force_Type]) +
  #                0.3 * (.25 * Use_of_Force_Level) +
  #               0.05 * (Officer_Injured ) +
  #                0.1 * (Subject_Injured ) +
  #                0.2 * (Subject_Hospitalized)
  #           )
  
  # 3rd Gras weight  R-squared: 0.3744143   on dl model: central over weighted best performing over all
  # Liner regression in .407, Tensorflow can get up there id we really penalize 
  # firearm exhibited to .8, .9. but Im worried of over fitting, penalizing use of force type? 
  
  # incident_nola_use_of_force <- incident_nola_use_of_force %>%
  #   left_join(aggregated_police, by = "PIB_File_Number") %>%
  #   left_join(aggregated_subject, by = "PIB_File_Number") %>%
  #   mutate(
  #     weighted_sum = 0.35 * (weights[Disposition]) +
  #       0.3 * (type_weight[Use_of_Force_Type]) +
  #       0.05 * (Officer_Injured ) +
  #       0.1 * (Subject_Injured ) +
  #       0.2 * (Subject_Hospitalized)
  #   )

  # 4th: Og setup more weight on injuries less on disposition/ adding weight to 
  # potential injury use of force type dl model R-squared: 0.3344992 
  # I like this model but the data just doesn't look very balanced 
  
  # incident_nola_use_of_force <- incident_nola_use_of_force %>%
  #   left_join(aggregated_police, by = "PIB_File_Number") %>%
  #   left_join(aggregated_subject, by = "PIB_File_Number") %>%
  #   mutate(
  #     weighted_sum = 0.25 * (weights[Disposition]) +
  #       0.25 * (type_weight[Use_of_Force_Type]) +
  #              0.05 * (Officer_Injured ) +
  #       0.2 * (Subject_Injured ) +
  #       0.3 * (Subject_Hospitalized )
  #   )

  # 5th: OG setup no weight on use of force type dl model R-squared: 0.3117431 
  # this is what you would have the data blindly tell, that most use of force were 
  # justified except those that got out of hand where people are injured 
  
  # incident_nola_use_of_force <- incident_nola_use_of_force %>%
  #   left_join(aggregated_police, by = "PIB_File_Number") %>%
  #   left_join(aggregated_subject, by = "PIB_File_Number") %>%
  #   mutate(
  #     weighted_sum = 0.3 * (weights[Disposition]) +
  #              0.1 * (Officer_Injured ) +
  #       0.2 * (Subject_Injured ) +
  #       0.4 * (Subject_Hospitalized )
  #   )
  
  # Remove names from the weighted_sum column if needed
  incident_nola_use_of_force$weighted_sum <- unname(incident_nola_use_of_force$weighted_sum)
  incident_nola_use_of_force$weighted_sum <- round(incident_nola_use_of_force$weighted_sum, 3)
  police_table_nola_use_of_force$weighted_sum <-unname(police_table_nola_use_of_force$weighted_sum)
  police_table_nola_use_of_force$weighted_sum <- round(police_table_nola_use_of_force$weighted_sum, 3)
  # Replace values above 1 with 1 in the weighted_sum column
  # Basicly if more that one person is injured on the incident they stake up the weight, if they go over 1 they top out at 1
  incident_nola_use_of_force$weighted_sum <- ifelse(incident_nola_use_of_force$weighted_sum > 1, 1, incident_nola_use_of_force$weighted_sum)
  police_table_nola_use_of_force$weighted_sum <- ifelse(police_table_nola_use_of_force$weighted_sum > 1, 1, police_table_nola_use_of_force$weighted_sum)
  
  
   hist(incident_nola_use_of_force$weighted_sum)
   hist(police_table_nola_use_of_force$weighted_sum)
  # Continue with the Grade assignment from the weighted sum
  incident_nola_use_of_force <- incident_nola_use_of_force %>%
    mutate(
      Grade = case_when(
        weighted_sum >= 0.5 ~ "F",
        weighted_sum >= 0.4 ~ "D",
        weighted_sum >= 0.3 ~ "C",
        weighted_sum >= 0.2 ~ "B",
        weighted_sum >= 0 ~ "A",
        TRUE ~ "A"
      )
    )
    
  # Continue with the Grade assignment
  police_table_nola_use_of_force <- police_table_nola_use_of_force %>%
    mutate(
      Grade = case_when(
        weighted_sum >= 0.5 ~ "F",
        weighted_sum >= 0.4 ~ "D",
        weighted_sum >= 0.3 ~ "C",
        weighted_sum >= 0.2 ~ "B",
        weighted_sum >= 0 ~ "A",
        TRUE ~ "A"
      )
    )

  table(incident_nola_use_of_force$Grade)
 
 freq_table <- table(incident_nola_use_of_force$Grade)
 
 df <- data.frame(String = names(freq_table), Frequency = freq_table)
 df <- df[order(df$Frequency.Freq), ]
 df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])

 ggplot(df, aes(x = String, y = Frequency.Freq)) +
   geom_bar(stat = "identity") +
   labs(x = "Grade", y = "Frequency", title = "Grade") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 # making sure it looks neat
 unique(incident_nola_use_of_force$Officer_Race)
 unique(incident_nola_use_of_force$Officer_Gender)
 unique(incident_nola_use_of_force$Subject_Ethnicity)
 unique(incident_nola_use_of_force$Subject_Gender)
 unique(incident_nola_use_of_force$Use_of_Force_Type)
 unique(incident_nola_use_of_force$Subject_Influencing_Factors)
 unique(incident_nola_use_of_force$Subject_Build)
 unique(incident_nola_use_of_force$Division)
 unique(incident_nola_use_of_force$Light_Condition)
 unique(incident_nola_use_of_force$Weather_Condition)
 

 # Group the data by Officer_Race and calculate the mean of weighted_sum
 # the incident data may be a bit messy, once you aggregate it most police by race 
 # are close to mean. White offices have a tendency to be a little more higher mean.
 # American Indian is such a small number of officers so high mean is not reliable 
 
 
 
 mean(police_table_nola_use_of_force$weighted_sum)
 aggregate_mean_by_race <- police_table_nola_use_of_force %>%
   group_by(Officer_Race) %>%
   summarise(Mean_Weighted_Sum = mean(weighted_sum, na.rm = TRUE))
 
 aggregate_mean_by_race
 
 selected_groups <- c("Black",  "White")
 subset_data <- police_table_nola_use_of_force %>%
   group_by(Officer_Race) %>%
   filter(Officer_Race %in% selected_groups)
 
 
 # Creating a density plot showing the slight difference between the Black and White cops 
 ggplot(subset_data, aes(x = weighted_sum, fill = Officer_Race)) +
   geom_density(alpha = 0.5) +
   labs(x = "Mean Weighted Sum", y = "Density", title = "Distribution of Mean Weighted Sum by Officer Race") +
   scale_fill_manual(values = c(
     "Black" = "green",
     "White" = "orange"
   )) +
   theme_minimal()
 
 incident_mean_by_race <-incident_nola_use_of_force %>%
   group_by(Officer_Race) %>%
   summarise(Mean_Weighted_Sum = mean(weighted_sum, na.rm = TRUE))
 
 print(n= 22, incident_mean_by_race)
 # the mean weighted sum by race comparison in interesting  don't trust the sample size for most of these but
 # Black and Wite offices are alone their weighted sum mean is lower. Maybe larger groups of officers skews the mean?
 
 ggplot(incident_mean_by_race, aes(x = Officer_Race, y = Mean_Weighted_Sum)) +
   geom_bar(stat = "identity", fill = "skyblue") +
   labs(x = "Officer Race", y = "Mean Weighted Sum", title = "Mean Weighted Sum by Officer Race")+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 # write.csv(nola_use_of_force, "nola_use-Of_force_Quicksight_data.csv", row.names = FALSE)
 
 
 # Frequency of subject ethnicity overwhelmingly black subjects
 # I was going to maybe do a predictive model over the race of the subject but 
 # it's over overwhelmingly black subjects
 
 freq_table <- table(subject_table_use_of_force$Subject_Ethnicity)
 df <- data.frame(String = names(freq_table), Frequency = freq_table)
 df <- df[order(df$Frequency.Freq), ]
 df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])
 print(freq_table)
 
 ggplot(df, aes(x = String, y = Frequency.Freq)) +
   geom_bar(stat = "identity") +
   labs(x = "String", y = "Frequency", title = "Occurrence Rate of Subject by Race") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 
 # Histogram of occurrences by officer race
 # Today's NOPD is 57 percent black and 37 percent white : 
 # https://www.nola.com/nopd-diversity-decreasing-as-more-recruits-hired-numbers-show-police-chief-unconcerned/article_2baea663-a1d5-51d3-a4f4-7956f93be1af.html#:~:text=The%20new%20crop%20of%20New,percent%20white%2C%20police%20figures%20show.
 
 freq_table <- table(incident_nola_use_of_force$Officer_Race)
 df <- data.frame(String = names(freq_table), Frequency = freq_table)
 df <- df[order(df$Frequency.Freq), ]
 df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])
 print(freq_table)
 
 ggplot(df, aes(x = String, y = Frequency.Freq)) +
   geom_bar(stat = "identity") +
   labs(x = "Race Combo", y = "Frequency", title = "Occurrence Rate of Officer by Race Combonation") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 
 # Filter the dataframe to include only White and Black race categories
 filtered_df <- df[df$String %in% c("White", "Black"), ]
 
 # Calculate the percentage of each race in the incidents dataset
 filtered_df$Percentage <- filtered_df$Frequency.Freq / sum(filtered_df$Frequency.Freq) * 100
 
 # Provided percentages for NOPD for White and Black
 provided_percentages <- c(57, 37) 
 
 # Calculate the difference between provided percentages and calculated percentages
 percentage_difference <- provided_percentages - filtered_df$Percentage
 
 # Display the results
 comparison_df <- data.frame(Race = filtered_df$String, Provided_Percentage = provided_percentages, Calculated_Percentage = filtered_df$Percentage, Difference = percentage_difference)
 print(comparison_df)
 
 
 
 freq_table <- table(police_table_nola_use_of_force$Officer_Race)
 df <- data.frame(String = names(freq_table), Frequency = freq_table)
 df <- df[order(df$Frequency.Freq), ]
 df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])
 df
 
 ggplot(df, aes(x = String, y = Frequency.Freq)) +
   geom_bar(stat = "identity") +
   labs(x = "String", y = "Frequency", title = "Occurrence Rate of Officer by Race Total Officers Involved") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 # Filter the dataframe to include only White and Black race categories
 filtered_df <- df[df$String %in% c("White", "Black"), ]
 
 # Calculate the percentage of each race in the incidents dataset
 filtered_df$Percentage <- filtered_df$Frequency.Freq / sum(filtered_df$Frequency.Freq) * 100
 
 # Provided percentages for NOPD for White and Black
 provided_percentages <- c(57, 37)  
 
 # Calculate the difference between provided percentages and calculated percentages
 percentage_difference <- provided_percentages - filtered_df$Percentage
 
 # Display the results
 comparison_df <- data.frame(Race = filtered_df$String, Provided_Percentage = provided_percentages, Calculated_Percentage = filtered_df$Percentage, Difference = percentage_difference)
 print(comparison_df)
 
 # Create a new data frame for plotting
 plot_df <- data.frame(
   Race = comparison_df$Race,
   Percentage_Type = rep(c("% of Police Force", "% of Incidences"), each = nrow(comparison_df)),
   Percentage = rep(c(comparison_df$Provided_Percentage, comparison_df$Calculated_Percentage), times = 1),
   Difference = rep(comparison_df$Difference, times = 2)
 )
 
 # Create the side-by-side bar plot
 p <- ggplot(data = plot_df, aes(x = Race, y = Percentage, fill = Percentage_Type)) +
   geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
   geom_text(aes(label = ifelse(Percentage_Type == "% of Police Force", paste(round(Percentage, 2)), "")),
             position = position_dodge(width = 0.8), vjust = -0.5, color = "white") +
   geom_text(aes(label = ifelse(Percentage_Type == "% of Police Force", paste(round(Difference, 2), "%"), "")), 
             position = position_dodge(width = 0.8), vjust = 1.5, hjust = 0.5, color = "white") +
   geom_text(aes(label = paste(round(Percentage, 2))),
             position = position_dodge(width = 0.8), vjust = -0.5) +
   labs(title = "Comparison of Percentages: \n % of Police Force vs % of Incidences",
        x = "Race",
        y = "Percentage (%)",
        fill = "Percentage Type",
        caption = "Difference in Percentage") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   scale_fill_manual(values = c("% of Police Force" = "blue", "% of Incidences" = "red"))
 
 print(p)
 
 # If we compare the composition of the actual police force to the racial makeup of 
 # police force incidences, we observe notable differences between the two datasets. 
 # Specifically, when comparing Black and white police officers, there is a significant 
 # 25% difference in their incidence numbers compared to the police force makeup numbers. 
 # These are the numbers when using the the aggregated police table, which is 2% less 
 # than the incident table, resulting in a 9.77% lower incidence percentage for Black 
 # officers and a 15.77% higher incidence percentage for white officers compared to 
 # the police force makeup. While this data may normalize further with additional 
 # information, it does raise some concerns. As previously demonstrated in our weighted 
 # grading feature, I have incorporated this data to adjust the grading weights for this occurrence.

 
 # Filter the data by Officer_Race
 filtered_data <- incident_nola_use_of_force %>%
   filter(Officer_Race %in% c("White", "Majority_White"))
 
 # Create a bar plot of Grade column
 p <- ggplot(data = filtered_data, aes(x = Grade, fill = Officer_Race)) +
   geom_bar(position = "dodge") +
   labs(title = "Frequency of Grade by Officer Race",
        x = "Grade",
        y = "Frequency",
        fill = "Officer Race") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 print(p)
 
 
 summarized_data <- filtered_data %>%
   group_by(Officer_Race, Grade) %>%
   summarize(Count = n())
 
 # Filter the summarized data to include only "D" and "F" Grade entries
 summarized_data_filtered <- summarized_data %>%
   filter(Grade %in% c("D", "F"))
 
 print(summarized_data_filtered)
 
 
 # If we address problematic low-grade incidents involving white police officers 
 # by adjusting their incidence count based on the difference in their percentage 
 # of force makeup, we may witness a substantial reduction in violent incident cases. 
 # It's important to note that we are utilizing the lower aggregate police table difference 
 # mentioned earlier; when using the incidents table, the difference is actually 2% higher. 
 # Consequently, this adjustment could potentially account for up to 7.2% (subject to evaluation 
 # based on weighted features) of the above-average violent incidents, which can be attributed 
 # to white officers not aligning with their expected representation in the police force makeup. 
 # 
 extra_white_bad_incidences <- sum(summarized_data_filtered$Count)*.158
 print(extra_white_bad_incidences)
 
 sumerized_incidents <- incident_nola_use_of_force %>%
   filter(Grade %in% c("D", "F"))
 
 # 7.2% (depending on evaluation weighted feature) of above and beyond violent incidences are due to white officers not being 
 # in line with a minimum of their force makeup percentage  
 extra_white_bad_incidences/nrow(sumerized_incidents)
 

 # Calculate the frequency of occurrences
 frequency_data <- police_table_nola_use_of_force %>%
   group_by(Officer_Race, Use_of_Force_Type) %>%
   summarise(Frequency = n())
 
 # everybody loves exhibiting fire arms
 # Create a scatter plot of frequency
 ggplot(frequency_data, aes(x = Officer_Race, y = Frequency, color = Use_of_Force_Type)) +
   geom_point(position = position_dodge(width = 0.5), size = 3) +
   labs(x = "Officer Race", y = "Frequency", title = "Frequency of Use of Force Type by Officer Race") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 
 # Filter data for Officer_Race == "White"
 white_officers_data <- filter(police_table_nola_use_of_force, Officer_Race == "White")
 
 # Calculate the frequency of occurrences
 frequency_data <- white_officers_data %>%
   group_by(Use_of_Force_Type) %>%
   summarise(Frequency = n())
 
 # Create a histogram of frequency, looks fairly similar to all races, everyone is exhibiting firearm 
 ggplot(frequency_data, aes(x = Use_of_Force_Type, y = Frequency, fill = Use_of_Force_Type)) +
   geom_bar(stat = "identity") +
   labs(x = "Use of Force Type", y = "Frequency", title = "Frequency of Use of Force Type for White Officers") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "none")
 
 
 # Calculate the frequency of occurrences for each combination of Officer_Race and Grade
 frequency_data <- police_table_nola_use_of_force %>%
   group_by(Officer_Race, Grade) %>%
   summarise(Frequency = n())
 
 # important to recognize that these were skewed further by the % of the makeup of the whole force.
 # once we normalized the data in the weighted grade more White offices were showing 
 # Grades in the C range.
 # Create a single histogram of frequency for all Officer_Race entries
 ggplot(frequency_data, aes(x = Grade, y = Frequency, fill = Officer_Race)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Grade", y = "Frequency",
        title = "Frequency of Use of Force Grade by Officer Race") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "top")
 
 
 
 # "NOPD is well ahead of the curve with 23 percent women already within its ranks":
 # https://nopdnews.com/post/april-2021/nopd-proudly-commits-to-increasing-women-represent/
 
 mean(police_table_nola_use_of_force$weighted_sum)
 
 aggregate_mean_by_gender <- police_table_nola_use_of_force %>%
   group_by(Officer_Gender) %>%
   summarise(Mean_Weighted_Sum = mean(weighted_sum, na.rm = TRUE))
 
 aggregate_mean_by_gender
 
 selected_groups <- c("Male",  "Female")
 subset_data <- police_table_nola_use_of_force %>%
   group_by(Officer_Gender) %>%
   filter(Officer_Gender %in% selected_groups)
 
 ggplot(subset_data, aes(x = weighted_sum, fill = Officer_Gender)) +
   geom_density(alpha = 0.5) +
   labs(x = "Mean Weighted Sum", y = "Density", title = "Distribution of Mean Weighted Sum by Officer Gender") +
   scale_fill_manual(values = c(
     "Male" = "green",
     "Female" = "orange"
   )) +
   theme_minimal()
 
 
 freq_table <- table(police_table_nola_use_of_force$Officer_Gender)
 df <- data.frame(String = names(freq_table), Frequency = freq_table)
 df <- df[order(df$Frequency.Freq), ]
 df$String <- factor(df$String, levels = df$String[order(df$Frequency.Freq)])
print(freq_table)
 
 ggplot(df, aes(x = String, y = Frequency.Freq)) +
   geom_bar(stat = "identity") +
   labs(x = "String", y = "Frequency", title = "Occurrence Rate of Officer by Gender Total Officers Involved") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 
 # Calculate the percentage of each Gender in the incidents dataset
 df$Percentage <- df$Frequency.Freq / sum(df$Frequency.Freq) * 100
 
 # Provided percentages for NOPD for Male and Female
 provided_percentages <- c(23, 77) 
 
 # Calculate the difference between provided percentages and calculated percentages
 percentage_difference <- provided_percentages - df$Percentage
 
 # Display the results
 comparison_df <- data.frame(Gender = df$String, Provided_Percentage = provided_percentages, Calculated_Percentage = df$Percentage, Difference = percentage_difference)
 print(comparison_df)
 
 
 plot_df <- data.frame(
   Race = comparison_df$Gender,
   Percentage_Type = rep(c("% of Police Force", "% of Incidences"), each = nrow(comparison_df)),
   Percentage = rep(c(comparison_df$Provided_Percentage, comparison_df$Calculated_Percentage), times = 1),
   Difference = rep(comparison_df$Difference, times = 2)
 )
 
 # Create the side-by-side bar plot
 p <- ggplot(data = plot_df, aes(x = Race, y = Percentage, fill = Percentage_Type)) +
   geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
   geom_text(aes(label = round(Percentage, 2)), position = position_dodge(width = 0.8), vjust = -0.5) +
   geom_text(aes(label = paste(round(Difference, 2), "%")), 
             position = position_dodge(width = 0.8), vjust = 1.5, hjust = 0.5) +
   labs(title = "Comparison of Percentages by Gender : % of Police Force vs % of Incidences",
        x = "Race",
        y = "Percentage (%)",
        fill = "Percentage Type",
        caption = "Difference in Percentage") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   scale_fill_manual(values = c("% of Police Force" = "blue", "% of Incidences" = "red"))
 
 print(p)
 
 
 # Filter the data by Officer_gender
 filtered_data <- incident_nola_use_of_force %>%
   filter(Officer_Gender %in% c("Male", "Majority_Male"))
 
 # Create a bar plot of Grade column
 p <- ggplot(data = filtered_data, aes(x = Grade, fill = Officer_Gender)) +
   geom_bar(position = "dodge") +
   labs(title = "Frequency of Grade by Officer Gender",
        x = "Grade",
        y = "Frequency",
        fill = "Officer Gender") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 print(p)
 
 summarized_data <- filtered_data %>%
   group_by(Officer_Gender, Grade) %>%
   summarize(Count = n())
 
 # Filter the summarized data to include only "D" and "F" Grade entries
 summarized_data_filtered <- summarized_data %>%
   filter(Grade %in% c("D", "F"))
 
 print(summarized_data_filtered)
 
 # If problematic low grade incidences caused by male cops are decreased by their 
 # difference of percentage of force makeup to incidence count than we could see 
 # a significant drop in violent incident cases
 
 extra_male_bad_incidences <- sum(summarized_data_filtered$Count)*.119
 extra_male_bad_incidences
 
 sumerized_incidents <- incident_nola_use_of_force %>%
   filter(Grade %in% c("D", "F"))
 
 
 
 # 11 % of above and beyond violent incidences are due to male officers not being 
 # in line with at least minimizing to their force makeup percentage  some of those are 
 # probably mixed up in the previous White officer issue 
 
 
 extra_male_bad_incidences/nrow(sumerized_incidents)
 
 # Calculate the frequency of occurrences for each combination of Officer_Gender and Use_of_Force_Level
 police_table_nola_use_of_force$off
 frequency_data <- police_table_nola_use_of_force %>%
   group_by(Officer_Gender, Use_of_Force_Level) %>%
   summarise(Frequency = n())
 
 # Create a single histogram of frequency for all Officer_Gender entries
 ggplot(frequency_data, aes(x = Use_of_Force_Level, y = Frequency, fill = Officer_Gender)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Use of Force Level", y = "Frequency",
        title = "Frequency of Use of Force Level by Officer Gender") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "top")
 
 # Calculate the frequency of occurrences for each combination of Use_of_Force_Type and Officer_Gender
 frequency_data <- police_table_nola_use_of_force %>%
   group_by(Use_of_Force_Type, Officer_Gender) %>%
   summarise(Frequency = n())
 
 # Create a side-by-side histogram of frequency for all Use_of_Force_Type entries
 ggplot(frequency_data, aes(x = Use_of_Force_Type, y = Frequency, fill = Officer_Gender)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Use of Force Type", y = "Frequency",
        title = "Frequency of Officer Gender by Use of Force Type") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "top")
 
 # Calculate the total frequency for Officer Male and Officer Female separately
 total_frequency <- frequency_data %>%
   group_by(Officer_Gender) %>%
   summarise(Total_Frequency = sum(Frequency))
 
 # Calculate the percentage makeup of Officer Male and Officer Female based on their total frequencies
 percentage_data <- frequency_data %>%
   group_by(Use_of_Force_Type, Officer_Gender) %>%
   summarise(Frequency = sum(Frequency)) %>%
   left_join(total_frequency, by = "Officer_Gender") %>%
   mutate(Percentage = (Frequency / Total_Frequency) * 100)
 
 # Create a side-by-side histogram of the percentage makeup
 ggplot(percentage_data, aes(x = Use_of_Force_Type, y = Percentage, fill = Officer_Gender)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Use of Force Type", y = "Percentage (%)",
        title = "Percentage Makeup of Officer Gender by Use of Force Type") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "top") 
 # If the grading model is weighted without type of force having any weight it almost paint the 
 # over use of firearms as being a successful low injury option. Once we start weighing the use of 
 # firearms in the grading feature the use force type looks a little more dangerous. But weight alone on 
 # firearm exhibition can receive a B or C grade, for D and F grades a subject need to be injured.
 # So why are there so many injuries on the incidences with firearms exhibited as being the use of force type?
 
 
 frequency_data <- incident_nola_use_of_force %>%
   group_by(Use_of_Force_Type, Grade) %>%
   summarise(Frequency = n())
 
 ggplot(frequency_data, aes(x = Use_of_Force_Type, y = Frequency, fill = Grade)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Use of Force Type", y = "Frequency",
        title = "Frequency of Grade by Use of Force Type") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "top")
 
 
#  Filter data based on Officer_Number == 1
 filtered_data <- incident_nola_use_of_force %>%
   filter(Officer_Number == 1)
 
# Group by Use_of_Force_Type and calculate frequencies
 frequency_data <- filtered_data %>%
   group_by(Use_of_Force_Type) %>%
   summarise(
     Frequency_Hospitalized = sum(Subject_Hospitalized, na.rm = TRUE),
     Frequency_Injured = sum(Subject_Injured, na.rm = TRUE)
   )
 
# Create a histogram
# There is no direct one to one correlation with the data for type of force to 
# injury and hospitalization of the subject unless you filter by the number of 
# officers involved to be 1. Then we can assume the use of force type is 
# responsible for the injury. I have questions why the Exhibition of a firearm
# would be responsible for a decent portion of the total injuries. 
 
 ggplot(frequency_data, aes(x = Use_of_Force_Type)) +
   geom_bar(aes(y = Frequency_Hospitalized + Frequency_Injured, fill = "Hospitalized"), position = "stack", stat = "identity") +
   geom_bar(aes(y = Frequency_Injured, fill = "Injured"), position = "stack", stat = "identity") +
   labs(x = "Use of Force Type", y = "Frequency", title = "Combined Frequency of Hospitalized and \n Injured Subjects by Use of Force Type (Single Officer Incidents)") +
   scale_fill_manual(values = c("Hospitalized" = "blue", "Injured" = "red"), name = "Status") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "top")
 

 
 # Create a bar plot
 ggplot(data = incident_nola_use_of_force, aes(x = Subject_Influencing_Factors, fill = Grade)) +
   geom_bar(position = "dodge") +
   labs(title = "Frequency of Subject Influencing Factors by Grade",
        x = "Subject Influencing Factors",
        y = "Frequency") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
 
 summarized_data <- filtered_data %>%
   group_by(Subject_Influencing_Factors, Grade) %>%
   summarize(Count = n())
 # Filter the summarized data to include only "D" and "F" Grade entries
 summarized_data_filtered <- summarized_data %>%
   filter(Grade %in% c("D", "F"))
 
 print(summarized_data_filtered)

 
 # Calculate the count of "Alcohol" entries for both grades
 alcohol_count <- summarized_data_filtered %>%
   filter(Subject_Influencing_Factors == "Alcohol") %>%
   summarize(Count = sum(Count)) %>%
   pull(Count)
 alcohol_count
 # Calculate the total count of entries for both grades
 total_entries <- summarized_data_filtered %>%
   summarize(Total_Count = sum(Count)) %>%
   pull(Total_Count)
 
 # Sum the Total_Count values for all four columns
 total_entries_all <- sum(total_entries)
 total_entries_all
 # Calculate the percentage
 percentage_alcohol <- (alcohol_count /  total_entries_all) * 100
 
 # Print the result
 cat("Percentage of D an F Graded entries with 'Alcohol':", percentage_alcohol, "%\n")

 filtered_data <- incident_nola_use_of_force %>%
   filter( Service_Type == "Call for Service")
 
 incident_counts <- filtered_data %>%
   group_by(Grade) %>%
   summarize(Count = n())
 
 print(incident_counts) 
 
 # Incident grade for calls to service on mentaly unstable 
 ggplot(incident_counts, aes(x = Grade, y = Count, fill = Grade)) +
   geom_bar(stat = "identity") +
   labs(title = "Frequency of Incidences by Grade for Call for Service",
        x = "Grade",
        y = "Count") +
   theme_minimal()
 
 dficisentcount <- incident_counts %>%
   filter(Grade %in% c("D", "F")) 
 dficisentcount <- sum(dficisentcount$Count)
 print(dficisentcount)
  
 filtered_data <- incident_nola_use_of_force %>%
   filter(Subject_Influencing_Factors == "Mentally unstable", Service_Type == "Call for Service")
 
 incident_counts <- filtered_data %>%
   group_by(Grade) %>%
   summarize(Count = n())
 
 print(incident_counts) 
 ggplot(incident_counts, aes(x = Grade, y = Count, fill = Grade)) +
   geom_bar(stat = "identity") +
   labs(title = "Frequency of Incidences by Grade for Mentally Unstable Subjects (Call for Service)",
        x = "Grade",
        y = "Count") +
   theme_minimal()
 
 dfunstablecount <- incident_counts %>%
   filter(Grade %in% c("D", "F")) 
 dfunstablecount <- sum(dfunstablecount$Count)
 print(dfunstablecount) 
 
 # Calculate the percentage
 percentage_unstable <- (dfunstablecount /  dficisentcount) * 100
 
 # Print the result
 cat("Percentage of D an F Graded entries with 'Unstable' and Calls to Service:", percentage_unstable, "%\n")
 # Calculate the percentage of "Call for Service" entries
 percentage_call_for_service <- (sum(incident_nola_use_of_force$Service_Type == "Call for Service") / nrow(incident_nola_use_of_force)) * 100
 
 # Print the result
 cat("Percentage of 'Call for Service' entries:", percentage_call_for_service, "%\n")
 
 
  totalustablecalls<-percentage_call_for_service * (percentage_unstable/ 100)
 cat("Percentage of Total Violent Cases Effected Calls of Service and Mentally Unstable Subjects:",  totalustablecalls,  "%\n")
 #     Model Training
 #________________________________________________________________________________
 # Linear regression model prep 
 
 # making one big table where all these features are split up, if I did it over I
 # would probably do percentages for the race and gender features
 data <- incident_nola_use_of_force %>%
   mutate(
     Officer_Race_White = as.numeric(Officer_Race == "White"),
     Officer_Race_Majority_White = as.numeric(Officer_Race == "Majority_White"),
     Officer_Race_White_Black = as.numeric(Officer_Race == "White&Black"),
     Officer_Race_Hispanic = as.numeric(Officer_Race == "Hispanic"),
     Officer_Race_Black_Hispanic = as.numeric(Officer_Race == "Black&Hispanic"),
     Officer_Race_Black_Asian_Pacific_Islander = as.numeric(Officer_Race == "Black&Asian_Pacific_Islander"),
     Officer_Race_White_Race_Unknown = as.numeric(Officer_Race == "White&Race_Unknown"),
     Officer_Race_Majority_Asian_Pacific_Islander = as.numeric(Officer_Race == "Majority_Asian_Pacific_Islander"),
     Officer_Race_Mixed = as.numeric(Officer_Race == "Mixed"),
     Officer_Race_White_Hispanic = as.numeric(Officer_Race == "White&Hispanic"  ),
     Officer_Race_Hispanic_Asian_Pacific_Islander = as.numeric(Officer_Race == "Hispanic&Asian_Pacific_Islander"),
     Officer_Race_Majority_Hispanic = as.numeric(Officer_Race == "Majority_Hispanic"),
     Officer_Race_BlackandRace_Unknown = as.numeric(Officer_Race == "Black&Race_Unknown"),
     Officer_Race_Black_American_Indian = as.numeric(Officer_Race == "Black&American_Indian"),
     Officer_Race_Black = as.numeric(Officer_Race == "Black"), 
     Officer_Race_Majority_Black = as.numeric(Officer_Race == "Majority_Black"),
     Officer_Race_Asian_Pacific_Islander = as.numeric(Officer_Race == "Asian_Pacific_Islander"),
     Officer_Race_American_Indian = as.numeric(Officer_Race == "American_Indian"),
     Officer_Race_Unknown = as.numeric(Officer_Race == "Race_Unknown"),
     Officer_Race_White_Asian_Pacific_Islander = as.numeric(Officer_Race == "White&Asian_Pacific_Islander"),
     Officer_Race_White_American_Indian = as.numeric(Officer_Race == "White&American_Indian"),
     Officer_Gender_Male = as.numeric(Officer_Gender == "Male"),
     Officer_Gender_MaleandFemale = as.numeric(Officer_Gender == "Male&Female"),
     Officer_Gender_Majority_Male = as.numeric(Officer_Gender == "Majority_Male"),
     Officer_Gender_Female = as.numeric(Officer_Gender == "Female"),
     Officer_Gender_Majority_Female = as.numeric(Officer_Gender == "Majority_Female"),
     Subject_Race_White = as.numeric(Subject_Ethnicity == "White"),
     Subject_Race_BlackandHispanic = as.numeric(Subject_Ethnicity == "Black&Hispanic"),
     Subject_Race_BlackandIndian = as.numeric(Subject_Ethnicity == "Black&Indian"),
     Subject_Race_Black = as.numeric(Subject_Ethnicity == "Black"),
     Subject_Race_WhiteandBlack = as.numeric(Subject_Ethnicity == "White&Black"),
     Subject_Race_BlackandRace_Unknown = as.numeric(Subject_Ethnicity == "Black&Race_Unknown"),
     Subject_Race_Race_Unknown = as.numeric(Subject_Ethnicity == "Race_Unknown"),
     Subject_Race_Mixed = as.numeric(Subject_Ethnicity == "Mixed"),
     Subject_Race_WhiteandHispanic = as.numeric(Subject_Ethnicity == "White&Hispanic"),
     Subject_Race_Hispanic = as.numeric(Subject_Ethnicity == "Hispanic"),
     Subject_Race_Majority_Black = as.numeric(Subject_Ethnicity == "Majority_Black"),
     Subject_Race_Majority_Hispanic = as.numeric(Subject_Ethnicity == "Majority_Hispanic"),
     Subject_Race_Asian = as.numeric(Subject_Ethnicity == "Asian"),
     Subject_Race_BlackandIndiane = as.numeric(Subject_Ethnicity == "Black&Indian"),
     Subject_Race_Majority_White = as.numeric(Subject_Ethnicity == "Majority_White"),
     Subject_Race_Indian = as.numeric(Subject_Ethnicity == "Indian"),
     Subject_Gender_Male = as.numeric(Subject_Gender == "Male"),
     Subject_Gender_Female = as.numeric(Subject_Gender == "Female"),
     Subject_Gender_MaleandFemale = as.numeric(Subject_Gender == "Male&Female"),
     Subject_Gender_Majority_Male = as.numeric(Subject_Gender == "Majority_Male"),
     Forcetype_Firearm_Exhibited = as.numeric(Use_of_Force_Type == "Firearm (Exhibited)"),
     Forcetype_Hands = as.numeric(Use_of_Force_Type == "Hands"),
     Forcetype_Force_Escort_Tech = as.numeric(Use_of_Force_Type == "Force (Escort Tech)"),
     Forcetype_Head_Strike_No_Wep = as.numeric(Use_of_Force_Type == "Head Strike (No Wep)"),
     Forcetype_Hands_with_injury = as.numeric(Use_of_Force_Type == "Hands (with injury)"),
     Forcetype_Force_Take_Down = as.numeric(Use_of_Force_Type == "CEW Exhibited/Laser"),
     Forcetype_CEW_Exhibited_Laser = as.numeric(Use_of_Force_Type == "Firearm (Exhibited)"),
     Forcetype_Canine_Contact = as.numeric(Use_of_Force_Type == "Canine (Contact)"),
     Forcetype_Baton_PR_24_NonStrk = as.numeric(Use_of_Force_Type == "Baton/PR-24(NonStrk)"),
     Forcetype_VehPursuits_wInjury = as.numeric(Use_of_Force_Type == "VehPursuits w/Injury"),
     Forcetype_Takedown_winjury = as.numeric(Use_of_Force_Type == "Takedown (w/injury)"),
     Forcetype_Canine_NoBite = as.numeric(Use_of_Force_Type == "Canine (No Bite)"),
     Forcetype_Other = as.numeric(Use_of_Force_Type == "Other"),
     Forcetype_Handcuffed_Subject = as.numeric(Use_of_Force_Type == "Handcuffed Subject"),
     Forcetype_Baton_PR_24_Strike = as.numeric(Use_of_Force_Type == "Baton/PR-24 (Strike)"),
     Forcetype_CEW_Deployment = as.numeric(Use_of_Force_Type == "CEW Deployment"),
     Forcetype_Force_Defense_Tech = as.numeric(Use_of_Force_Type == "Force (Defense Tech)"),
     Forcetype_Firearm_Discharged = as.numeric(Use_of_Force_Type == "Firearm (Discharged)"),
     Forcetype_Baton_PR_24_Miss = as.numeric(Use_of_Force_Type == "Baton/PR-24 (Miss)"),
     Forcetype_Force_Neck_Holds = as.numeric(Use_of_Force_Type == "Force (Neck Holds)"),
     Subject_Influence_None_Detected =  as.numeric(Subject_Influencing_Factors == "None detected"),
     Subject_Influence_Mentally_Unstable =  as.numeric(Subject_Influencing_Factors == "Mentally unstable"),
     Subject_Influence_Unknown =  as.numeric(Subject_Influencing_Factors == "Unknown"),
     Subject_Influence_Alchohol =  as.numeric(Subject_Influencing_Factors == "Alchohol"),
     Subject_Build_Small = as.numeric(Subject_Build == "Small"),
     Subject_Build_Medium = as.numeric(Subject_Build == "Medium"),
     Subject_Build_Large = as.numeric(Subject_Build == "Large"),
     Subject_Build_XLarge = as.numeric(Subject_Build == "XLarge"),
     Division_Task_Force = as.numeric(Division == "Task Force" ),
     Division_Night_Watch = as.numeric(Division == "Night Watch" ),
     Division_C_Platoon = as.numeric(Division == "C Platoon" ),
     Division_General_Assignments = as.numeric(Division == "General Assignments" ),
     Division_A_Platoon = as.numeric(Division == "A Platoon" ),
     Division_Second_Watch = as.numeric(Division == "Second_Watch" ),
     Division_Narcotics = as.numeric(Division == "Narcotics" ),
     Division_3rd_District = as.numeric(Division == "3rd District" ),
     Division_Traffic = as.numeric(Division == "Traffice" ),
     Division_4th_District = as.numeric(Division == "4th District" ),
     Division_2nd_District = as.numeric(Division == "2nd District" ),
     Division_K9_Section = as.numeric(Division == "K9 Section" ),
     Division_Staff = as.numeric(Division == "Staff" ),
     Division_Bourbon_Promenade = as.numeric(Division == "Bourbon Promenade" ),
     Division_District_Investigations_Unit = as.numeric(Division == "District Investigations Unit" ),
     Division_7th_District = as.numeric(Division == "7th District" ),
     Division_8th_District = as.numeric(Division == "8th District" ),
     Division_Other_Support = as.numeric(Division == "Task Force" ),
     Division_3rd_Platoon = as.numeric(Division == "3rd Platoon" ),
     Division_6th_District = as.numeric(Division == "6th District" ),
     Division_Homicide = as.numeric(Division == "Homicide" ),
     Division_B_Platoon = as.numeric(Division == "B Platoon" ),
     Division_Day_Watch = as.numeric(Division == "Day Watch" ),
     Division_Tactical_Section = as.numeric(Division == "Tactical Section" ),
     Division_Investigations = as.numeric(Division == "Investigations" ),
     Division_1st_Platoon = as.numeric(Division == "1st Platoon" ),
     Division_Street_Gang_Unit = as.numeric(Division == "Street Gang Unit" ),
     Division_2nd_Platoon = as.numeric(Division == "2nd Platoon" ),
     Division_Force_Investigation_Team = as.numeric(Division == "Force Investigation Team" ),
     Division_1st_District = as.numeric(Division == "1st District" ),
     Division_5th_District = as.numeric(Division == "5th District" ),
     Weather_Clear = as.numeric(Weather_Condition == "Clear Conditions"),
     Weather_Other = as.numeric(Weather_Condition == "Other"),
     Weather_Rainy_Heavy = as.numeric(Weather_Condition == "Rainy Conditions - Heavy"),
     Weather_Rainy_Light = as.numeric(Weather_Condition == "Rainy Conditions - Light"),
     Weather_Rainy_Medium = as.numeric(Weather_Condition == "Rainy Conditions - Medium"),
     Weather_Foggy = as.numeric(Weather_Condition == "Foggy Condition"),
   )
 
 copdata <- police_table_nola_use_of_force %>%
   mutate(
     Officer_Race_White = as.numeric(Officer_Race == "White"),
     Officer_Race_Hispanic = as.numeric(Officer_Race == "Hispanic"),
     Officer_Race_Black = as.numeric(Officer_Race == "Black"), 
     Officer_Race_Asian_Pacific_Islander = as.numeric(Officer_Race == "Asian_Pacific_Islander"),
     Officer_Race_American_Indian = as.numeric(Officer_Race == "American_Indian"),
     Officer_Race_Unknown = as.numeric(Officer_Race == "Race_Unknown"),
     Officer_Gender_Male = as.numeric(Officer_Gender == "Male"),
     Officer_Gender_Female = as.numeric(Officer_Gender == "Female"),
     Officer_Gender_Majority_Female = as.numeric(Officer_Gender == "Majority_Female"),
     Forcetype_Firearm_Exhibited = as.numeric(Use_of_Force_Type == "Firearm (Exhibited)"),
     Forcetype_Hands = as.numeric(Use_of_Force_Type == "Hands"),
     Forcetype_Force_Escort_Tech = as.numeric(Use_of_Force_Type == "Force (Escort Tech)"),
     Forcetype_Head_Strike_No_Wep = as.numeric(Use_of_Force_Type == "Head Strike (No Wep)"),
     Forcetype_Hands_with_injury = as.numeric(Use_of_Force_Type == "Hands (with injury)"),
     Forcetype_Force_Take_Down = as.numeric(Use_of_Force_Type == "CEW Exhibited/Laser"),
     Forcetype_CEW_Exhibited_Laser = as.numeric(Use_of_Force_Type == "Firearm (Exhibited)"),
     Forcetype_Canine_Contact = as.numeric(Use_of_Force_Type == "Canine (Contact)"),
     Forcetype_Baton_PR_24_NonStrk = as.numeric(Use_of_Force_Type == "Baton/PR-24(NonStrk)"),
     Forcetype_VehPursuits_wInjury = as.numeric(Use_of_Force_Type == "VehPursuits w/Injury"),
     Forcetype_Takedown_winjury = as.numeric(Use_of_Force_Type == "Takedown (w/injury)"),
     Forcetype_Canine_NoBite = as.numeric(Use_of_Force_Type == "Canine (No Bite)"),
     Forcetype_Other = as.numeric(Use_of_Force_Type == "Other"),
     Forcetype_Handcuffed_Subject = as.numeric(Use_of_Force_Type == "Handcuffed Subject"),
     Forcetype_Baton_PR_24_Strike = as.numeric(Use_of_Force_Type == "Baton/PR-24 (Strike)"),
     Forcetype_CEW_Deployment = as.numeric(Use_of_Force_Type == "CEW Deployment"),
     Forcetype_Force_Defense_Tech = as.numeric(Use_of_Force_Type == "Force (Defense Tech)"),
     Forcetype_Firearm_Discharged = as.numeric(Use_of_Force_Type == "Firearm (Discharged)"),
     Forcetype_Baton_PR_24_Miss = as.numeric(Use_of_Force_Type == "Baton/PR-24 (Miss)"),
     Forcetype_Force_Neck_Holds = as.numeric(Use_of_Force_Type == "Force (Neck Holds)"),
   )

 # Split data into training and testing sets
 set.seed(123)
 trainIndex <- createDataPartition(data$weighted_sum, p = 0.7, list = FALSE)
 trainData <- data[trainIndex, ]
 testData <- data[-trainIndex, ]

 coptrainIndex <- createDataPartition(copdata$weighted_sum, p = 0.7, list = FALSE)
 coptrainData <- copdata[coptrainIndex, ]
 coptestData <- copdata[-coptrainIndex, ]
 
 # Build and train linear regression models
  
 modelcoprace <- lm(weighted_sum ~ Officer_Race_White +
                    Officer_Race_Majority_White +
                    Officer_Race_White_Black +
                    Officer_Race_Hispanic +
                    Officer_Race_Black_Hispanic +
                    Officer_Race_Black_Asian_Pacific_Islander +
                    Officer_Race_White_Race_Unknown +
                    Officer_Race_Majority_Asian_Pacific_Islander +
                    Officer_Race_Mixed +
                    Officer_Race_White_Hispanic +
                    Officer_Race_Hispanic_Asian_Pacific_Islander +
                    Officer_Race_Majority_Hispanic +
                    Officer_Race_BlackandRace_Unknown +
                    Officer_Race_Black_American_Indian +
                    Officer_Race_Black +
                    Officer_Race_Majority_Black +
                    Officer_Race_Asian_Pacific_Islander +
                    Officer_Race_American_Indian +
                    Officer_Race_Unknown +
                    Officer_Race_White_Asian_Pacific_Islander +
                    Officer_Race_White_American_Indian,
             data = trainData)
 
 
 # Calculate the correlation matrix
 correlation_matrix <- cor(trainData[, c("Officer_Race_White", "Officer_Race_Majority_White", 
                                         "Officer_Race_White_Black", "Officer_Race_Hispanic", 
                                         "Officer_Race_Black_Hispanic", "Officer_Race_Black_Asian_Pacific_Islander", 
                                         "Officer_Race_White_Race_Unknown", "Officer_Race_Majority_Asian_Pacific_Islander", 
                                         "Officer_Race_Mixed", "Officer_Race_White_Hispanic", 
                                         "Officer_Race_Hispanic_Asian_Pacific_Islander", "Officer_Race_Majority_Hispanic", 
                                         "Officer_Race_BlackandRace_Unknown", "Officer_Race_Black_American_Indian", 
                                         "Officer_Race_Black", "Officer_Race_Majority_Black", 
                                         "Officer_Race_Asian_Pacific_Islander", "Officer_Race_American_Indian", 
                                         "Officer_Race_Unknown", "Officer_Race_White_Asian_Pacific_Islander", 
                                         "Officer_Race_White_American_Indian")])
 
 # Create a heatmap of the correlation matrix
 
 correlation_matrix_melted <- melt(correlation_matrix)

 ggplot(data = correlation_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
   geom_tile() +
   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                        midpoint = 0, limit = c(-1,1), space = "Lab",
                        name="Correlation") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                    size = 10, hjust = 1)) +
   coord_fixed()
 # There is a moderate negative correlation between the officer race which 
 # means that when the number of officers of one race increase the number of 
 # other officers of different race will go down.
 
 

 
 modelsubinjured <- lm(weighted_sum ~ Subject_Injured,
                      data = trainData)
 
  
 modelcopinjured <- lm(weighted_sum ~ Officer_Injured,
                       data = trainData)
 
 modelsubhospital <- lm(weighted_sum ~ Subject_Hospitalized,
                       data = trainData) 
 
 
 modelsubrace <- lm(weighted_sum ~ Subject_Race_White +
             Subject_Race_BlackandHispanic +
             Subject_Race_BlackandIndian +
             Subject_Race_Black +
             Subject_Race_WhiteandBlack +
             Subject_Race_BlackandRace_Unknown +
             Subject_Race_Race_Unknown +
             Subject_Race_Mixed +
             Subject_Race_WhiteandHispanic +
             Subject_Race_Hispanic +
             Subject_Race_Majority_Black +
             Subject_Race_Majority_Hispanic +
             Subject_Race_Asian +
             Subject_Race_BlackandIndiane +
             Subject_Race_Majority_White +
             Subject_Race_Indian,
             data = trainData)
 
 modeldivision <- lm(weighted_sum ~ Division_Task_Force +
                     Division_Night_Watch +
                     Division_C_Platoon +
                     Division_General_Assignments  +
                     Division_A_Platoon +
                     Division_Second_Watch +
                     Division_Narcotics +
                     Division_3rd_District +
                     Division_Traffic +
                     Division_4th_District +
                     Division_2nd_District +
                     Division_K9_Section +
                     Division_Staff +
                     Division_Bourbon_Promenade +
                     Division_District_Investigations_Unit +
                     Division_7th_District +
                     Division_8th_District +
                     Division_Other_Support +
                     Division_3rd_Platoon +
                     Division_6th_District +
                     Division_Homicide +
                     Division_B_Platoon +
                     Division_Day_Watch +
                     Division_Tactical_Section +
                     Division_Investigations +
                     Division_1st_Platoon +
                     Division_Street_Gang_Unit +
                     Division_2nd_Platoon +
                     Division_Force_Investigation_Team +
                     Division_1st_District +
                     Division_5th_District,
                    data = trainData)
 
 modelweather <- lm(weighted_sum ~ Weather_Clear +
                    Weather_Other +
                    Weather_Rainy_Heavy +
                    Weather_Rainy_Light +
                    Weather_Rainy_Medium +
                    Weather_Foggy,
                      data = trainData)
 
 
 modelsubgender <- lm(weighted_sum ~ Subject_Gender_Male +
                      Subject_Gender_Female +
                      Subject_Gender_MaleandFemale +
                      Subject_Gender_Majority_Male,
                   data = trainData)
 
 modelsubsize <- lm(weighted_sum ~ Subject_Build_Small +
                    Subject_Build_Medium +
                    Subject_Build_Large +
                    Subject_Build_XLarge,
                      data = trainData)
 
 modelcopgender <- lm(weighted_sum ~ Officer_Gender_Male +
                     Officer_Gender_MaleandFemale +
                     Officer_Gender_Majority_Male +
                     Officer_Gender_Female +
                     Officer_Gender_Majority_Female,
                   data = trainData)
 
 modelinfluence <- lm(weighted_sum ~ Subject_Influence_None_Detected +
                      Subject_Influence_Mentally_Unstable +
                      Subject_Influence_Unknown +
                      Subject_Influence_Alchohol,
                   data = trainData)
 
 modelforcetype <- lm(weighted_sum ~ Forcetype_Firearm_Exhibited +
                        Forcetype_Hands +
                        Forcetype_Force_Escort_Tech +
                        Forcetype_Head_Strike_No_Wep +
                        Forcetype_Hands_with_injury +
                        Forcetype_Force_Take_Down +
                        Forcetype_CEW_Exhibited_Laser +
                        Forcetype_Canine_Contact +
                        Forcetype_Baton_PR_24_NonStrk +
                        Forcetype_VehPursuits_wInjury +
                        Forcetype_Takedown_winjury +
                        Forcetype_Canine_NoBite +
                        Forcetype_Other +
                        Forcetype_Handcuffed_Subject +
                        Forcetype_Baton_PR_24_Strike +
                        Forcetype_CEW_Deployment +
                        Forcetype_Force_Defense_Tech +
                        Forcetype_Firearm_Discharged +
                        Forcetype_Baton_PR_24_Miss +
                        Forcetype_Force_Neck_Holds,
                      data = trainData)
 

 modeledistance <- lm(weighted_sum ~ Distance_Between,
                      data = trainData) 
 

 modelesubage <- lm(weighted_sum ~ Subject_Age,
                      data = trainData) 
 
 modelsubheight <- lm(weighted_sum ~ Subject_Height,
                      data = trainData) 
 
 modelfocelevel <- lm(weighted_sum ~ Use_of_Force_Level,
                      data = trainData) 
 

 
 modelcopyearsservice <- lm(weighted_sum ~ Officer_Years_of_Service,
                      data = trainData) 
 

 
 modelcopage <- lm(weighted_sum ~ Officer_Age,
                      data = trainData) 
 

 modellightcondition <- lm(weighted_sum ~ Light_Condition,
                      data = trainData) 
 
 modelcombo <- lm(weighted_sum ~ Officer_Gender_Male +
                    Officer_Gender_MaleandFemale +
                    Officer_Gender_Majority_Male +
                    Officer_Gender_Female +
                    Officer_Gender_Majority_Female + Subject_Race_White +
                    Subject_Race_BlackandHispanic +
                    Subject_Race_BlackandIndian +
                    Subject_Race_Black +
                    Subject_Race_WhiteandBlack +
                    Subject_Race_BlackandRace_Unknown +
                    Subject_Race_Race_Unknown +
                    Subject_Race_Mixed +
                    Subject_Race_WhiteandHispanic +
                    Subject_Race_Hispanic +
                    Subject_Race_Majority_Black +
                    Subject_Race_Majority_Hispanic +
                    Subject_Race_Asian +
                    Subject_Race_BlackandIndiane +
                    Subject_Race_Majority_White +
                    Subject_Race_Indian + 
                    Officer_Years_of_Service + 
                    Subject_Gender_Male +
                    Subject_Gender_Female +
                    Subject_Gender_MaleandFemale +
                    Subject_Gender_Majority_Male + 
                    Subject_Age +
                    Subject_Influence_None_Detected +
                    Subject_Influence_Mentally_Unstable +
                    Subject_Influence_Unknown +
                    Subject_Influence_Alchohol + 
                    Forcetype_Firearm_Exhibited +
                    Forcetype_Hands +
                    Forcetype_Force_Escort_Tech +
                    Forcetype_Head_Strike_No_Wep +
                    Forcetype_Hands_with_injury +
                    Forcetype_Force_Take_Down +
                    Forcetype_CEW_Exhibited_Laser +
                    Forcetype_Canine_Contact +
                    Forcetype_Baton_PR_24_NonStrk +
                    Forcetype_VehPursuits_wInjury +
                    Forcetype_Takedown_winjury +
                    Forcetype_Canine_NoBite +
                    Forcetype_Other +
                    Forcetype_Handcuffed_Subject +
                    Forcetype_Baton_PR_24_Strike +
                    Forcetype_CEW_Deployment +
                    Forcetype_Force_Defense_Tech +
                    Forcetype_Firearm_Discharged +
                    Forcetype_Baton_PR_24_Miss +
                    Forcetype_Force_Neck_Holds +
                    Use_of_Force_Level +
                    Subject_Injured +
                    Officer_Injured +
                    Subject_Hospitalized +
                    Light_Condition,
                           data = trainData) 

 
 
 
 modelprecog <- lm(weighted_sum ~ Officer_Gender_Male +
                  Officer_Gender_MaleandFemale +
                  Officer_Gender_Majority_Male +
                  Officer_Gender_Female +
                  Officer_Gender_Majority_Female + Subject_Race_White +
                  Subject_Race_BlackandHispanic +
                  Subject_Race_BlackandIndian +
                  Subject_Race_Black +
                  Subject_Race_WhiteandBlack +
                  Subject_Race_BlackandRace_Unknown +
                  Subject_Race_Race_Unknown +
                  Subject_Race_Mixed +
                  Subject_Race_WhiteandHispanic +
                  Subject_Race_Hispanic +
                  Subject_Race_Majority_Black +
                  Subject_Race_Majority_Hispanic +
                  Subject_Race_Asian +
                  Subject_Race_BlackandIndiane +
                  Subject_Race_Majority_White +
                  Subject_Race_Indian + 
                  Officer_Years_of_Service + 
                  Subject_Gender_Male +
                  Subject_Gender_Female +
                  Subject_Gender_MaleandFemale +
                  Subject_Gender_Majority_Male + 
                  Subject_Age +
                  Subject_Influence_None_Detected +
                  Subject_Influence_Mentally_Unstable +
                  Subject_Influence_Unknown +
                  Subject_Influence_Alchohol + 
                  Forcetype_Firearm_Exhibited +
                  Forcetype_Hands +
                  Forcetype_Force_Escort_Tech +
                  Forcetype_Head_Strike_No_Wep +
                  Forcetype_Hands_with_injury +
                  Forcetype_Force_Take_Down +
                  Forcetype_CEW_Exhibited_Laser +
                  Forcetype_Canine_Contact +
                  Forcetype_Baton_PR_24_NonStrk +
                  Forcetype_VehPursuits_wInjury +
                  Forcetype_Takedown_winjury +
                  Forcetype_Canine_NoBite +
                  Forcetype_Other +
                  Forcetype_Handcuffed_Subject +
                  Forcetype_Baton_PR_24_Strike +
                  Forcetype_CEW_Deployment +
                  Forcetype_Force_Defense_Tech +
                  Forcetype_Firearm_Discharged +
                  Forcetype_Baton_PR_24_Miss +
                  Forcetype_Force_Neck_Holds +
                  Light_Condition,
                data = trainData)  



copmodelcopyearsservice <- lm(weighted_sum ~ Officer_Years_of_Service,
                              data = coptrainData) 

copmodelfocelevel <- lm(weighted_sum ~ Use_of_Force_Level,
                        data = coptrainData) 

copmodelforcetype <- lm(weighted_sum ~ Forcetype_Firearm_Exhibited +
                          Forcetype_Hands +
                          Forcetype_Force_Escort_Tech +
                          Forcetype_Head_Strike_No_Wep +
                          Forcetype_Hands_with_injury +
                          Forcetype_Force_Take_Down +
                          Forcetype_CEW_Exhibited_Laser +
                          Forcetype_Canine_Contact +
                          Forcetype_Baton_PR_24_NonStrk +
                          Forcetype_VehPursuits_wInjury +
                          Forcetype_Takedown_winjury +
                          Forcetype_Canine_NoBite +
                          Forcetype_Other +
                          Forcetype_Handcuffed_Subject +
                          Forcetype_Baton_PR_24_Strike +
                          Forcetype_CEW_Deployment +
                          Forcetype_Force_Defense_Tech +
                          Forcetype_Firearm_Discharged +
                          Forcetype_Baton_PR_24_Miss +
                          Forcetype_Force_Neck_Holds,
                        data = coptrainData)



copmodelcopgender <- lm(weighted_sum ~ Officer_Gender_Male +
                       Officer_Gender_Female,
                     data = coptrainData)

copmodelcoprace <- lm(weighted_sum ~ Officer_Race_White +
                     Officer_Race_Hispanic +
                     Officer_Race_Black +
                     Officer_Race_Asian_Pacific_Islander +
                     Officer_Race_American_Indian +
                     Officer_Race_Unknown,
                   data = coptrainData)

copmodelfull <- lm(weighted_sum ~ Use_of_Force_Level + 
                          Officer_Years_of_Service +
                          Officer_Gender_Male +
                          Officer_Gender_Female +
                          Officer_Race_White +
                          Officer_Race_Hispanic +
                          Officer_Race_Black +
                          Officer_Race_Asian_Pacific_Islander +
                          Officer_Race_American_Indian +
                          Officer_Race_Unknown + 
                          Forcetype_Firearm_Exhibited +
                          Forcetype_Hands +
                          Forcetype_Force_Escort_Tech +
                          Forcetype_Head_Strike_No_Wep +
                          Forcetype_Hands_with_injury +
                          Forcetype_Force_Take_Down +
                          Forcetype_CEW_Exhibited_Laser +
                          Forcetype_Canine_Contact +
                          Forcetype_Baton_PR_24_NonStrk +
                          Forcetype_VehPursuits_wInjury +
                          Forcetype_Takedown_winjury +
                          Forcetype_Canine_NoBite +
                          Forcetype_Other +
                          Forcetype_Handcuffed_Subject +
                          Forcetype_Baton_PR_24_Strike +
                          Forcetype_CEW_Deployment +
                          Forcetype_Force_Defense_Tech +
                          Forcetype_Firearm_Discharged +
                          Forcetype_Baton_PR_24_Miss +
                          Forcetype_Force_Neck_Holds,
                        data = coptrainData)

# Create a table to store evaluation metrics for each model
 evaluation_table <- data.frame(
   Model = character(0),
   MSE = numeric(0),
   RMSE = numeric(0),
   MAE = numeric(0),
   R_squared = numeric(0)
 )
 
 

 # Iterate through different models and store their evaluation metrics
 models <- list(modelcoprace, modelcopgender, modelsubrace, modelcopyearsservice,
                modelcopage, modelsubinjured, modelcopinjured,  modelsubhospital,
                modelsubgender, modelsubsize, modelesubage, modelsubheight,
                modelinfluence, modelforcetype, modelfocelevel, modeledistance, 
                modeldivision, modelweather, modellightcondition, modelcombo, modelprecog)  # Add more models as needed
 model_names <- c("modelcoprace", "modelcopgender", "modelsubrace", "modelcopyearsservice", 
                  "modelcopage", "modelsubinjured", "modelcopinjured", "modelsubhospital",
                  "modelsubgender", "modelsubsize", "modelesubage", "modelsubheight",
                  "modelinfluence", "modelforcetype", "modelfocelevel", "modeledistance", 
                  "modeldivision", "modelweather", "modellightcondition", "modelcombo", "modelprecog") 
 
 for (i in seq_along(models)) {
   model_name <- model_names[i]
   model <- models[[i]]
   
   predictions <- predict(model, newdata = testData)
   
   mse <- mean((testData$weighted_sum - predictions)^2)
   rmse <- sqrt(mse)
   mae <- mean(abs(testData$weighted_sum - predictions))
   
   residuals <- testData$weighted_sum - predictions
   rss <- sum(residuals^2)
   tss <- sum((testData$weighted_sum - mean(testData$weighted_sum))^2)
   r_squared <- 1 - (rss / tss)
   
   # Round R-squared to the 5th decimal place
   r_squared_rounded <- round(r_squared, 5)
   
   new_row <- data.frame(
     Model = model_name,
     MSE = mse,
     RMSE = rmse,
     MAE = mae,
     R_squared = r_squared_rounded  # Use the rounded R-squared value
   )
   
   evaluation_table <- rbind(evaluation_table, new_row)
 }
 ############ These errors are most likely due to the negative correlation 
 # of Black and White Officers
 
 # Print the evaluation table
 print(evaluation_table)
 
 # Iterate through different models and store their evaluation metrics
 copmodels <- list(copmodelcoprace, copmodelcopgender, copmodelcopyearsservice, copmodelfull,
                copmodelforcetype, copmodelfocelevel)  # Add more models as needed
 copmodel_names <- c("copmodelcoprace", "copmodelcopgender", "copmodelcopyearsservice", "copmodelfull",
                  "copmodelforcetype", "copmodelfocelevel") 
 # Create a table to store evaluation metrics for each cop model
 copevaluation_table <- data.frame(
   Model = character(0),
   MSE = numeric(0),
   RMSE = numeric(0),
   MAE = numeric(0),
   R_squared = numeric(0)
 )
 
 # Iterate through different cop models and store their evaluation metrics
 for (i in seq_along(copmodels)) {
   model_name <- copmodel_names[i]
   model <- copmodels[[i]]
   
   coppredictions <- predict(model, newdata = coptestData)
   
   mse <- mean((coptestData$weighted_sum - coppredictions)^2)
   rmse <- sqrt(mse)
   mae <- mean(abs(coptestData$weighted_sum - coppredictions))
   
   copresiduals <- coptestData$weighted_sum - coppredictions
   rss <- sum(copresiduals^2)
   tss <- sum((coptestData$weighted_sum - mean(coptestData$weighted_sum))^2)
   r_squared <- 1 - (rss / tss)
   
   # Round R-squared to the 5th decimal place
   r_squared_rounded <- round(r_squared, 5)
   
   new_row <- data.frame(
     Model = model_name,
     MSE = mse,
     RMSE = rmse,
     MAE = mae,
     R_squared = r_squared_rounded  # Use the rounded R-squared value
   )
   
   copevaluation_table <- rbind(copevaluation_table, new_row)
 }
 
 
 # Print the evaluation table
 # Most of the the officers tables model's prediction is coming from the weight 
 # on use of for type in our weighted feature .28 r2 here and .35 on the previous 
 # instance table. Before an incident occurrences this the main predictor. I'm not
 # continuing this copmodel any further. This data is evident in the main model
 print(copevaluation_table)
 
 
 #  the randomForest model
 # Split the data into training and testing sets
 set.seed(123)
 trainIndex <- createDataPartition(data$weighted_sum, p = 0.7, list = FALSE)
 trainData <- data[trainIndex, ]
 testData <- data[-trainIndex, ]
 
 # Create the Random Forest model
 model_rf <- randomForest(weighted_sum ~ Officer_Gender_Male +
                            Officer_Gender_MaleandFemale +
                            Officer_Gender_Majority_Male +
                            Officer_Gender_Female +
                            Officer_Gender_Majority_Female + Subject_Race_White +
                            Subject_Race_BlackandHispanic +
                            Subject_Race_BlackandIndian +
                            Subject_Race_Black +
                            Subject_Race_WhiteandBlack +
                            Subject_Race_BlackandRace_Unknown +
                            Subject_Race_Race_Unknown +
                            Subject_Race_Mixed +
                            Subject_Race_WhiteandHispanic +
                            Subject_Race_Hispanic +
                            Subject_Race_Majority_Black +
                            Subject_Race_Majority_Hispanic +
                            Subject_Race_Asian +
                            Subject_Race_BlackandIndiane +
                            Subject_Race_Majority_White +
                            Subject_Race_Indian + 
                            Officer_Years_of_Service + 
                            Subject_Gender_Male +
                            Subject_Gender_Female +
                            Subject_Gender_MaleandFemale +
                            Subject_Gender_Majority_Male + 
                            Subject_Age +
                            Subject_Influence_None_Detected +
                            Subject_Influence_Mentally_Unstable +
                            Subject_Influence_Unknown +
                            Subject_Influence_Alchohol + 
                            Forcetype_Firearm_Exhibited +
                            Forcetype_Hands +
                            Forcetype_Force_Escort_Tech +
                            Forcetype_Head_Strike_No_Wep +
                            Forcetype_Hands_with_injury +
                            Forcetype_Force_Take_Down +
                            Forcetype_CEW_Exhibited_Laser +
                            Forcetype_Canine_Contact +
                            Forcetype_Baton_PR_24_NonStrk +
                            Forcetype_VehPursuits_wInjury +
                            Forcetype_Takedown_winjury +
                            Forcetype_Canine_NoBite +
                            Forcetype_Other +
                            Forcetype_Handcuffed_Subject +
                            Forcetype_Baton_PR_24_Strike +
                            Forcetype_CEW_Deployment +
                            Forcetype_Force_Defense_Tech +
                            Forcetype_Firearm_Discharged +
                            Forcetype_Baton_PR_24_Miss +
                            Forcetype_Force_Neck_Holds +
                            Use_of_Force_Level +
                            Subject_Injured +
                            Officer_Injured +
                            Subject_Hospitalized +
                            Light_Condition, data = trainData, ntree = 100, mtry = 5)
 
 model_rfprecog <- randomForest(weighted_sum ~ Officer_Gender_Male +
                            Officer_Gender_MaleandFemale +
                            Officer_Gender_Majority_Male +
                            Officer_Gender_Female +
                            Officer_Gender_Majority_Female + Subject_Race_White +
                            Subject_Race_BlackandHispanic +
                            Subject_Race_BlackandIndian +
                            Subject_Race_Black +
                            Subject_Race_WhiteandBlack +
                            Subject_Race_BlackandRace_Unknown +
                            Subject_Race_Race_Unknown +
                            Subject_Race_Mixed +
                            Subject_Race_WhiteandHispanic +
                            Subject_Race_Hispanic +
                            Subject_Race_Majority_Black +
                            Subject_Race_Majority_Hispanic +
                            Subject_Race_Asian +
                            Subject_Race_BlackandIndiane +
                            Subject_Race_Majority_White +
                            Subject_Race_Indian + 
                            Officer_Years_of_Service + 
                            Subject_Gender_Male +
                            Subject_Gender_Female +
                            Subject_Gender_MaleandFemale +
                            Subject_Gender_Majority_Male + 
                            Subject_Age +
                            Subject_Influence_None_Detected +
                            Subject_Influence_Mentally_Unstable +
                            Subject_Influence_Unknown +
                            Subject_Influence_Alchohol + 
                            Forcetype_Firearm_Exhibited +
                            Forcetype_Hands +
                            Forcetype_Force_Escort_Tech +
                            Forcetype_Head_Strike_No_Wep +
                            Forcetype_Hands_with_injury +
                            Forcetype_Force_Take_Down +
                            Forcetype_CEW_Exhibited_Laser +
                            Forcetype_Canine_Contact +
                            Forcetype_Baton_PR_24_NonStrk +
                            Forcetype_VehPursuits_wInjury +
                            Forcetype_Takedown_winjury +
                            Forcetype_Canine_NoBite +
                            Forcetype_Other +
                            Forcetype_Handcuffed_Subject +
                            Forcetype_Baton_PR_24_Strike +
                            Forcetype_CEW_Deployment +
                            Forcetype_Force_Defense_Tech +
                            Forcetype_Firearm_Discharged +
                            Forcetype_Baton_PR_24_Miss +
                            Forcetype_Force_Neck_Holds +
                            Light_Condition, data = trainData, ntree = 100, mtry = 5)
 
 # Make predictions on the testing set
 predictions <- predict(model_rf, newdata = testData)

 # Create a scatter plot to compare predicted vs. actual values
 plot(testData$weighted_sum, predictions, 
      xlab = "Actual weighted_sum", ylab = "Predicted weighted_sum",
      main = "Predicted vs. Actual", pch = 16, col = "blue")
 
 # Add a reference line
 abline(a = 0, b = 1, col = "red")
 
 # Calculate and print accuracy

 mse <- mean((predictions - testData$weighted_sum)^2)
 print(paste("Mean Squared Error:", mse))
 # Calculate Root Mean Squared Error (RMSE):
   
 rmse <- sqrt(mse)
 print(paste("Root Mean Squared Error:", rmse))
 #Calculate Mean Absolute Error (MAE):
    
 mae <- mean(abs(predictions - testData$weighted_sum))
 print(paste("Mean Absolute Error:", mae))
 #Calculate R-squared (R2):
 rss <- sum((testData$weighted_sum - predictions)^2)  # Residual Sum of Squares
 tss <- sum((testData$weighted_sum - mean(testData$weighted_sum))^2)  # Total Sum of Squares
 r_squared <- 1 - (rss / tss)
 print(paste("R-squared:", r_squared))
 
 # examine the importance of features
 print(model_rf$importance)
 
 
 # Create a new row for Random Forest results
 rf_row <- data.frame(
   Model = "Random Forest",
   MSE = mse,
   RMSE = rmse,
   MAE = mae,
   R_squared = round(r_squared, 5)  # Round R-squared to 5 decimal places
 )
 
 # Append the Random Forest results to the existing evaluation table
 evaluation_table <- rbind(evaluation_table, rf_row)
 
 # Print the updated evaluation table
 print(evaluation_table)
 
 
 # Make predictions on the testing set
 predictions <- predict(model_rfprecog, newdata = testData)
 
 # Evaluate the model's performance
 # Create a scatter plot to compare predicted vs. actual values
 plot(testData$weighted_sum, predictions, 
      xlab = "Actual weighted_sum", ylab = "Predicted weighted_sum",
      main = "Predicted vs. Actual", pch = 16, col = "blue")
 
 # Add a reference line
 abline(a = 0, b = 1, col = "red")
 
 # Print Accuracy  
 mse <- mean((predictions - testData$weighted_sum)^2)
 print(paste("Mean Squared Error:", mse))
 # Calculate Root Mean Squared Error (RMSE):
 
 rmse <- sqrt(mse)
 print(paste("Root Mean Squared Error:", rmse))
 #Calculate Mean Absolute Error (MAE):
 
 mae <- mean(abs(predictions - testData$weighted_sum))
 print(paste("Mean Absolute Error:", mae))
 #Calculate R-squared (R2):
 rss <- sum((testData$weighted_sum - predictions)^2)  # Residual Sum of Squares
 tss <- sum((testData$weighted_sum - mean(testData$weighted_sum))^2)  # Total Sum of Squares
 r_squared <- 1 - (rss / tss)
 print(paste("R-squared:", r_squared))
 
 # examine the importance of features
 print(model_rf$importance)
 
 
 # Create a new row for Random Forest results
 rf_row <- data.frame(
   Model = "Random Forest Precog",
   MSE = mse,
   RMSE = rmse,
   MAE = mae,
   R_squared = round(r_squared, 5)  # Round R-squared to 5 decimal places
 )
 
 # Append the Random Forest results to the existing evaluation table
 evaluation_table <- rbind(evaluation_table, rf_row)
 
 # Print the updated evaluation table
 print(evaluation_table) 
 
 
 # Xboost Model

 # the feature Subject_Hospitalized really boosts this models performance,
 # converting it into the appropriate format for xgboost.
 numeric_factor_trainData <- trainData[, sapply(trainData, is.numeric) | sapply(trainData, is.factor)]
 numeric_factor_testData <-  testData[, sapply(testData, is.numeric) | sapply(testData, is.factor)]

 # Create the x_train matrix
 x_train <- as.matrix(numeric_factor_trainData[, -which(names(numeric_factor_trainData) == "weighted_sum")])

 y_train <- trainData$weighted_sum
 x_test <- as.matrix(numeric_factor_testData[, -which(names(numeric_factor_testData) == "weighted_sum")])
 
#  Create an xgb.DMatrix object for training and testing data:
 dtrain <- xgb.DMatrix(data = x_train, label = y_train)
 dtest <- xgb.DMatrix(data = x_test)

# Define hyperparameters
 params <- list(
   objective = "reg:squarederror",  # Regression task
   max_depth = 6,                   # Maximum tree depth
   eta = 0.3,                       # Learning rate
   nthread = 2                      # Number of parallel threads
                    # Number of boosting rounds
 )


 model_xgb <- xgboost(data = dtrain, params = params, nrounds = 100, verbose = 0)

 # Get feature importance
 importance <- xgb.importance(model = model_xgb)
list(importance)
 ggplot(data = importance, aes(x = Feature, y = Gain, fill = Gain)) +
   geom_bar(stat = "identity") +
   labs(x = "Feature", y = "Gain", title = "Feature Importance") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "top")

 predictions <- predict(model_xgb, newdata = dtest)
 y_test <- testData$weighted_sum
 # Create a data frame with predictions and actual values
 scatter_data <- data.frame(Predicted = predictions, Actual = y_test)

 # Create a scatter plot
 ggplot(scatter_data, aes(x = Actual, y = Predicted)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
   labs(x = "Actual Weighted Sum", y = "Predicted Weighted Sum", title = "Scatter Plot of Predicted vs Actual") +
   theme_minimal()


 # Evaluate the model's performance
 rounded_predictions <- round(predictions, digits = 3)

 accuracy <- mean(rounded_predictions == testData$weighted_sum)
 print(paste("Accuracy:", accuracy))

 mse <- mean((rounded_predictions - testData$weighted_sum)^2)
 print(paste("Mean Squared Error:", mse))
 # Calculate Root Mean Squared Error (RMSE):

 rmse <- sqrt(mse)
 print(paste("Root Mean Squared Error:", rmse))
 #Calculate Mean Absolute Error (MAE):

 mae <- mean(abs(rounded_predictions - testData$weighted_sum))
 print(paste("Mean Absolute Error:", mae))
 #Calculate R-squared (R2):
 rss <- sum((testData$weighted_sum - rounded_predictions)^2)  # Residual Sum of Squares
 tss <- sum((testData$weighted_sum - mean(testData$weighted_sum))^2)  # Total Sum of Squares
 r_squared <- 1 - (rss / tss)
 print(paste("R-squared:", r_squared))

 # Create a new row for Random Forest results
 rf_row <- data.frame(
   Model = "Xboost",
   MSE = mse,
   RMSE = rmse,
   MAE = mae,
   R_squared = round(r_squared, 5)  # Round R-squared to 5 decimal places
 )

 # Append the Random Forest results to the existing evaluation table
 evaluation_table <- rbind(evaluation_table, rf_row)

 # Print the updated evaluation table
 print(evaluation_table)

 # XGBoost precog Model
 
 # the feature Subject_Hospitalized really boosts this models performance, it doesn't really affect other models
 # converting it into the appropriate format for xgboost. 
 numeric_factor_trainData <- trainData[, sapply(trainData, is.numeric) | sapply(trainData, is.factor)]
 numeric_factor_testData <-  testData[, sapply(testData, is.numeric) | sapply(testData, is.factor)]
 # Filter only numeric and factor columns from trainData
 numeric_factor_precogData_train <- numeric_factor_trainData[, !(names(numeric_factor_trainData) %in% c(
   "Use_of_Force_Level",
   "Subject_Injured",
   "Officer_Injured",
   "Subject_Hospitalized"
 ))]
 
 # Filter only numeric and factor columns from testData
 numeric_factor_precogData_test <- numeric_factor_testData[, !(names(numeric_factor_testData) %in% c(
   "Use_of_Force_Level",
   "Subject_Injured",
   "Officer_Injured",
   "Subject_Hospitalized"
 ))]
 
 
 
 
 
 # Create the x_train matrix
 x_train <- as.matrix(numeric_factor_precogData_train[, -which(names(numeric_factor_precogData_train) == "weighted_sum")])
 
 y_train <- numeric_factor_precogData_train$weighted_sum
 x_test <- as.matrix(numeric_factor_precogData_test[, -which(names(numeric_factor_precogData_test) == "weighted_sum")])
 
 
 #  Create an xgb.DMatrix object for training and testing data:
 dtrain <- xgb.DMatrix(data = x_train, label = y_train)
 dtest <- xgb.DMatrix(data = x_test)
 
 # Define hyperparameters
 params <- list(
   objective = "reg:squarederror",  # Regression task
   max_depth = 6,                   # Maximum tree depth
   eta = 0.3,                       # Learning rate
   nthread = 2                      # Number of parallel threads
   # Number of boosting rounds
 )
 
 
 model_xgb <- xgboost(data = dtrain, params = params, nrounds = 100, verbose = 0)
 
 # Get feature importance
 importance <- xgb.importance(model = model_xgb)
 list(importance)
 ggplot(data = importance, aes(x = Feature, y = Gain, fill = Gain)) +
   geom_bar(stat = "identity") +
   labs(x = "Feature", y = "Gain", title = "Feature Importance") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "top")
 
 predictions <- predict(model_xgb, newdata = dtest)
 y_test <- numeric_factor_precogData_test$weighted_sum
 # Create a data frame with predictions and actual values
 scatter_data <- data.frame(Predicted = predictions, Actual = y_test)
 
 # Create a scatter plot
 ggplot(scatter_data, aes(x = Actual, y = Predicted)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
   labs(x = "Actual Weighted Sum", y = "Predicted Weighted Sum", title = "Scatter Plot of Predicted vs Actual") +
   theme_minimal()
 
 
 # Evaluate the model's performance
 rounded_predictions <- round(predictions, digits = 3)
 
 accuracy <- mean(rounded_predictions == testData$weighted_sum)
 print(paste("Accuracy:", accuracy))
 
 mse <- mean((rounded_predictions - testData$weighted_sum)^2)
 print(paste("Mean Squared Error:", mse))
 # Calculate Root Mean Squared Error (RMSE):
 
 rmse <- sqrt(mse)
 print(paste("Root Mean Squared Error:", rmse))
 #Calculate Mean Absolute Error (MAE):
 
 mae <- mean(abs(rounded_predictions - testData$weighted_sum))
 print(paste("Mean Absolute Error:", mae))
 #Calculate R-squared (R2):
 rss <- sum((testData$weighted_sum - rounded_predictions)^2)  # Residual Sum of Squares
 tss <- sum((testData$weighted_sum - mean(testData$weighted_sum))^2)  # Total Sum of Squares
 r_squared <- 1 - (rss / tss)
 print(paste("R-squared:", r_squared))
 
 # Create a new row for Random Forest results
 rf_row <- data.frame(
   Model = "XGBoost Precog",
   MSE = mse,
   RMSE = rmse,
   MAE = mae,
   R_squared = round(r_squared, 5)  # Round R-squared to 5 decimal places
 )
 
 # Append the Random Forest results to the existing evaluation table
 evaluation_table <- rbind(evaluation_table, rf_row)
 
 # Print the updated evaluation table
 print(evaluation_table)
 
 
 # Here is maybe a tip to help install packages bellow
 # install.packages("reticulate")
 # reticulate::install_miniconda()
 # I would Try this one bellow first
 # reticulate::conda_create(envname = "r-tensorflow", python = "3.9")
 # This one is specific to my computer build 
# reticulate::conda_install(envname = "r-tensorflow", packages = "tensorflow=2.6.0")

 # Install and load required packages
 use_condaenv("r-tensorflow", required = TRUE)
 # install.packages("keras")
 #Set up Python environment
 # notes for my enviroment 
 # use_python("/home/chris/.local/share/r-miniconda/envs/r-tensorflow/bin/python")
 # install.packages("tensorflow", update = TRUE)
 # tensorflow::tf_version()
 # py_config()

 # Check data types
 class(numeric_factor_trainData)  # Should be matrix-like We will train bellow
 class(trainData$weighted_sum)    # Should be numeric

# best for #2 analysis variable and #3/#4 og variable
numeric_matrix_trainData <- as.matrix(numeric_factor_trainData[, which(names(numeric_factor_trainData) %in% c( "Officer_Gender_Male",
                                                                                                               "Officer_Gender_MaleandFemale",
                                                                                                               "Officer_Gender_Majority_Male",
                                                                                                               "Officer_Gender_Female",
                                                                                                               "Officer_Gender_Majority_Female",
                                                                                                               "Subject_Race_White",
                                                                                                               "Subject_Race_BlackandHispanic",
                                                                                                               "Subject_Race_BlackandIndian",
                                                                                                               "Subject_Race_Black",
                                                                                                               "Subject_Race_WhiteandBlack",
                                                                                                               "Subject_Race_BlackandRace_Unknown",
                                                                                                               "Subject_Race_Race_Unknown",
                                                                                                               "Subject_Race_Mixed",
                                                                                                               "Subject_Race_WhiteandHispanic",
                                                                                                               "Subject_Race_Hispanic",
                                                                                                               "Subject_Race_Majority_Black",
                                                                                                               "Subject_Race_Majority_Hispanic",
                                                                                                               "Subject_Race_Asian",
                                                                                                               "Subject_Race_BlackandIndiane",
                                                                                                               "Subject_Race_Majority_White",
                                                                                                               "Subject_Race_Indian",
                                                                                                               "Forcetype_Firearm_Exhibited",
                                                                                                               "Forcetype_Hands",
                                                                                                               "Forcetype_Force_Escort_Tech",
                                                                                                               "Forcetype_Head_Strike_No_Wep",
                                                                                                               "Forcetype_Hands_with_injury",
                                                                                                               "Forcetype_Force_Take_Down",
                                                                                                               "Forcetype_CEW_Exhibited_Laser",
                                                                                                               "Forcetype_Canine_Contact",
                                                                                                               "Forcetype_Baton_PR_24_NonStrk",
                                                                                                               "Forcetype_VehPursuits_wInjury",
                                                                                                               "Forcetype_Takedown_winjury",
                                                                                                               "Forcetype_Canine_NoBite",
                                                                                                               "Forcetype_Other",
                                                                                                               "Forcetype_Handcuffed_Subject",
                                                                                                               "Forcetype_Baton_PR_24_Strike",
                                                                                                               "Forcetype_CEW_Deployment",
                                                                                                               "Forcetype_Force_Defense_Tech",
                                                                                                               "Forcetype_Firearm_Discharged",
                                                                                                               "Forcetype_Baton_PR_24_Miss",
                                                                                                               "Forcetype_Force_Neck_Holds",
                                                                                                               "Use_of_Force_Level",
                                                                                                               "Subject_Injured",
                                                                                                               "Officer_Injured",
                                                                                                               "Subject_Hospitalized",
                                                                                                               "Subject_Influence_None_Detected",
                                                                                                               "Subject_Influence_Mentally_Unstable",
                                                                                                               "Subject_Influence_Unknown",
                                                                                                               "Subject_Influence_Alcohol"))])


 # # Another slimmed down model, based on xboost, doesn't seem to be effective for any grade feature weight
  numeric_matrix_precogData <- as.matrix(numeric_factor_trainData[, which(names(numeric_factor_trainData) %in% c( "Officer_Gender_Male",
                                                                                               "Officer_Gender_MaleandFemale",
                                                                                               "Officer_Gender_Majority_Male",
                                                                                               "Officer_Gender_Female",
                                                                                               "Officer_Gender_Majority_Female",
                                                                                               "Subject_Race_White",
                                                                                               "Subject_Race_BlackandHispanic",
                                                                                               "Subject_Race_BlackandIndian",
                                                                                               "Subject_Race_Black",
                                                                                               "Subject_Race_WhiteandBlack",
                                                                                               "Subject_Race_BlackandRace_Unknown",
                                                                                               "Subject_Race_Race_Unknown",
                                                                                               "Subject_Race_Mixed",
                                                                                               "Subject_Race_WhiteandHispanic",
                                                                                               "Subject_Race_Hispanic",
                                                                                               "Subject_Race_Majority_Black",
                                                                                               "Subject_Race_Majority_Hispanic",
                                                                                               "Subject_Race_Asian",
                                                                                               "Subject_Race_BlackandIndiane",
                                                                                               "Subject_Race_Majority_White",
                                                                                               "Subject_Race_Indian",
                                                                                               "Forcetype_Firearm_Exhibited",
                                                                                               "Forcetype_Hands",
                                                                                               "Forcetype_Force_Escort_Tech",
                                                                                               "Forcetype_Head_Strike_No_Wep",
                                                                                               "Forcetype_Hands_with_injury",
                                                                                               "Forcetype_Force_Take_Down",
                                                                                               "Forcetype_CEW_Exhibited_Laser",
                                                                                               "Forcetype_Canine_Contact",
                                                                                               "Forcetype_Baton_PR_24_NonStrk",
                                                                                               "Forcetype_VehPursuits_wInjury",
                                                                                               "Forcetype_Takedown_winjury",
                                                                                               "Forcetype_Canine_NoBite",
                                                                                               "Forcetype_Other",
                                                                                               "Forcetype_Handcuffed_Subject",
                                                                                               "Forcetype_Baton_PR_24_Strike",
                                                                                               "Forcetype_CEW_Deployment",
                                                                                               "Forcetype_Force_Defense_Tech",
                                                                                               "Forcetype_Firearm_Discharged",
                                                                                               "Forcetype_Baton_PR_24_Miss",
                                                                                               "Forcetype_Force_Neck_Holds",
                                                                                               "Subject_Influence_None_Detected",
                                                                                               "Subject_Influence_Mentally_Unstable",
                                                                                               "Subject_Influence_Unknown",
                                                                                               "Subject_Influence_Alcohol"))])
  

 # Check the class of the new matrix
 class(numeric_matrix_trainData)  # Should be "matrix"
 any(is.infinite(numeric_matrix_trainData))  # Should be FALSE

 
 # Check for NaN or Inf values
 any(is.na(numeric_factor_trainData))  # Should be FALSE
 any(is.na(trainData$weighted_sum))    # Should be FALSE
 any(is.infinite(numeric_matrix_trainData))  # Should be FALSE
 any(is.infinite(trainData$weighted_sum))    # Should be FALSE
 
 # Convert input features to TensorFlow Tensor
 numberofcoll <- ncol(numeric_matrix_trainData)
 numberofcoll
 x_train <- as.matrix(numeric_matrix_trainData)
 x_train <- tf$convert_to_tensor(x_train, dtype=tf$float32)
 
 # Convert target variable to TensorFlow Tensor
 y_train <- as.vector(trainData$weighted_sum)
 y_train <- tf$convert_to_tensor(y_train, dtype=tf$float32)
 
 # Create a sequential model
 model <- tf$keras$Sequential()
 
# Add layers to the model with specifying input_shape
 
# Input layer: Dense layer with 64 units and ReLU activation function. 
# The input shape is (numberofcoll) defined above, which matches the number of features in the input data.
# Hidden layer: Dense layer with 32 units and ReLU activation function.
# Output layer: Dense layer with 1 unit and linear activation function.
# The output shape will be (batch_size, 1).
 
 
 model <- keras_model_sequential() %>%
   layer_dense(units = 64, activation = "relu", input_shape = c(numberofcoll)) %>%
   layer_dense(units = 32, activation = "relu") %>%
   layer_dense(units = 1, activation = "linear")
 

 # Compile the model
 model$compile(
   loss = "mean_squared_error",
   optimizer = tf$keras$optimizers$Adam(learning_rate = 0.001),
   metrics = c("mean_absolute_error", "mean_squared_error")
 )
 
 # Print the model summary
 model$summary()
 
 # Fit the model
 history <- model$fit(
   x = x_train,
   y = y_train,
   epochs = 100L,
   batch_size = 1824L,
   validation_split = 0.2,
   verbose = 0
 )

 
 # Plot training history
 # The training and validation loss show a similar decrease and remain
 # close to each other, it's generally a positive sign. This suggests that 
 # our model is not over fitting to the training data 

 plot(history$history$loss, type = "l", col = "blue", xlab = "Epoch", ylab = "Loss",
      main = "Training Loss Over Epochs", ylim = c(0, max(history$history$loss)))
 lines(history$history$val_loss, col = "red")
 legend("topright", legend = c("Training Loss", "Validation Loss"), col = c("blue", "red"), lty = 1)
 
 
 # Plot mean absolute error
 # No degrade of Validation showing good potential
 plot(history$history$mean_absolute_error, type = "l", col = "blue", xlab = "Epoch", ylab = "Mean Absolute Error",
      main = "Mean Absolute Error Over Epochs", ylim = c(0, max(history$history$mean_absolute_error)))
 lines(history$history$val_mean_absolute_error, col = "red")
 legend("topright", legend = c("Training MAE", "Validation MAE"), col = c("blue", "red"), lty = 1)
 
 # Plot mean squared error
 # Same here no degrade of Validation 
 plot(history$history$mean_squared_error, type = "l", col = "blue", xlab = "Epoch", ylab = "Mean Squared Error",
      main = "Mean Squared Error Over Epochs", ylim = c(0, max(history$history$mean_squared_error)))
 lines(history$history$val_mean_squared_error, col = "red")
 legend("topright", legend = c("Training MSE", "Validation MSE"), col = c("blue", "red"), lty = 1)
 
 # Make predictions using the trained model
 predicted_train <- model$predict(x_train)

 predicted_train <- as.vector(predicted_train)
 
 # Convert y_train and predicted_train to R vectors
 y_train <- as.vector(y_train)

# Pulling in outliers like we did to actual data
predicted_train <- ifelse(predicted_train > 1, 1, predicted_train)
predicted_train <- ifelse(predicted_train < 0, 0, predicted_train)

hist(predicted_train, col = "green", main = "Tensor Full Model Perdicted \n Weighted Sums", xlab = "Values")
hist(y_train, col = "orange", main = "Actual Training Weighted Sum", xlab = "Values")
 # Create a data frame with actual and predicted values
 train_df <- data.frame(Actual = y_train, Predicted = predicted_train)
 
 # Create a scatter plot
 ggplot(train_df, aes(x = Actual, y = Predicted)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
   labs(x = "Actual Weighted Sum", y = "Predicted Weighted Sum", title = "Scatter Plot of Predicted vs Actual") +
   theme_minimal()
 
 # Calculate Mean Absolute Error (MAE)
 mae <- mean(abs(predicted_train - y_train))
 
 # Calculate Mean Squared Error (MSE)
 mse <- mean((predicted_train - y_train)^2)
 
 # Calculate Root Mean Squared Error (RMSE)
 rmse <- sqrt(mse)
 
 # Calculate R-squared
 ss_total <- sum((y_train - mean(y_train))^2)
 ss_residual <- sum((y_train - predicted_train)^2)
 r_squared <- 1 - (ss_residual / ss_total)
 
 # Print the calculated metrics
 cat("MAE:", mae, "\n")
 cat("MSE:", mse, "\n")
 cat("RMSE:", rmse, "\n")
 cat("R-squared:", r_squared, "\n")
 
 # Create a new row for Tensor results
 rf_row <- data.frame(
   Model = "Tensorflow",
   MSE = mse,
   RMSE = rmse,
   MAE = mae,
   R_squared = round(r_squared, 5)  # Round R-squared to 5 decimal places
 )
 
 # Append the Random Forest results to the existing evaluation table
 evaluation_table <- rbind(evaluation_table, rf_row)
 
 # Print the updated evaluation table
 print(evaluation_table)
 
################ Doing the same for the precog data
 # Convert input features to TensorFlow Tensor
 numberofcoll <- ncol(numeric_matrix_precogData)
 numberofcoll
 x_train <- as.matrix(numeric_matrix_precogData)
 x_train <- tf$convert_to_tensor(x_train, dtype=tf$float32)
 
 # Convert target variable to TensorFlow Tensor
 y_train <- as.vector(trainData$weighted_sum)
 y_train <- tf$convert_to_tensor(y_train, dtype=tf$float32)
 
 # Create a sequential model
 model <- tf$keras$Sequential()
 
 # Add layers to the model with specifying input_shape
 
 model <- keras_model_sequential() %>%
   layer_dense(units = 64, activation = "relu", input_shape = c(numberofcoll)) %>%
   layer_dense(units = 32, activation = "relu") %>%
   layer_dense(units = 1, activation = "linear")
 
 
 # Compile the model
 model$compile(
   loss = "mean_squared_error",
   optimizer = tf$keras$optimizers$Adam(learning_rate = 0.001),
   metrics = c("mean_absolute_error", "mean_squared_error")
 )
 
 # Print the model summary
 model$summary()
 
 # Fit the model
 history <- model$fit(
   x = x_train,
   y = y_train,
   epochs = 100L,
   batch_size = 1824L,
   validation_split = 0.2,
   verbose = 0
 )
 
 # Plot training history
# Less Over Fitting with this model
 plot(history$history$loss, type = "l", col = "blue", xlab = "Epoch", ylab = "Loss",
      main = "Training Loss Over Epochs", ylim = c(0, max(history$history$loss)))
 lines(history$history$val_loss, col = "red")
 legend("topright", legend = c("Training Loss", "Validation Loss"), col = c("blue", "red"), lty = 1)
 
 # Plot mean absolute error
 # No degrade of Validation showing good potential
 plot(history$history$mean_absolute_error, type = "l", col = "blue", xlab = "Epoch", ylab = "Mean Absolute Error",
      main = "Mean Absolute Error Over Epochs", ylim = c(0, max(history$history$mean_absolute_error)))
 lines(history$history$val_mean_absolute_error, col = "red")
 legend("topright", legend = c("Training MAE", "Validation MAE"), col = c("blue", "red"), lty = 1)
 
 # Plot mean squared error
 # Same here no degrade of Validation 
 plot(history$history$mean_squared_error, type = "l", col = "blue", xlab = "Epoch", ylab = "Mean Squared Error",
      main = "Mean Squared Error Over Epochs", ylim = c(0, max(history$history$mean_squared_error)))
 lines(history$history$val_mean_squared_error, col = "red")
 legend("topright", legend = c("Training MSE", "Validation MSE"), col = c("blue", "red"), lty = 1)
 
 
 # Make predictions using the trained model
 predicted_train <- model$predict(x_train)
 predicted_train <- as.vector(predicted_train)

 # Convert y_train and predicted_train to R vectors
 y_train <- as.vector(y_train)
 # mash in outliers
 predicted_train <- ifelse(predicted_train > 1, 1, predicted_train)
 predicted_train <- ifelse(predicted_train < 0, 0, predicted_train)
 
 hist(predicted_train, col = "green", main = "Tensor Precog Model Perdicted \n Weighted Sums", xlab = "Values")
 hist(y_train, col = "orange", main = "Actual Training Weighted Sum", xlab = "Values")
 # Create a data frame with actual and predicted values
 train_df <- data.frame(Actual = y_train, Predicted = predicted_train)
 
 # Create scatter plot for training data
 ggplot(train_df, aes(x = Actual, y = Predicted)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
   labs(x = "Actual Weighted Sum", y = "Predicted Weighted Sum", title = "Scatter Plot of Predicted vs Actual") +
   theme_minimal()
 
 # Calculate Mean Absolute Error (MAE)
 mae <- mean(abs(predicted_train - y_train))
 
 # Calculate Mean Squared Error (MSE)
 mse <- mean((predicted_train - y_train)^2)
 
 # Calculate Root Mean Squared Error (RMSE)
 rmse <- sqrt(mse)
 
 # Calculate R-squared
 ss_total <- sum((y_train - mean(y_train))^2)
 ss_residual <- sum((y_train - predicted_train)^2)
 r_squared <- 1 - (ss_residual / ss_total)
 
 # Print the calculated metrics
 cat("MAE:", mae, "\n")
 cat("MSE:", mse, "\n")
 cat("RMSE:", rmse, "\n")
 cat("R-squared:", r_squared, "\n")
 
 # Create a new row for Random Forest results
 rf_row <- data.frame(
   Model = "Tensorflow Precog",
   MSE = mse,
   RMSE = rmse,
   MAE = mae,
   R_squared = round(r_squared, 5)  # Round R-squared to 5 decimal places
 )
 
 # Append the Random Forest results to the existing evaluation table
 evaluation_table <- rbind(evaluation_table, rf_row)
 
 # Print the updated evaluation table
 print(evaluation_table)  
#

# Deep learning models often require large amounts of data to truly leverage 
# their complexity. If your dataset is not very large, linear models might perform comparably.
# So I ran a bootstrap to increase dat size to see if my small data set was an issue.

# Full model
 numeric_matrix_trainData <- as.matrix(numeric_factor_trainData[, which(names(numeric_factor_trainData) %in% 
                                                                          c( "Officer_Gender_Male",
                                                                             "Officer_Gender_MaleandFemale",
                                                                             "Officer_Gender_Majority_Male",
                                                                             "Officer_Gender_Female",
                                                                             "Officer_Gender_Majority_Female",
                                                                             "Subject_Race_White",
                                                                             "Subject_Race_BlackandHispanic",
                                                                             "Subject_Race_BlackandIndian",
                                                                             "Subject_Race_Black",
                                                                             "Subject_Race_WhiteandBlack",
                                                                             "Subject_Race_BlackandRace_Unknown",
                                                                             "Subject_Race_Race_Unknown",
                                                                             "Subject_Race_Mixed",
                                                                             "Subject_Race_WhiteandHispanic",
                                                                             "Subject_Race_Hispanic",
                                                                             "Subject_Race_Majority_Black",
                                                                             "Subject_Race_Majority_Hispanic",
                                                                             "Subject_Race_Asian",
                                                                             "Subject_Race_BlackandIndiane",
                                                                             "Subject_Race_Majority_White",
                                                                             "Subject_Race_Indian",
                                                                             "Forcetype_Firearm_Exhibited",
                                                                             "Forcetype_Hands",
                                                                             "Forcetype_Force_Escort_Tech",
                                                                             "Forcetype_Head_Strike_No_Wep",
                                                                             "Forcetype_Hands_with_injury",
                                                                             "Forcetype_Force_Take_Down",
                                                                             "Forcetype_CEW_Exhibited_Laser",
                                                                             "Forcetype_Canine_Contact",
                                                                             "Forcetype_Baton_PR_24_NonStrk",
                                                                             "Forcetype_VehPursuits_wInjury",
                                                                             "Forcetype_Takedown_winjury",
                                                                             "Forcetype_Canine_NoBite",
                                                                             "Forcetype_Other",
                                                                             "Forcetype_Handcuffed_Subject",
                                                                             "Forcetype_Baton_PR_24_Strike",
                                                                             "Forcetype_CEW_Deployment",
                                                                             "Forcetype_Force_Defense_Tech",
                                                                             "Forcetype_Firearm_Discharged",
                                                                             "Forcetype_Baton_PR_24_Miss",
                                                                             "Forcetype_Force_Neck_Holds",
                                                                             "Use_of_Force_Level",
                                                                             "Subject_Injured",
                                                                             "Officer_Injured",
                                                                             "Subject_Hospitalized",
                                                                             "Subject_Influence_None_Detected",
                                                                             "Subject_Influence_Mentally_Unstable",
                                                                             "Subject_Influence_Unknown",
                                                                             "Subject_Influence_Alcohol",
                                                                             "weighted_sum"))])
 # Precog model                                                                                                               
 numeric_matrix_precogData <- as.matrix(numeric_factor_trainData[, which(names(numeric_factor_trainData) %in% 
                                                                           c( "Officer_Gender_Male",
                                                                             "Officer_Gender_MaleandFemale",
                                                                             "Officer_Gender_Majority_Male",
                                                                             "Officer_Gender_Female",
                                                                             "Officer_Gender_Majority_Female",
                                                                             "Subject_Race_White",
                                                                             "Subject_Race_BlackandHispanic",
                                                                             "Subject_Race_BlackandIndian",
                                                                             "Subject_Race_Black",
                                                                             "Subject_Race_WhiteandBlack",
                                                                             "Subject_Race_BlackandRace_Unknown",
                                                                             "Subject_Race_Race_Unknown",
                                                                             "Subject_Race_Mixed",
                                                                             "Subject_Race_WhiteandHispanic",
                                                                             "Subject_Race_Hispanic",
                                                                             "Subject_Race_Majority_Black",
                                                                             "Subject_Race_Majority_Hispanic",
                                                                             "Subject_Race_Asian",
                                                                             "Subject_Race_BlackandIndiane",
                                                                             "Subject_Race_Majority_White",
                                                                             "Subject_Race_Indian",
                                                                             "Forcetype_Firearm_Exhibited",
                                                                             "Forcetype_Hands",
                                                                             "Forcetype_Force_Escort_Tech",
                                                                             "Forcetype_Head_Strike_No_Wep",
                                                                             "Forcetype_Hands_with_injury",
                                                                             "Forcetype_Force_Take_Down",
                                                                             "Forcetype_CEW_Exhibited_Laser",
                                                                             "Forcetype_Canine_Contact",
                                                                             "Forcetype_Baton_PR_24_NonStrk",
                                                                             "Forcetype_VehPursuits_wInjury",
                                                                             "Forcetype_Takedown_winjury",
                                                                             "Forcetype_Canine_NoBite",
                                                                             "Forcetype_Other",
                                                                             "Forcetype_Handcuffed_Subject",
                                                                             "Forcetype_Baton_PR_24_Strike",
                                                                             "Forcetype_CEW_Deployment",
                                                                             "Forcetype_Force_Defense_Tech",
                                                                             "Forcetype_Firearm_Discharged",
                                                                             "Forcetype_Baton_PR_24_Miss",
                                                                             "Forcetype_Force_Neck_Holds",
                                                                             "Subject_Influence_None_Detected",
                                                                             "Subject_Influence_Mentally_Unstable",
                                                                             "Subject_Influence_Unknown",
                                                                             "Subject_Influence_Alcohol",
                                                                             "weighted_sum"))])
# View(numeric_factor_trainData)
 # Check the class of the new matrix
 class(numeric_matrix_trainData)  # Should be "matrix"
 any(is.infinite(numeric_matrix_trainData))  # Should be FALSE
 
 
 # Check for NaN or Inf values
 any(is.na(numeric_factor_trainData))  # Should be FALSE
 any(is.na(trainData$weighted_sum))    # Should be FALSE
 any(is.infinite(numeric_matrix_trainData))  # Should be FALSE
 any(is.infinite(trainData$weighted_sum))    # Should be FALSE
 
 
 # Split the data into training, validation, and test sets
 set.seed(123)  # For reproducibility
 trainIndex <- createDataPartition(numeric_matrix_trainData[, "weighted_sum"], p = 0.7, list = FALSE)
 train_data <- numeric_matrix_trainData[trainIndex, ]
 temp_data <- numeric_matrix_trainData[-trainIndex, ]
 valIndex <- createDataPartition(temp_data[, "weighted_sum"], p = 0.5, list = FALSE)
 validation_data <- temp_data[valIndex, ]
 test_data <- temp_data[-valIndex, ]
 
 # Define the size multiplier this variable will crash memory: stop seeing improvements after 6
 size_multiplier <- 6
 
 # Create a larger dataset by repeating the original dataset
 larger_dataset <- train_data
 for (i in 1:(size_multiplier - 1)) {
   larger_dataset <- rbind(larger_dataset, train_data)
 }
 ncol(larger_dataset)
 
 # Define the model architecture
 model <- keras_model_sequential() %>%
   layer_dense(units = 64, activation = "relu", input_shape = c(ncol(larger_dataset) - 1)) %>%
   layer_dense(units = 32, activation = "relu") %>%
   layer_dense(units = 1, activation = "linear")
 
 # Compile the model
 model$compile(
   loss = "mean_squared_error",
   optimizer = tf$keras$optimizers$Adam(learning_rate = 0.001),
   metrics = c("mean_absolute_error", "mean_squared_error")
 )
 
 # Exclude columns by using %in% and indexing [, ]
 columns_to_exclude <- c("weighted_sum")
 x_train_boot <- larger_dataset[, !(colnames(larger_dataset) %in% columns_to_exclude)]
 y_train_boot <- as.vector(larger_dataset[, "weighted_sum"])
 
 # Convert input features to TensorFlow Tensor
 x_train_boot <- tf$convert_to_tensor(x_train_boot, dtype = tf$float32)
 
 # Convert target variable to TensorFlow Tensor
 y_train_boot <- tf$convert_to_tensor(y_train_boot, dtype = tf$float32)

 # Fit the model using the training set
 history <- model$fit(
   x = x_train_boot,
   y = y_train_boot,
   epochs = 100L,
   batch_size = 1824L,
   validation_split = 0.2,
   verbose = 0,
 )
 

# Plot training history

plot(history$history$loss, type = "l", col = "blue", xlab = "Epoch", ylab = "Loss",
     main = "Training Loss Over Epochs", ylim = c(0, max(history$history$loss)))
lines(history$history$val_loss, col = "red")
legend("topright", legend = c("Training Loss", "Validation Loss"), col = c("blue", "red"), lty = 1)


# Plot mean absolute error
# No degrade of Validation showing good potential
plot(history$history$mean_absolute_error, type = "l", col = "blue", xlab = "Epoch", ylab = "Mean Absolute Error",
     main = "Mean Absolute Error Over Epochs", ylim = c(0, max(history$history$mean_absolute_error)))
lines(history$history$val_mean_absolute_error, col = "red")
legend("topright", legend = c("Training MAE", "Validation MAE"), col = c("blue", "red"), lty = 1)

# Plot mean squared error
# Same here no degrade of Validation 
plot(history$history$mean_squared_error, type = "l", col = "blue", xlab = "Epoch", ylab = "Mean Squared Error",
     main = "Mean Squared Error Over Epochs", ylim = c(0, max(history$history$mean_squared_error)))
lines(history$history$val_mean_squared_error, col = "red")
legend("topright", legend = c("Training MSE", "Validation MSE"), col = c("blue", "red"), lty = 1)


# Make predictions using the trained model
predicted_values <- model$predict(x_train_boot)
# Create a scatter plot
plot(y_train_boot, predicted_values, 
     xlab = "Actual Values", 
     ylab = "Predicted Values",
     main = "Actual vs. Predicted Values")

# Add a diagonal line for reference
abline(0, 1, col = "red")

# Calculate evaluation metrics
mae <- mean(abs(predicted_values- y_train_boot))
mse <- mean((predicted_values - y_train_boot )^2)
rmse <- sqrt(mse)

# Convert TensorFlow tensor to numeric vector
y_train_boot_num <- as.numeric(y_train_boot)


# Calculate R-squared
ss_total <- sum((y_train_boot_num - mean(y_train_boot_num))^2)
ss_residual <- sum((y_train_boot_num - predicted_values)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Print evaluation metrics
cat("MAE:", sprintf("%.4f", mae), "\n")
cat("MSE:", sprintf("%.4f", mse), "\n")
cat("RMSE:", sprintf("%.4f", rmse), "\n")
cat("R2:", sprintf("%.4f", r_squared), "\n")
# Extract numeric values from TensorFlow tensors
mse_numeric <- as.numeric(mse)
rmse_numeric <- as.numeric(rmse)
mae_numeric <- as.numeric(mae)
r_squared_numeric <- as.numeric(r_squared)

# Create a new row for Bootstrap Forest results
rf_row <- data.frame(
  Model = "Bootstrap Tensor Full",
  MSE = mse_numeric,
  RMSE = rmse_numeric,
  MAE = mae_numeric,
  R_squared = round(r_squared_numeric, 5)  # Round R-squared to 5 decimal places
)

# Append the Random Forest results to the existing evaluation table
evaluation_table <- rbind(evaluation_table, rf_row)

# Print the updated evaluation table
print(evaluation_table)


# Bootstrap enlarging Tensorflow Precog model
# Split the data into training, validation, and test sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(numeric_matrix_precogData[, "weighted_sum"], p = 0.7, list = FALSE)
train_data <- numeric_matrix_precogData[trainIndex, ]
temp_data <- numeric_matrix_precogData[-trainIndex, ]
valIndex <- createDataPartition(temp_data[, "weighted_sum"], p = 0.5, list = FALSE)
validation_data <- temp_data[valIndex, ]
test_data <- temp_data[-valIndex, ]

# Define the size multiplier this variable will crash memory: stop seeing improvments after 6
size_multiplier <- 6

# Create a larger dataset by repeating the original dataset
larger_dataset <- train_data
for (i in 1:(size_multiplier - 1)) {
  larger_dataset <- rbind(larger_dataset, train_data)
}
ncol(larger_dataset)

# Define the model architecture
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(ncol(larger_dataset) - 1)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")

# Compile the model
model$compile(
  loss = "mean_squared_error",
  optimizer = tf$keras$optimizers$Adam(learning_rate = 0.001),
  metrics = c("mean_absolute_error", "mean_squared_error")
)

# Exclude columns by using %in% and indexing [, ]
columns_to_exclude <- c("weighted_sum")
x_train_boot <- larger_dataset[, !(colnames(larger_dataset) %in% columns_to_exclude)]
y_train_boot <- as.vector(larger_dataset[, "weighted_sum"])

# Convert input features to TensorFlow Tensor
x_train_boot <- tf$convert_to_tensor(x_train_boot, dtype = tf$float32)


# Convert target variable to TensorFlow Tensor
y_train_boot <- tf$convert_to_tensor(y_train_boot, dtype = tf$float32)


# Fit the model using the training set
history <- model$fit(
  x = x_train_boot,
  y = y_train_boot,
  epochs = 100L,
  batch_size = 1824L,
  validation_split = 0.2,
  verbose = 0,
)


# Plot training history

plot(history$history$loss, type = "l", col = "blue", xlab = "Epoch", ylab = "Loss",
     main = "Training Loss Over Epochs", ylim = c(0, max(history$history$loss)))
lines(history$history$val_loss, col = "red")
legend("topright", legend = c("Training Loss", "Validation Loss"), col = c("blue", "red"), lty = 1)


# Plot mean absolute error
# No degrade of Validation showing good potential
plot(history$history$mean_absolute_error, type = "l", col = "blue", xlab = "Epoch", ylab = "Mean Absolute Error",
     main = "Mean Absolute Error Over Epochs", ylim = c(0, max(history$history$mean_absolute_error)))
lines(history$history$val_mean_absolute_error, col = "red")
legend("topright", legend = c("Training MAE", "Validation MAE"), col = c("blue", "red"), lty = 1)

# Plot mean squared error
# Same here no degrade of Validation 
plot(history$history$mean_squared_error, type = "l", col = "blue", xlab = "Epoch", ylab = "Mean Squared Error",
     main = "Mean Squared Error Over Epochs", ylim = c(0, max(history$history$mean_squared_error)))
lines(history$history$val_mean_squared_error, col = "red")
legend("topright", legend = c("Training MSE", "Validation MSE"), col = c("blue", "red"), lty = 1)


# Make predictions using the trained model
predicted_values <- model$predict(x_train_boot)
# Create a scatter plot
plot(y_train_boot, predicted_values, 
     xlab = "Actual Values", 
     ylab = "Predicted Values",
     main = "Actual vs. Predicted Values")

# Add a diagonal line for reference
abline(0, 1, col = "red")

# Calculate evaluation metrics
mae <- mean(abs(predicted_values- y_train_boot))
mse <- mean((predicted_values - y_train_boot )^2)
rmse <- sqrt(mse)

# Convert TensorFlow tensor to numeric vector
y_train_boot_num <- as.numeric(y_train_boot)

# Calculate R-squared
ss_total <- sum((y_train_boot_num - mean(y_train_boot_num))^2)
ss_residual <- sum((y_train_boot_num - predicted_values)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Print evaluation metrics
cat("MAE:", sprintf("%.4f", mae), "\n")
cat("MSE:", sprintf("%.4f", mse), "\n")
cat("RMSE:", sprintf("%.4f", rmse), "\n")
cat("R2:", sprintf("%.4f", r_squared), "\n")
# Extract numeric values from TensorFlow tensors
mse_numeric <- as.numeric(mse)
rmse_numeric <- as.numeric(rmse)
mae_numeric <- as.numeric(mae)
r_squared_numeric <- as.numeric(r_squared)

# Create a new row for Bootstrap Forest results
rf_row <- data.frame(
  Model = "Bootstrap Tensor Precog",
  MSE = mse_numeric,
  RMSE = rmse_numeric,
  MAE = mae_numeric,
  R_squared = round(r_squared_numeric, 5)  # Round R-squared to 5 decimal places
)

# Append the Random Forest results to the existing evaluation table
evaluation_table <- rbind(evaluation_table, rf_row)

# Print the updated evaluation table
print(evaluation_table)

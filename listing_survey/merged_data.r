library(dplyr)
library(tidyverse)
library(reshape2)
library(splitstackshape)

str_data = read.csv("C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-5-2023/listing_survey_5.csv")
hh_data = read.csv("C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-5-2023/listing_survey_5-repeat_households.csv")
cp_data = read.csv("C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-5-2023/listing_survey_5-repeat_couple.csv") 
ch_data = read.csv("C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-5-2023/listing_survey_5-repeat_children.csv")


colnames(ch_data)
dim(structure_data)
dim(household_data)
dim(couple_data)
dim(child_data)

# Rename the variables in the Structure Data Frame 
variable_rename_map <- c(
  "date_survey" = "A_01",
  "consent" = "A1",
  "neigh_consent" = "A1a",
  "neigh_under_3" = "A1b",
  "lang_int" = "A2",
  "multiple_lang" = "A3",
  "dwel_head" = "A4",
  "num_hh" = "A5",
  "num_hh_other" = "A5a",
  "num_hh_under_3" = "A6",
  "num_str_under_3" = "A6a",
  "interview_status" = "A20",
  "resch_date" = "A20a"
)
str_data <- str_data %>%
  rename(!!!variable_rename_map)

# Rename the variables in the Household Data Frame 
variable_rename_map <- c(
	"hh_head" = "A7",
	"num_ch_under_3" = "A9",
	"num_cou_under_3" = "A9a"
)
hh_data <- hh_data %>%
  rename(!!!variable_rename_map)

# Rename the variables in the Couple Data Frame 
variable_rename_map <- c("num_ch_cp_under_3" = "A9b")
cp_data <- cp_data %>%
  rename(!!!variable_rename_map)

# Rename the variables in the Children Data Frame 
variable_rename_map <- c(
  "child_name" = "A10",
  "gender" = "A10a",
  "child_father" = "A11",
  "child_mother" = "A12",
  "ph_no" = "A13",
  "repeat_no" = "A14",
  "correct_no" = "A14a",
  "A14b" = "A14b",
  "contact_yes" = "A15",
  "new_no" = "A16",
  "num_belong" = "A17",
  "repeat_no_again" = "A18",
  "no_correct" = "A19"
)
ch_data <- ch_data %>%
  rename(!!!variable_rename_map)




# Load the required libraries
library(reshape2)

# Assuming you have the dataset loaded as long_format_data
colnames(hh_data)
# Convert long_format_data to a wider format based on "PARENT_KEY"
hh_wide_data <- dcast(hh_data, PARENT_KEY ~ ., value.var = c("instructions_1", "hh_no", "HH_ID", "hh_head", "num_ch_under_3", "num_cou_under_3", "KEY"))

# If you want to save the final dataset to a CSV file
write.csv(wide_format_data, "path/to/wide_format_data.csv", row.names = FALSE)

wide_hh_data <- dcast(hh_data, PARENT_KEY ~ variable, value.var = "value")
?dcast

write.csv(wide_hh_data, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/experiments/wide_hh_data.csv", row.names = FALSE)


wide_hh_data <- dcast(hh_data, PARENT_KEY ~ ., value.var = "instructions_1")










































































# Left join str_data with hh_data based on the common key "KEY" and "parent_key"
str_hh <- left_join(str_data, hh_data, by = c("KEY" = "PARENT_KEY"))

# Melt the hh_data columns based on the parent key
str_hh_melted <- str_hh %>%
  pivot_longer(cols = starts_with("instructions"), names_to = "instruction_type", values_to = "instruction_value")

dim(str_hh)
dim(str_hh_melted)
# Now, str_hh_melted contains str_data with melted hh_data columns based on the parent key

# If there are duplicated rows in str_data due to the left join, you can use distinct to keep only unique rows
str_hh_unique <- distinct(str_hh_melted)

# If you want to save the final dataset to a CSV file
write.csv(str_hh_unique, "path/to/str_hh.csv", row.names = FALSE)


# Combining the structure data frame with the hh dataframe
str_hh = left_join( 
    str_data, hh_data, by=c('KEY'='PARENT_KEY'))


# Combining the structure data frame with the hh dataframe and with the couple dataframe
dim(str_data)
dim(hh_data)
dim(str_hh)

dim(cp_data)
dim(str_hh_cp)

str_hh_cp = left_join( 
    str_data, hh_data, by=c('KEY'='PARENT_KEY')) %>%
    left_join(
    hh_data, cp_data, by=c('KEY'='PARENT_KEY'))

write.csv(new_df, "C:/Users/basicuser/Desktop/HH survey Data/new_df.csv")



head(child_data)
?melt

new_child_table = dcast(getanID(child_data_1, 'PARENT_KEY'), PARENT_KEY~.id, value.var=c("A10", "A11"))
?dcast
new_child_table = reshape(child_data_2, idvar='PARENT_KEY', timevar='child_no', direction='wide')
new_child_table = reshape(child_data_2, idvar='KEY', timevar='child_no', direction='wide')

write.csv(new_child_table,"C:/Users/basicuser/Desktop/HH survey Data/New folder/wide_format_1.csv")



new_couple_table = reshape(couple_data, idvar='PARENT_KEY', timevar='couple_no', direction='wide')
write.csv(new_couple_table ,"C:/Users/basicuser/Desktop/HH survey Data/New folder/wide_coulple_format_1.csv")

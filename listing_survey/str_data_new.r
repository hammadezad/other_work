library(dplyr)
library(tidyverse)
library(reshape2)
library(splitstackshape)

setwd("C:/Users/basicuser/Desktop/surveys/Listing Survey/Data")
structure_data = read.csv("listing_survey_9.csv")
household_data = read.csv("listing_survey_9-repeat_households.csv")
couple_data = read.csv("listing_survey_9-repeat_couple.csv") 
child_data = read.csv("listing_survey_9-repeat_children.csv")


cat("No of structures in the dataset are", dim(structure_data)[1])
cat("No of households in the dataset are", dim(household_data)[1])
cat("No of couples in the dataset are", dim(couple_data)[1])
cat("No of children in the dataset are", dim(child_data)[1])



#cat("No of structures in the dataset are", dim(str_data)[1])
#cat("No of households in the dataset are", dim(hh_data)[1])
#cat("No of couples in the dataset are", dim(cp_data)[1])
#cat("No of children in the dataset are", dim(ch_data)[1])



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
structure_data <- structure_data %>%
  rename(!!!variable_rename_map)

colnames(structure_data)



str_data <- structure_data[, c("today",
    "structure_id",
    "id_number" ,
    "residence_binary",
    "outcome_visit",
    "lang_int", 
    "dwel_head", 
    "num_hh_under_3", 
    "num_str_under_3", 
    "KEY")]
#colnames(structure_data)
#colnames(str_data)

languages <- c(
  "English",
  "Urdu",
  "Punjabi",
  "Sindhi",
  "Pushtu",
  "Balochi",
  "Kashmiri",
  "Sheena",
  "Brushiski"
)

str_data$lang_int <- factor(str_data$lang_int, levels = 1:9, labels = languages)





# Rename the variables in the Household Data Frame 



colnames(household_data)
variable_rename_map <- c(
	"hh_head" = "A7",
	"num_ch_under_3" = "A9",
	"num_cou_under_3" = "A9a"
)
household_data <- household_data %>%
  rename(!!!variable_rename_map)

hh_data <- household_data[,c( "hh_no","hh_head" ,"num_cou_under_3" , "PARENT_KEY", "KEY")]


dim(hh_data)

hh_data <- hh_data %>%
  group_by(PARENT_KEY) %>%
  mutate(hh_num = row_number()) %>%
  ungroup() %>%
  group_by(PARENT_KEY) %>%
  mutate(hh_nos = max(hh_num))

print(hh_data, n = 50)




# Rename the variables in the Couple Data Frame 
variable_rename_map <- c(
  "num_ch_cp_under_3" = "A9b",
  "child_father" = "A11",
  "child_mother" = "A12",
  "ph_no" = "A13",
  "repeat_no" = "A14",
  "correct_no" = "A14a",
  "contact_yes" = "A15",
  "new_no" = "A16",
  "num_belong" = "A17",
  "repeat_no_again" = "A18"
)
cp_data <- cp_data %>%
  rename(!!!variable_rename_map)

cp_data$str_key = substr(cp_data$KEY, 1, 41)

colnames(ch_data)
colnames(str_data)
colnames(cp_data)
colnames(hh_data)

# Rename the variables in the Children Data Frame 
variable_rename_map <- c(
  "child_name" = "A10",
  "gender" = "A10a",
  "no_correct" = "A19"
)
ch_data <- ch_data %>%
  rename(!!!variable_rename_map)

ch_data$str_key = substr(ch_data$KEY, 1, 41)

write.csv(str_data, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/9-5-2023/simple_data/str_data.csv")
write.csv(hh_data, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/9-5-2023/simple_data/hh_data.csv")
write.csv(cp_data, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/9-5-2023/simple_data/cp_data.csv")
write.csv(ch_data, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/9-5-2023/simple_data/ch_data.csv")


################################################################################
#############################  Working on the hh data ##########################
################################################################################
library(dplyr)
library(tidyr)

# Assuming you've already grouped your data and added the 'index' column

# Add a unique identifier for each row
grouped_hh_data <- grouped_hh_data %>%
  group_by(index) %>%
  mutate(row_id = row_number())

# Pivot the data from long to wide format with numeric suffixes
hh_wide_data_1 <- grouped_hh_data %>%
  pivot_wider(
    id_cols = c("PARENT_KEY", "row_id"),
    names_from = index,
    values_from = c(
      instructions_1,
      hh_no,
      new_hh_no,
      HH_ID,
      hh_head,
      num_ch_under_3,
      num_cou_under_3,
      repeat_couple_count
    ),
    names_sep = "_"
  )

# Remove the row_id column (optional)
hh_wide_data_1 <- hh_wide_data_1 %>%
  select(-row_id)

# Reorder columns
hh_wide_data_1 <- hh_wide_data_1 %>%
  select(
    order(
      as.numeric(gsub(".*_", "", colnames(.))),
      everything()
    )
  )

# Write the wide data to a CSV file
write.csv(hh_wide_data_1, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/9-5-2023/wide_data/hh_wide_data_1.csv")



################################################################################
#############################  Working on the couple data ######################
################################################################################

grouped_cp_data <- cp_data %>%
  group_by(PARENT_KEY) %>%
  mutate(index = row_number())

# Pivot the data from long to wide format with numeric suffixes
cp_wide_data <- grouped_cp_data %>%
  pivot_wider(
    names_from = index,
    values_from = c(instructions_2,couple_no,couple_id,num_ch_cp_under_3,repeat_children_count,PARENT_KEY,KEY),
    names_sep = "_"
  )

cp_wide_data <- cp_wide_data %>%
  select(
    order(
      as.numeric(gsub(".*_", "", colnames(.))),
      everything()
    )
  )
# Remove grouping
cp_wide_data <- cp_wide_data %>%
  ungroup()


# Display the wide data
print(cp_wide_data)

write.csv(cp_wide_data, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/structure_data/cp_wide_data.csv")


#hh_cp <- left_join(hh_data, cp_wide_data, by = c("KEY" = "PARENT_KEY"))


#write.csv(hh_cp, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/derived_data/hh_cp.csv")



dim(cp_data)
dim(str_data)
dim(hh_data)



################################################################################
#############################  Working on the children data ####################
################################################################################

colnames(ch_data)
grouped_ch_data <- ch_data %>%
  group_by(str_key) %>%
  mutate(index = row_number())

# Pivot the data from long to wide format with numeric suffixes
ch_wide_data <- grouped_ch_data %>%
  pivot_wider(
    names_from = index,
    values_from = c(instructions_2,child_no,child_id, child_name,gender,no_correct,PARENT_KEY,KEY),
    names_sep = "_"
  )

ch_wide_data <- ch_wide_data %>%
  select(
    order(
      as.numeric(gsub(".*_", "", colnames(.))),
      everything()
    )
  )
# Remove grouping
ch_wide_data <- ch_wide_data %>%
  ungroup()

# Display the wide data
print(ch_wide_data)

write.csv(ch_wide_data, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/structure_data/ch_wide_data.csv")

#cp_ch <- left_join(cp_data, ch_wide_data, by = c("KEY" = "PARENT_KEY"))

#write.csv(cp_ch, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/derived_data/cp_ch.csv")



dim(str_data)
dim(cp_wide_data)
dim(ch_wide_data)
dim(hh_wide_data)

str_hh_cp_ch <- str_data %>%
  left_join(hh_wide_data, by = c('KEY' = 'str_key')) %>%
  left_join(cp_wide_data, by = c('KEY' = 'str_key')) %>%
  left_join(ch_wide_data, by = c('KEY' = 'str_key'))

str_hh_cp_ch = left_join( 
    str_data, hh_wide_data, by=c('KEY'='str_key')) %>%
    left_join(
    hh_wide_data, cp_wide_data, by=c('KEY'='str_key')) %>%
    left_join(
    cp_wide_data, ch_wide_data, by=c('KEY'='str_key'))


write.csv(str_hh_cp_ch, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/structure_data/structure_data.csv")
















































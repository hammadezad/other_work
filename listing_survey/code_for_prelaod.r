library(dplyr)
library(tidyverse)
library(reshape2)
library(splitstackshape)

str_data = read.csv("C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/listing_survey_5.csv")
hh_data = read.csv("C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/listing_survey_5-repeat_households.csv")
cp_data = read.csv("C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/listing_survey_5-repeat_couple.csv") 
ch_data = read.csv("C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/listing_survey_5-repeat_children.csv")




cat("No of structures in the dataset are", dim(str_data)[1])
cat("No of households in the dataset are", dim(hh_data)[1])
cat("No of couples in the dataset are", dim(cp_data)[1])
cat("No of children in the dataset are", dim(ch_data)[1])

str_data$str_key = str_data$PARENT_KEY

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

# Display the updated data frame
print(str_data$lang_int)

# Rename the variables in the Household Data Frame 
variable_rename_map <- c(
	"hh_head" = "A7",
	"num_ch_under_3" = "A9",
	"num_cou_under_3" = "A9a" 
)
hh_data <- hh_data %>%
  rename(!!!variable_rename_map)

hh_data$str_key = substr(hh_data$KEY, 1, 41)




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
    "repeat_no_again" = "A18",
    "no_correct" = "A19"
)
cp_data <- cp_data %>%
  rename(!!!variable_rename_map)

cp_data$str_key = substr(cp_data$KEY, 1, 41)


# Rename the variables in the Children Data Frame 
variable_rename_map <- c(
  "child_name" = "A10",
  "gender" = "A10a"
)
ch_data <- ch_data %>%
  rename(!!!variable_rename_map)

ch_data$str_key = substr(ch_data$KEY, 1, 41)

################################################################################
#############################  Working on the hh data ##########################
################################################################################

grouped_hh_data <- hh_data %>%
  group_by(str_key) %>%
  mutate(index = row_number())

# Pivot the data from long to wide format with numeric suffixes
hh_wide_data <- grouped_hh_data %>%
  pivot_wider(
    names_from = index,
    values_from = c(instructions_1, hh_no, HH_ID, hh_head, num_ch_under_3, num_cou_under_3, repeat_couple_count, PARENT_KEY,KEY),
    names_sep = "_"
  )

hh_wide_data <- hh_wide_data %>%
  select(
    order(
      as.numeric(gsub(".*_", "", colnames(.))),
      everything()
    )
  )
# Remove grouping
hh_wide_data <- hh_wide_data %>%
  ungroup()

# Display the wide data
print(hh_wide_data)

write.csv(hh_wide_data, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/structure_data/hh_wide_data.csv")


#str_hh <- left_join(str_data, hh_wide_data, by = c("KEY" = "PARENT_KEY"))


#write.csv(str_hh, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/derived_data/str_hh.csv")



################################################################################
#############################  Working on the couple data ######################
################################################################################

grouped_cp_data <- cp_data %>%
  group_by(str_key) %>%
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


grouped_ch_data <- ch_data %>%
  group_by(str_key) %>%
  mutate(index = row_number())

# Pivot the data from long to wide format with numeric suffixes
ch_wide_data <- grouped_ch_data %>%
  pivot_wider(
    names_from = index,
    values_from = c(instructions_2,child_no,child_id, child_name,gender,child_father,child_mother,ph_no,repeat_no,correct_no,A14b,contact_yes,new_no,num_belong,repeat_no_again,no_correct,PARENT_KEY,KEY),
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


write.csv(str_hh_cp_ch, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/structure_data/structure_data_2.csv")
















































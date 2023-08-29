library(dplyr)
library(tidyverse)
library(reshape2)
library(splitstackshape)

hh_data = read.csv("C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/listing_survey_5-repeat_households.csv")
cp_data = read.csv("C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/listing_survey_5-repeat_couple.csv") 
ch_data = read.csv("C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/listing_survey_5-repeat_children.csv")





cat("No of households in the dataset are", dim(hh_data)[1])
cat("No of couples in the dataset are", dim(cp_data)[1])
cat("No of children in the dataset are", dim(ch_data)[1])

hh_data$hh_key = hh_data$KEY

# Rename the variables in the Structure Data Frame


# Rename the variables in the Household Data Frame
variable_rename_map <- c(
	"hh_head" = "A7",
	"num_ch_under_3" = "A9",
	"num_cou_under_3" = "A9a"
)
hh_data <- hh_data %>%
  rename(!!!variable_rename_map)


# Rename the variables in the Couple Data Frame 
variable_rename_map <- c(
  "num_ch_cp_under_3" = "A9b"
)
cp_data <- cp_data %>%
  rename(!!!variable_rename_map)

cp_data$hh_key = substr(cp_data$KEY, 1, 41)



# Rename the variables in the Children Data Frame 
variable_rename_map <- c(
  "child_name" = "A10",
  "gender" = "A10a",
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
ch_data <- ch_data %>%
  rename(!!!variable_rename_map)

ch_data$hh_key = substr(ch_data$KEY, 1, 41)



################################################################################
#############################  Working on the couple data ######################
################################################################################

grouped_cp_data <- cp_data %>%
  group_by(hh_key) %>%
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

write.csv(cp_wide_data, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/hh_data/cp_wide_data.csv")


#hh_cp <- left_join(hh_data, cp_wide_data, by = c("KEY" = "PARENT_KEY"))


#write.csv(hh_cp, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/derived_data/hh_cp.csv")



dim(cp_data)
dim(str_data)
dim(hh_data)



################################################################################
#############################  Working on the children data ####################
################################################################################


grouped_ch_data <- ch_data %>%
  group_by(hh_key) %>%
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

str_hh_cp_ch = left_join( 
    str_data, hh_wide_data, by=c('KEY'='str_key')) %>%
    left_join(
    hh_wide_data, cp_wide_data, by=c('KEY'='str_key')) %>%
    left_join(
    cp_wide_data, ch_wide_data, by=c('KEY'='str_key'))


write.csv(str_hh_cp_ch, "C:/Users/basicuser/Desktop/Household Survey Prep/Listing Survey/Data/8-21-2023/structure_data/structure_data.csv")

















































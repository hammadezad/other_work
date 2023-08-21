library(haven)
library("dplyr")
library(lubridate)

#fs= read_sav("C:/Users/basicuser/Desktop/misc_data/fs.sav")
#hl = read_sav("C:/Users/basicuser/Desktop/misc_data/hl.sav")
#mm = read_sav("C:/Users/basicuser/Desktop/misc_data/mm.sav")
#mn = read_sav("C:/Users/basicuser/Desktop/misc_data/mn.sav")
#tn = read_sav("C:/Users/basicuser/Desktop/misc_data/tn.sav")
#wm = read_sav("C:/Users/basicuser/Desktop/misc_data/wm.sav")
#bh = read_sav("C:/Users/basicuser/Desktop/misc_data/bh.sav")

hh = read_sav("C:/Users/basicuser/Desktop/misc_data/hh.sav")
ch = read_sav("C:/Users/basicuser/Desktop/misc_data/ch.sav")

dim(hh)
dim(ch)

# For the household data
new_hh = hh %>% 
    filter(HH7 == 2 | HH7 == 3 | HH7 == 7 | HH7 == 13 | HH7 == 14 | HH7 == 18 | HH7 == 23 | HH7 == 24 | HH7 == 25 | HH7 == 26 | HH7 == 27 | HH7 == 29)

wow_2 = table(new_hh$HH6, new_hh$HH7)
write.csv(wow_2, "C:/Users/basicuser/Desktop/Sample Calculation/comparison/wow_2.csv" )

rural_urban = new_hh %>% group_by(HH6,HH7) %>% summarize(count=n())
print(rural_urban, n = 30)
new_hh %>%
    group_by(HH7) %>%
    summarise(count=n())


wow = table(new_data$HH6.x, new_data$districts)

write.csv(wow, "C:/Users/basicuser/Desktop/Sample Calculation/comparison/wow.csv" )
# Merging the household and the child level data
data = merge(hh, ch, by=c("HH1","HH2"))

districts =data %>%
    group_by(HH7.x) %>%
    summarise(count=n())
print(districts, n = 50)
kambar_data = subset(data, data$HH7.x == 3)
kambar_data %>%
    group_by(HH7.x) %>%
    summarise(count=n())
ke_data = subset(data, data$HH7.x == 25)
ke_data %>%
    group_by(HH7.x) %>%
    summarise(count=n())
dim(data)
colnames(data)
data %>%
    group_by(HH6.x) %>%
    summarise(count=n())

write.csv(ke_data, "C:/Users/basicuser/Desktop/Sample Calculation/comparison/ke_data.csv")

write.csv(kambar_data, "C:/Users/basicuser/Desktop/Sample Calculation/comparison/kambar_data.csv" )

# Filtering the data to only have 12 desired districts
new_data = data %>% 
    filter(HH7.x == 2 | HH7.x == 3 | HH7.x == 7 | HH7.x == 13 | HH7.x == 14 | HH7.x == 18 | HH7.x == 23 | HH7.x == 24 | HH7.x == 25 | HH7.x == 26 | HH7.x == 27 | HH7.x == 29)
#data %>% 
    #filter(HH7.x == 2 | HH7.x == 3 | HH7.x == 7 | HH7.x == 13 | HH7.x == 14 | HH7.x == 18 | HH7.x == 23 | HH7.x == 24 | HH7.x == 25 | HH7.x == 26 | HH7.x == 27 | HH7.x == 29) %>%  
    #group_by(HH7.x) %>%     
    #summarize(count=n())

# Creating a new column with district names 
new_data =new_data %>%
  mutate(districts = as_factor(HH7.x))

new_data %>% 
    group_by(districts) %>%
    summarize(count=n())

new_data %>% 
    group_by(UB2, districts) %>%
    summarize(count=n())

ages = table(new_data$UB2, new_data$districts)
write.csv(ages, "C:/Users/basicuser/Desktop/Sample Calculation/comparison/ages.csv" )




new_data$districts


sub_ch = subset(ch, ch$HH1 >= 193 & ch$HH1 <= 222 | ch$HH1 >= 281 & ch$HH1 <= 341)



count_sub_ch = sub_ch %>% group_by(HH1) %>% summarize(count=n())
write.csv(count_sub_ch, "C:/Users/basicuser/Desktop/Sample Calculation/comparison/sub_ch.csv" )


hh_new = subset(hh, hh$HH1 >= 193 & hh$HH1 <= 222 | hh$HH1 >= 281 & hh$HH1 <= 341)

hh_new %>% group_by(HH1) %>% summarize(count=n())


districts = hh %>% group_by(HH7) %>% summarize(count=n())
districts_clusters = hh %>% group_by(HH1, HH7) %>% summarize(count=n())

print(districts, n = 29)

write.csv(districts, "C:/Users/basicuser/Desktop/Sample Calculation/comparison/clusters.csv")


ch %>% group_by(IM3) %>% summarize(count=n())


ch %>% group_by(IM2) %>% summarize(count=n())


ch %>% group_by(IM5, IM2) %>% summarize(count=n())

# Kambar Shahdadkot Data in Child data
kambar = subset(ch, ch$HH1 >= 193 & ch$HH1 <= 222)
kambar %>% group_by(UF17) %>% summarize("complted surveys"=n()) # How many surveys completed
kambar %>% group_by(UF13) %>% summarize(count=n()) #Language used for the survey
kambar %>% group_by(UB2) %>% summarize(count=n()) #
kambar %>% group_by(BR1) %>% summarize(count=n())
kambar %>% group_by(IM2) %>% summarize(count=n())
kambar %>% group_by(IM3) %>% summarize(count=n())
kambar %>% mutate('date' = make_date(year = IM6BY, month = IM6BM, day = IM6BD))
kambar$bcg_date = make_date(year = kambar$IM6BY, month = kambar$IM6BM, day = kambar$IM6BD)


# Karachi East data

ke = subset(ch, ch$HH1 >= 281 & ch$HH1 <= 341)
ke %>% group_by(UF17) %>% summarize(count=n())
ke %>% group_by(UF13) %>% summarize(count=n())
ke %>% group_by(UB2) %>% summarize(count=n())
ke %>% group_by(BR1) %>% summarize(count=n())



print(kambar %>% group_by(bcg_date) %>% summarize(count=n()), n = 141)


print(kambar %>% group_by(IM6M1D) %>% summarize(count=n()), n = 21)

print(kambar %>% group_by(IM6BD) %>% summarize(count=n()), n = 31)

print(kambar %>% group_by(IM14) %>% summarize(count=n()), n = 31)

print(ke %>% group_by(IM14) %>% summarize(count=n()), n = 31)


print(kambar %>% group_by(IM20) %>% summarize(count=n()), n = 31)

print(ke %>% group_by(IM20) %>% summarize(count=n()), n = 31)


print(kambar %>% group_by(IM21) %>% summarize(count=n()), n = 31)

print(ke %>% group_by(IM21) %>% summarize(count=n()), n = 31)

print(kambar %>% group_by(IM26) %>% summarize(count=n()), n = 31)

print(ke %>% group_by(IM26) %>% summarize(count=n()), n = 31)

print(kambar %>% group_by(IM26A) %>% summarize(count=n()), n = 31)

print(ke %>% group_by(IM26A) %>% summarize(count=n()), n = 31)
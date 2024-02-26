### Author: Farhad Panahov
### Purpose: Sets ONET-ESCO mapping and get green tasks mapping



########################
### START ##############
########################
# clear all
rm(list = ls())
cat("\014")

# packages
if(!require(tidyverse))
  install.packages("tidyverse")
library(tidyverse)

if(!require(readxl))
  install.packages("readxl")
library(readxl)





#######################################
### SECTION 1: LOAD DATA ##############
#######################################

############
### ESCO ###
############

# occupations - esco
df_esco_occup_unit <- read.csv("1 - input/2 - esco/occupations_en.csv")
df_esco_occup_unit$iscoGroup <- ifelse(nchar(df_esco_occup_unit$iscoGroup) == 3,
                                       str_c("0", df_esco_occup_unit$iscoGroup),
                                       df_esco_occup_unit$iscoGroup) # add leading zero to groups starting with zero, since they miss it




############
### ONET ###
############

# occupation - onet
df_onet_occup_titles <- read_excel("1 - input/1 - onet/Occupation Data.xlsx")


# green occupations
df_green_occupation <- read_excel("1 - input/1 - onet/Green Occupations.xlsx")


# tasks
df_tasks <- read_excel("1 - input/1 - onet/Task Statements.xlsx")


# tasks - green
df_tasks_green <- read_excel("1 - input/1 - onet/Green Task Statements.xlsx")


# crosswalk
df_crosswalk_onet_esco <- read.csv("1 - input/1 - onet/onet-esco-crosswalk-exhaustive.csv", skip = 16)
df_crosswalk_onet_esco$esco_code <- df_esco_occup_unit$code[match(df_crosswalk_onet_esco$ESCO.or.ISCO.URI, df_esco_occup_unit$conceptUri)]
df_crosswalk_onet_esco <- df_crosswalk_onet_esco %>% filter(.$Type.of.Match != "wrongMatch")


# greenness measure ESCO
df_esco_greenness <- read.csv("2 - output/1 - green skills share.csv")
df_esco_greenness$code_minor <- as.character(df_esco_greenness$code_minor)
df_esco_greenness$code_minor[df_esco_greenness$code_minor == "11"] <- "011"
df_esco_greenness$code_minor[df_esco_greenness$code_minor == "21"] <- "021"
df_esco_greenness$code_minor[df_esco_greenness$code_minor == "31"] <- "031"





############################
### SECTION 2: CROSSWALK ###
############################

# exact match
df_crosswalk_exact <- df_crosswalk_onet_esco %>% filter(.$Type.of.Match == "exactMatch") %>% subset()
df_esco_occup_unit$onet_title <- df_crosswalk_exact$O.NET.Title[match(df_esco_occup_unit$conceptUri, df_crosswalk_exact$ESCO.or.ISCO.URI)]
df_esco_occup_unit$onet_code <- df_crosswalk_exact$O.NET.Id[match(df_esco_occup_unit$conceptUri, df_crosswalk_exact$ESCO.or.ISCO.URI)]

rm(df_crosswalk_exact)


# close match
df_crosswalk_close <- df_crosswalk_onet_esco %>% filter(.$Type.of.Match == "closeMatch") %>% subset()
df_esco_occup_unit$onet_title <- ifelse(is.na(df_esco_occup_unit$onet_title),
                                        df_crosswalk_close$O.NET.Title[match(df_esco_occup_unit$conceptUri, df_crosswalk_close$ESCO.or.ISCO.URI)],
                                        df_esco_occup_unit$onet_title)
df_esco_occup_unit$onet_code <- ifelse(is.na(df_esco_occup_unit$onet_code),
                                        df_crosswalk_close$O.NET.Id[match(df_esco_occup_unit$conceptUri, df_crosswalk_close$ESCO.or.ISCO.URI)],
                                        df_esco_occup_unit$onet_code)

rm(df_crosswalk_close)


# broad match
df_crosswalk_broad <- df_crosswalk_onet_esco %>% filter(.$Type.of.Match == "broadMatch") %>% subset()
df_esco_occup_unit$onet_title <- ifelse(is.na(df_esco_occup_unit$onet_title),
                                        df_crosswalk_broad$O.NET.Title[match(df_esco_occup_unit$conceptUri, df_crosswalk_broad$ESCO.or.ISCO.URI)],
                                        df_esco_occup_unit$onet_title)
df_esco_occup_unit$onet_code <- ifelse(is.na(df_esco_occup_unit$onet_code),
                                        df_crosswalk_broad$O.NET.Id[match(df_esco_occup_unit$conceptUri, df_crosswalk_broad$ESCO.or.ISCO.URI)],
                                        df_esco_occup_unit$onet_code)

rm(df_crosswalk_broad)


# related match
df_crosswalk_related <- df_crosswalk_onet_esco %>% filter(.$Type.of.Match == "relatedMatch") %>% subset()
df_esco_occup_unit$onet_title <- ifelse(is.na(df_esco_occup_unit$onet_title),
                                        df_crosswalk_related$O.NET.Title[match(df_esco_occup_unit$conceptUri, df_crosswalk_related$ESCO.or.ISCO.URI)],
                                        df_esco_occup_unit$onet_title)
df_esco_occup_unit$onet_code <- ifelse(is.na(df_esco_occup_unit$onet_code),
                                        df_crosswalk_related$O.NET.Id[match(df_esco_occup_unit$conceptUri, df_crosswalk_related$ESCO.or.ISCO.URI)],
                                        df_esco_occup_unit$onet_code)

rm(df_crosswalk_related)


# narrow match
df_crosswalk_narrow <- df_crosswalk_onet_esco %>% filter(.$Type.of.Match == "narrowMatch") %>% subset()
df_esco_occup_unit$onet_title <- ifelse(is.na(df_esco_occup_unit$onet_title),
                                        df_crosswalk_narrow$O.NET.Title[match(df_esco_occup_unit$conceptUri, df_crosswalk_narrow$ESCO.or.ISCO.URI)],
                                        df_esco_occup_unit$onet_title)
df_esco_occup_unit$onet_code <- ifelse(is.na(df_esco_occup_unit$onet_code),
                                       df_crosswalk_narrow$O.NET.Id[match(df_esco_occup_unit$conceptUri, df_crosswalk_narrow$ESCO.or.ISCO.URI)],
                                       df_esco_occup_unit$onet_code)

rm(df_crosswalk_narrow)


# missing occupations from crosswalk
df_esco_missing <- df_esco_occup_unit %>% filter(is.na(.$onet_title)) %>% subset()

# for missing --- there are 123 ESCO occupations missing ONET crosswalk. 





#############################
### SECTION 3: ONET GREEN ###
#############################

# for each occup add type of occup
df_onet_occup_titles$type <- df_green_occupation$`Green Occupational Category`[match(df_onet_occup_titles$`O*NET-SOC Code`,
                                                                                     df_green_occupation$`O*NET-SOC Code`)]

df_onet_occup_titles$type[is.na(df_onet_occup_titles$type)] <- "Non-green"


# add count of tasks
df_task_count <- df_tasks %>% group_by(.$`O*NET-SOC Code`) %>% distinct(.$`Task ID`) %>% summarise(task_total = n())
colnames(df_task_count) <- c("onet_code", "task_total")


df_task_count_green <- df_tasks_green %>% group_by(.$`O*NET-SOC Code`, .$`Green Task Type`) %>% distinct(.$`Task ID`) %>% summarise(task_green = n())
colnames(df_task_count_green) <- c("onet_code","task_type", "task_green")


# add tasks to occups
df_onet_occup_titles$task_total <- df_task_count$task_total[match(df_onet_occup_titles$`O*NET-SOC Code`,
                                                                  df_task_count$onet_code)]


# now add green tasks --- first filter by task type --- then add
filter_existing <- df_task_count_green %>%
  filter(task_type == "Existing Green Task")

filter_new <- df_task_count_green %>%
  filter(task_type == "New Green Task")


df_onet_occup_titles$task_green_exist <- filter_existing$task_green[match(df_onet_occup_titles$`O*NET-SOC Code`,
                                                                          filter_existing$onet_code)]

df_onet_occup_titles$task_green_new <- filter_new$task_green[match(df_onet_occup_titles$`O*NET-SOC Code`,
                                                                   filter_new$onet_code)]


df_onet_occup_titles$tasks_green <- coalesce(df_onet_occup_titles$task_green_exist, 0) + 
  coalesce(df_onet_occup_titles$task_green_new, 0) # this sums green tasks in general


# remove
rm(filter_existing, filter_new)
rm(df_task_count, df_task_count_green)


# combine tasks
df_tasks <- df_tasks %>% select(`O*NET-SOC Code`, Title, `Task ID`, Task)
df_tasks_green_temp <- df_tasks_green %>% select(`O*NET-SOC Code`, Title, `Task ID`, Task)

df_tasks <- rbind(df_tasks, df_tasks_green_temp)
df_tasks$type <- df_tasks_green$`Green Task Type`[match(df_tasks$`O*NET-SOC Code`, df_tasks_green$`O*NET-SOC Code`)]
df_tasks$type[is.na(df_tasks$type)] <- "Non-green"


# remove
rm(df_tasks_green, df_tasks_green_temp, df_green_occupation,df_crosswalk_onet_esco)





############################
### SECTION 4: ADD TASKS ###
############################

# new df_esco --- only select columns
df_esco <- df_esco_occup_unit
df_esco <- df_esco %>% select(c(preferredLabel, conceptUri, code, onet_title, onet_code))
rm(df_esco_occup_unit)


# remove missing ONET crosswalks
df_esco <- df_esco %>% filter(!is.na(.$onet_code)) # another 142 occupation from are missing tasks in general in ONET


# add occup type
df_esco$occup_type <- df_onet_occup_titles$type[match(df_esco$onet_code, df_onet_occup_titles$`O*NET-SOC Code`)]


# add tasks
df_esco$task_total <- df_onet_occup_titles$task_total[match(df_esco$onet_code, df_onet_occup_titles$`O*NET-SOC Code`)]


# remove occupations that dont have any tasks
df_onet_missing <- df_esco %>% filter(is.na(task_total)) # another 142 occupation from are missing tasks in general in ONET
df_esco <- df_esco %>% filter(!is.na(.$task_total)) # filter occupations missing tasks

df_esco$task_green_exist <- df_onet_occup_titles$task_green_exist[match(df_esco$onet_code, df_onet_occup_titles$`O*NET-SOC Code`)]
df_esco$task_green_exist[is.na(df_esco$task_green_exist)] <- 0

df_esco$task_green_new <- df_onet_occup_titles$task_green_new[match(df_esco$onet_code, df_onet_occup_titles$`O*NET-SOC Code`)]
df_esco$task_green_new[is.na(df_esco$task_green_new)] <- 0


# total green tasks & get green tasks share
df_esco$task_total_green <- df_esco$task_green_exist + df_esco$task_green_new
df_esco$task_green_share <- df_esco$task_total_green / df_esco$task_total





############################
### SECTION 4: ADD TASKS ###
############################

# get 3 digit minor codes to the dataset
df_esco$code_minor <- substr(df_esco$code, 1,3)


# get number of occupation by esco minor and onet green type
df_esco_minor_ONETgreen <- df_esco %>% group_by(code_minor, occup_type) %>% summarise(count = n())
df_esco_minor_ONETgreen <- pivot_wider(df_esco_minor_ONETgreen, names_from = occup_type, values_from = count)


# nA's to zero
df_esco_minor_ONETgreen$`Non-green`[is.na(df_esco_minor_ONETgreen$`Non-green`)] <- 0
df_esco_minor_ONETgreen$`Green Enhanced Skills`[is.na(df_esco_minor_ONETgreen$`Green Enhanced Skills`)] <- 0
df_esco_minor_ONETgreen$`Green Increased Demand`[is.na(df_esco_minor_ONETgreen$`Green Increased Demand`)] <- 0
df_esco_minor_ONETgreen$`Green New & Emerging`[is.na(df_esco_minor_ONETgreen$`Green New & Emerging`)] <- 0


# total occup number
df_esco_minor_ONETgreen$total_occups <- df_esco_minor_ONETgreen$`Non-green` + df_esco_minor_ONETgreen$`Green Enhanced Skills` +
  df_esco_minor_ONETgreen$`Green Increased Demand` + df_esco_minor_ONETgreen$`Green New & Emerging`


# total green
df_esco_minor_ONETgreen$total_green <- df_esco_minor_ONETgreen$`Green Enhanced Skills` +
  df_esco_minor_ONETgreen$`Green Increased Demand` + df_esco_minor_ONETgreen$`Green New & Emerging`





############################
### SECTION 4: AGGREGATE ###
############################

# summarize by occup group
df_esco_task_summary <- df_esco %>% group_by(code_minor) %>% summarise(task_green_share = mean(task_green_share), 
                                                                  task_count_total = sum(task_total, na.rm = TRUE),
                                                                  task_count_green = sum(task_total_green, na.rm = TRUE))





##########################################
### SECTION 5: COMBINE ESCO WITH TASKS ###
##########################################

# match tasks counts/shares to ESCO/ISCO minor occupation groups
df_esco_greenness$task_green_share <- df_esco_task_summary$task_green_share[match(df_esco_greenness$code_minor, df_esco_task_summary$code_minor)]
df_esco_greenness$task_count_total <- df_esco_task_summary$task_count_total[match(df_esco_greenness$code_minor, df_esco_task_summary$code_minor)]
df_esco_greenness$task_count_total <- df_esco_task_summary$task_count_green[match(df_esco_greenness$code_minor, df_esco_task_summary$code_minor)]


# add occupation counts to ESCO-onet tasks dataframe
df_esco_task_summary$ONET_total_occups <- df_esco_minor_ONETgreen$total_occups[match(df_esco_task_summary$code_minor, df_esco_minor_ONETgreen$code_minor)]
df_esco_task_summary$ONET_total_green <- df_esco_minor_ONETgreen$total_green[match(df_esco_task_summary$code_minor, df_esco_minor_ONETgreen$code_minor)]





################################
### SECTION 6: SAVE ############
################################

save.image("C:/Users/panah/OneDrive/Desktop/rdir/1 - WBG - Green Skills Project/3 - env/2 - onet - tasks.R.RData")

write.csv(df_esco_greenness, "2 - output/2 - green skills tasks share.csv")
write.csv(df_esco_missing, "2 - output/2 - missing - esco onet crosswalk.csv")
write.csv(df_onet_missing, "2 - output/2 - missing - onet tasks.csv")



### Date Nov 13 2023
### Author: Farhad Panahov
### Purpose: Gets isco08/esco at 3 digit level and mapps skills and gets share of green skills



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

if(!require(ggplot2))
  install.packages("ggplot2")
library(ggplot2)

if(!require(readxl))
  install.packages("readxl")
library(readxl)







#######################################
### SECTION 1: LOAD DATA ##############
#######################################

# occupations
df_esco_occup_hierarchy <- read.csv("1 - input/2 - esco/broaderRelationsOccPillar_en.csv")
df_esco_occup_unit <- read.csv("1 - input/2 - esco/occupations_en.csv")
df_esco_skills_occup <- read.csv("1 - input/2 - esco/occupationSkillRelations_en.csv")

# skills
df_esco_skills_green <- read.csv("1 - input/2 - esco/greenSkillsCollection_en.csv")
df_esco_skills_digital <- read.csv("1 - input/2 - esco/digitalSkillsCollection_en.csv")
#df_esco_skills_transversal <- read.csv("1 - input/2 - esco/transversalSkillsCollection_en.csv")
df_esco_skills_research <- read.csv("1 - input/2 - esco/researchSkillsCollection_en.csv")
df_esco_skills_comp <- read.csv("1 - input/2 - esco/digCompSkillsCollection_en.csv")
df_esco_skills <- read.csv("1 - input/2 - esco/skills_en.csv")






##########################
### MAKE EDITS: SKILLS ###
##########################

# keep only essentail skills for each occupation
#df_esco_skills_occup <- df_esco_skills_occup %>% filter(.$relationType == "essential") %>% subset()


# name the skill
df_esco_skills_occup$name <- df_esco_skills$preferredLabel[match(df_esco_skills_occup$skillUri, df_esco_skills$conceptUri)]


# skill category
df_esco_skills_occup$category <- "general"

# add green
indices <- which(df_esco_skills_occup$skillUri %in% df_esco_skills_green$conceptUri)
df_esco_skills_occup$category[indices] <- "green"

# add digital
indices <- which(df_esco_skills_occup$skillUri %in% df_esco_skills_digital$conceptUri)
df_esco_skills_occup$category[indices] <- "stem"

# add digital comp
indices <- which(df_esco_skills_occup$skillUri %in% df_esco_skills_comp$conceptUri)
df_esco_skills_occup$category[indices] <- "stem"

# add research
indices <- which(df_esco_skills_occup$skillUri %in% df_esco_skills_research$conceptUri)
df_esco_skills_occup$category[indices] <- "stem"

# add transerversal
# indices <- which(df_esco_skills_occup$skillUri %in% df_esco_skills_transversal$conceptUri)
# df_esco_skills_occup$category[indices] <- "transversal"

rm(indices)
rm(df_esco_skills_comp, df_esco_skills_research, df_esco_skills_green, df_esco_skills_digital)






#######################################
### SECTION 2: MAKE EDIT ##############
#######################################

# identify occupational blocks (digits --- eg: 7543.10.2.3 --- 7543 = block 1, .10 = block 2, etc.)
df_esco_occup_unit$num_blocks <- str_count(df_esco_occup_unit$code, fixed(".")) + 1

length(unique(df_esco_occup_unit$iscoGroup)) ### --> 426
sum(df_esco_occup_unit$num_blocks == 2) ### --> 1760
sum(df_esco_occup_unit$num_blocks == 3) ### --> 1072
sum(df_esco_occup_unit$num_blocks == 4) ### --> 135
sum(df_esco_occup_unit$num_blocks == 5) ### --> 40
sum(df_esco_occup_unit$num_blocks == 6) ### --> 0




### df_esco_occup 5 digits
# get 5 digit levels
#df_esco_occup_unit$code_unit <- substr(df_esco_occup_unit$code,1,6)
df_esco_occup_unit$code_unit <- sub("^([^.]*\\.[^.]*).*", "\\1", df_esco_occup_unit$code)




# for skills add the occupation code
df_esco_skills_occup$code_unit <- df_esco_occup_unit$code_unit[match(df_esco_skills_occup$occupationUri, 
                                                                     df_esco_occup_unit$conceptUri)]


# get 3 digit minor
df_esco_skills_occup$code_minor <- substr(df_esco_skills_occup$code_unit,1,3)


# get 1 digit
df_esco_skills_occup$code_major <- substr(df_esco_skills_occup$code_unit, 1,1)







#########################################
### SECTION 2: GREEN SHARE ##############
#########################################

###############
### 1 digit ###
###############

# summarize by occup group
df_green_major <- df_esco_skills_occup %>% group_by(code_major, occupationUri, category) %>% summarise(count = n_distinct(skillUri))


# lond to wide format
df_green_major <- df_green_major %>% pivot_wider(names_from = category, values_from = count)
df_green_major <- df_green_major %>% filter(!is.na(code_major))


# if NA replace with zero
df_green_major$green[is.na(df_green_major$green)] <- 0
df_green_major$stem[is.na(df_green_major$stem)] <- 0
df_green_major$general[is.na(df_green_major$general)] <- 0


# find green share
df_green_major$green_share <- df_green_major$green / (df_green_major$general + df_green_major$green + df_green_major$stem)


# find stem share
df_green_major$stem_share <- df_green_major$stem / (df_green_major$general + df_green_major$green + df_green_major$stem)


# summarize
df_green_major <- df_green_major %>% group_by(code_major) %>% summarise(green_share = mean(green_share), stem_share = mean(stem_share))





###############
### 3 digit ###
###############

# summarize by occup group
df_green_minor <- df_esco_skills_occup %>% group_by(code_minor, occupationUri, category) %>% summarise(count = n_distinct(skillUri))
df_green_minor <- df_green_minor %>% filter(!is.na(count))


# lond to wide format
df_green_minor <- df_green_minor %>% pivot_wider(names_from = category, values_from = count)
df_green_minor <- df_green_minor %>% filter(!is.na(code_minor))


# if NA replace with zero
df_green_minor$green[is.na(df_green_minor$green)] <- 0
df_green_minor$stem[is.na(df_green_minor$stem)] <- 0
df_green_minor$general[is.na(df_green_minor$general)] <- 0


# find green share
df_green_minor$green_share <- df_green_minor$green / (df_green_minor$general + df_green_minor$green + df_green_minor$stem)


# find stem share
df_green_minor$stem_share <- df_green_minor$stem / (df_green_minor$general + df_green_minor$green + df_green_minor$stem)


# summarize
df_green_minor <- df_green_minor %>% group_by(code_minor) %>% summarise(green_share = mean(green_share), stem_share = mean(stem_share))






#########################################
### SECTION 3: GREEN COUNT ##############
#########################################

# count by 1 digit
df_temp_1digit <- df_esco_skills_occup %>% group_by(code_major, category) %>% summarise(count = n_distinct(skillUri))
df_temp_1digit <- df_temp_1digit %>%pivot_wider(names_from = category, values_from = count)
df_temp_1digit$total <- df_temp_1digit$general + df_temp_1digit$green + df_temp_1digit$stem

df_green_major$count_total <- df_temp_1digit$total[match(df_green_major$code_major, df_temp_1digit$code_major)]
df_green_major$count_green <- df_temp_1digit$green[match(df_green_major$code_major, df_temp_1digit$code_major)]
df_green_major$count_stem <- df_temp_1digit$stem[match(df_green_major$code_major, df_temp_1digit$code_major)]




# count by 3 digit
df_temp_3digit <- df_esco_skills_occup %>% group_by(code_minor, category) %>% summarise(count = n_distinct(skillUri))
df_temp_3digit <- df_temp_3digit %>%pivot_wider(names_from = category, values_from = count)

df_temp_3digit <- df_temp_3digit %>% mutate(across(everything(), ~replace(., is.na(.), 0))) # replace NAs with zeros
df_temp_3digit$total <- df_temp_3digit$general + df_temp_3digit$green + df_temp_3digit$stem

df_green_minor$count_total <- df_temp_3digit$total[match(df_green_minor$code_minor, df_temp_3digit$code_minor)]
df_green_minor$count_green <- df_temp_3digit$green[match(df_green_minor$code_minor, df_temp_3digit$code_minor)]
df_green_minor$count_stem <- df_temp_3digit$stem[match(df_green_minor$code_minor, df_temp_3digit$code_minor)]



# 
rm(df_temp_1digit, df_temp_3digit)





###############################################
### SECTION 4: GREEN OCCUPATIONS ##############
###############################################


# identify green skills
# method: if green share & green count >= 75% percentile
df_green_minor$green_by_skill <- NA
df_green_minor$green_by_skill[df_green_minor$green_share >= quantile(df_green_minor$green_share)[4] & 
                                df_green_minor$count_green>= quantile(df_green_minor$count_green)[4]] <- "green"


# green occupations list
df_green_occups <- df_green_minor %>% filter(green_by_skill == "green") %>% subset()



####
# green occupation distribution based on percentile choice
# keep changing manually percentiles --- 0,1 to 0,9
df_distribution_greenjob_summary <- data.frame(choice = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), occup_count = NA)

df_distribution_greenjob <- df_green_minor

df_distribution_greenjob$green_by_skill <- NA
df_distribution_greenjob$green_by_skill[df_distribution_greenjob$green_share >= quantile(df_distribution_greenjob$green_share,0.75) & 
                                          df_distribution_greenjob$count_green>= quantile(df_distribution_greenjob$count_green,0.75)] <- "green"


df_distribution_greenjob_summary$occup_count[1] <- sum(df_distribution_greenjob$green_by_skill == "green", na.rm = TRUE)
df_distribution_greenjob_summary$occup_count[2] <- sum(df_distribution_greenjob$green_by_skill == "green", na.rm = TRUE)
df_distribution_greenjob_summary$occup_count[3] <- sum(df_distribution_greenjob$green_by_skill == "green", na.rm = TRUE)
df_distribution_greenjob_summary$occup_count[4] <- sum(df_distribution_greenjob$green_by_skill == "green", na.rm = TRUE)
df_distribution_greenjob_summary$occup_count[5] <- sum(df_distribution_greenjob$green_by_skill == "green", na.rm = TRUE)
df_distribution_greenjob_summary$occup_count[6] <- sum(df_distribution_greenjob$green_by_skill == "green", na.rm = TRUE)
df_distribution_greenjob_summary$occup_count[7] <- sum(df_distribution_greenjob$green_by_skill == "green", na.rm = TRUE)
df_distribution_greenjob_summary$occup_count[8] <- sum(df_distribution_greenjob$green_by_skill == "green", na.rm = TRUE)
df_distribution_greenjob_summary$occup_count[9] <- sum(df_distribution_greenjob$green_by_skill == "green", na.rm = TRUE)


df_distribution_greenjob_summary$green_occup_share <- df_distribution_greenjob_summary$occup_count/125*100





################################
### SECTION 4: SAVE ############
################################

save.image("C:/Users/panah/OneDrive/Desktop/WBG - green skills/R - work folder - new/3 - env/1 - esco - green skill share.DATA")


write.csv(df_green_major, "2 - output/1 - green_skill_share_1digit.csv")
write.csv(df_green_minor, "2 - output/1 - green_skill_share_3digit.csv")
write.csv(df_esco_skills_occup, "2 - output/1 - skills X occup.csv")
write.csv(df_distribution_greenjob_summary, "2 - output/1 - distribution_green_choice.csv")




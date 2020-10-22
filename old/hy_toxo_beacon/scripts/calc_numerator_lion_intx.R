### 3.6 Tidy boldness lion data (approach lions)
## a) Format and rename the date in lion_bold
lion_bold <- lion_bold %>%
  mutate(lion_date = as.Date(lion_bold$Date, 
                             format = "%m/%d/%y"))

## b) Drop old date column
lion_bold <- lion_bold %>%
  select (- c(Date))

## c) Rename variable and reformat as factor
lion_bold <- lion_bold %>%
  rename("id" = "Hyena")  

## d) Join tblHyenas id, birthdate, etc. to tblDarting
lion_bold <- lion_bold %>%
  left_join(select(tblHyenas, c(id, birthdate, age_class, status, sex)), 
            by = c("id" = "id"))

## e) Create an estimated age in months by subtracting birthdate from
# lion_date using lubridate
lion_bold <- lion_bold %>%
  mutate(age_mon_lion = round((interval(birthdate, 
                                        lion_date) %/% days(1)/ 30.44), 1))

## NOTE: To convert from age in days to age in months divde by average 
# number of days per month  (30.44)  


## h) Join lion_bold to toxo_data
lion_bold <- lion_bold %>%
  left_join(select(toxo_data, c(id, darting_date, spratio, diagnosis)), 
            by = c("id" = "id"))

## i) Generate a flag to indicate for every hyena lion interaction, 
# if that interaction comes before or after diagnosis
lion_bold$diagnosis_time <- ifelse((lion_bold$lion_date >= 
                                      lion_bold$darting_date), 'after',
                                   (ifelse(is.na(lion_bold$lion_date), 
                                           NA, 'before')))

## j) Create an estimated age in months by subtracting birthdate from
# darting_date using lubridate
lion_bold <- lion_bold %>%
  mutate(age_mon_dart = round((interval(birthdate, 
                                        darting_date) %/% days(1)/ 
                                 30.44), 1))


## k) Create a binary factor for diagnosis as toxo_status
lion_bold <- lion_bold %>%
  mutate(toxo_status = as.factor(ifelse(diagnosis == "negative" | 
                                          diagnosis == "doubtful", 0, 1))) 



## m) subset data to include only lion data before negative infection
# and after postive infection
lion_bold_sero <- lion_bold %>%
  filter ((grepl("positive", diagnosis) & 
             grepl("after", diagnosis_time)) | 
            (grepl("negative", diagnosis) & 
               grepl("before", diagnosis_time)))


## n)  Summarize boldness lion data
lion_bold_sero_sum <- lion_bold_sero %>%
  arrange(lion_date) %>%
  group_by (id) %>% # set grouping same ID within same cat age
  summarise (id_reps_lion = sum(!is.na(MinDist)), # n per hyena id
             
             # closest hyena ever gets to lion
#             min_dist_lion = min(MinDist), 
             # lifetime average of minimum approach distances
#             life_avg_min_dist_lion = round(mean(MinDist, na.rm = T), 2),
             # average of the rank_dist/totWdist per hyena
#             std_rank_min_dist_lion = round(mean((rank_dist/totWdist), 
#                                                 na.rm = T), 2),
             # first retain other variables
#            age_class = first(age_class), 
#             status = first(status),
             sex = first(sex),
             age_mon_lion = round(mean(age_mon_lion),2),
             age_cat_lion  = first(age_cat_lion),
             spratio = first(spratio),
             diagnosis = first(diagnosis),
             diagnosis_time_first = first(diagnosis_time),
             diagnosis_time_fin = last(diagnosis_time),
             toxo_status = first(toxo_status),
             age_mon_dart = first(age_mon_dart))

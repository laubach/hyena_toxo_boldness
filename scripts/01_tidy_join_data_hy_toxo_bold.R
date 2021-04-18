###############################################################################
#############   Toxoplasma gondii infections are associated with   #############
#############     costly boldness toward felids in a wild host     #############
#############                                                      #############
#############                1. Tidy and Join Data                 #############
#############                                                      #############
#############          By: Zach Laubach and Eben Gerring           #############
#############               created: 12 July 2018                  #############
#############            last modified: 18 April 2021              #############
################################################################################


### PURPOSE: Tidy and join data in preparation for downstream analyses. 
           # Create 3 data sets for modeling:
              # Determinants of Toxo.
              # Toxo. Hyena Lion Dist.
              # Toxo. Hyena Mortality


  # Code Blocks
    # 1: Configure workspace
    # 2: Import data
    # 3: Tidy base data tables 
    # 4: Derterminants of Toxo: Join and re-tidy data
    # 5: Toxo Hyena Lion Dist: Join and re-tidy data
    # 6: Toxo Hyena Mortality: Join and re-tidy data
    # 7: Export data files
  

###############################################################################
##############              1. Configure workspace               ##############
###############################################################################

  ### 1.1 Global options
    ## a) clear global environment
      rm(list = ls())
    
    ## b) prevent R from automatically reading charater strins as factors
      options(stringsAsFactors = FALSE)
    
    # ## c) Setup packrat for reproducibility
    #   library('packrat')
    #   packrat::init('.') #initiate packrat in the current working directory
  
  
  ### 1.2 Install and load CRAN packages
    ## a) Data Manipulation and Descriptive Stats Packages
    
      # load tidyverse packages
        library ('tidyverse')
      
      # load here packages
        library ('here')
      
      # load lubridate package
        library('lubridate') 
      
  
  ### 1.3 Get Version and Session Info
    ## a) R and Package versions
      R.Version()
      sessionInfo()
      
      # Developed in:   
      # R version 4.0.2 (2020-06-22)
      # Platform: x86_64-apple-darwin17.0 (64-bit)
      # Running under: macOS Catalina 10.15.7

    
  ### 1.4 Set file paths outside of working directory
    ## a) Source scripts path
      source_path <- paste('~/Git/source_code/')  
      
  
  ### 1.5 Source functions
    ## a) all_char_to_lower function
      source(file = paste0(source_path, 'all_char_to_lower.R'))
      
    ## b) format_var.names function
      source(file = paste0(source_path, 'format_var_names.R'))
      
    ## c) format_var.names_dash function
      source(file = paste0(source_path, 'format_var_names_dash.R'))  
    
    
    
###############################################################################
##############                  2. Import data                   ##############
###############################################################################    

  ### 2.1 Load RData
    ## a) Load RData (diognotistics, hy lion intx., and hyena data base)
      load(here('data/01_raw_data_hy_toxo_bold.RData'))
      
      
      
###############################################################################
##############    3. Data management: Tidy basic data tables     ##############
###############################################################################

  ### 3.1 Tidy toxo_data  
    ## a) Standardize variable names in the form 'var.name' 
      toxo_data <- FormatVarNames (toxo_data)
      
    ## b) Convert tibble to data frame to fix bug that cuases this warning:
      toxo_data <- as.data.frame(toxo_data)
      
    ## c) Format and rename the darting date in toxo_data    
      toxo_data <- toxo_data %>%
        mutate(date = as.Date(toxo_data$date,
                              format = '%m/%d/%y')) %>%
        rename('dart.date' = 'date')
   
    ## d) extract the year for date of interest (here dart.date) using 
      # lubridate and make a new variable
      toxo_data$dart.year <- year(as.Date(toxo_data$dart.date, 
                                          format='%Y-%m-%d'))
      
    ## e) Drop unnecessary columns
      toxo_data <- toxo_data %>%
        select (- c(alt.code, sex, clan, hadley.notes, time.min,  
                     new.box, cell, type, sample.buffer, state,
                    orig.est.volume.u.l, current.vol.u.l)) 
      
    ## f) Rename id as hy.id 
      toxo_data <-  toxo_data %>%
        rename('hy.id' = 'id') 
      
#********************** Data Inclusion/Exclusion Criteria **********************
    ## e) Save original data for sensitivity analyses
      toxo_data_sens <- toxo_data
    
      # remove 'gil', kaycode 87, which has negative sp ratio 
      # and 'baj', kaycode 84, which is extreme outlier based on SPratio vs.
      # IFAT correlation
      toxo_data <- toxo_data %>%
        filter(kay.code != 87 & kay.code != 84)
#********************** Data Inclusion/Exclusion Criteria **********************       
           
      
  ### 3.2 Tidy hyenas (aka tblHyenas)
    ## a) Convert all text to lower case
      hyenas <- AllCharactersToLower(hyenas)
      
    ## b) Format variable names (lowercase and separated by '.')
      hyenas <- FormatVarNames(hyenas)  
      
    ## c) Rename id as hy.id 
      hyenas <-  hyenas %>%
        rename('hy.id' = 'id') 
      
     
  ### 3.3 Tidy life_hist (aka tblLifeHistory.wide)
    ## a) Convert all text to lower case
      life_hist <- AllCharactersToLower(life_hist)
      
    ## b) Format variable names (lowercase and separated by '.')
      life_hist <- FormatVarNames(life_hist) 
      
    ## c) Rename variables
      life_hist <- life_hist %>%
        rename('hy.id' = 'id') 
      
      life_hist <- life_hist %>%
        rename('dob.date' = 'dob')
      
      life_hist <- life_hist %>%
        rename('disappeared.date' = 'disappeared') 
      
    ## d) convert hy.id to character class  
      life_hist$hy.id <- as.character(life_hist$hy.id) 
      

  ### 3.4 Tidy female_ranks (aka tblFemaleRanks)
    ## a)  Convert all text to lower case
      female_ranks <- AllCharactersToLower(female_ranks)
      
    ## b) Format variable names (lowercase and separated by '.')
      female_ranks <- FormatVarNames(female_ranks) 
      
    ## c) convert characters to integer
      female_ranks$rank <- as.integer(female_ranks$rank)
    
    ## d) Rename variables
      female_ranks <- female_ranks %>%
        rename('rank.year' = 'year')
      
      female_ranks <- female_ranks %>%
        rename('hy.id' = 'id')
      
      female_ranks <- female_ranks %>%
        rename('rank.clan' = 'clan')
    
    ## e) convert rank.year to numeric
      female_ranks$rank.year <- as.numeric(as.character(female_ranks$rank.year))
      
    ## g) convert stan.rank to numeric
      female_ranks$stan.rank <- as.numeric(as.character(female_ranks$stan.rank))
 
      
  ### 3.5 Tidy lion_hy_dist_raw
    ## a) Standardize variable names in the form 'var.name' 
      lion_hy_dist_raw <- FormatVarNames(lion_hy_dist_raw)
    
    ## b) rename variable
      lion_hy_dist_raw <- lion_hy_dist_raw %>%
        rename('hy.id' = 'hyena')
      
    ## c) Covert all data to lowercase
      lion_hy_dist_raw <- AllCharactersToLower(lion_hy_dist_raw)
      
    ## d) Left join tblSessions info to lion_hy_dist_raw dataframe
      lion_hy_dist_raw <- lion_hy_dist_raw %>%
        left_join(select(sessions, c(clan, session, location, date, 
                                        start, stop)), 
                  by = c('session' = 'session'))
      
    ## e) rename variable
      lion_hy_dist_raw <- lion_hy_dist_raw %>%
        rename('lion.date' = 'date')
      
    ## f) Extract year from dob.date
      # Use lubridate to extract the year during which a hyena lion  
      # interaction occured and make a new variable  
      lion_hy_dist_raw$lion.yr <- year(lion_hy_dist_raw$lion.date)

    
  ### 3.6 Tidy data with precise distance
     ## a) Subset data where distance only includes single number or NA
      lion_hy_dist <- lion_hy_dist_raw %>%
        filter(grepl('^[[:digit:]]+$', distance))
      # | is.na(lion_hy_dist$distance)) 
      
    ## b) Convert distance to numeric
      lion_hy_dist$distance <- as.numeric(as.character(lion_hy_dist$distance))
      
        
  ### 3.7 Tidy data with distance ranges      
    ## a) Subset data where distance is a range 
      lion_hy_dist_range <- lion_hy_dist_raw %>%
#        filter(grepl('[^[:alnum:]]', distance))
        filter(grepl('-', distance))
      
    ## b) Separate united info into spearate variables/columns
      lion_hy_dist_range <- lion_hy_dist_range %>%
        separate(col = distance, into = c('min.dist', 
                                         'max.dist'), sep = '-',
                 remove = T)
      
    ## c) Convert min and max dist from character to numeric
      lion_hy_dist_range$min.dist <- as.numeric(lion_hy_dist_range$min.dist)
      lion_hy_dist_range$max.dist <- as.numeric(lion_hy_dist_range$max.dist)

    ## d) Determine the range of the distance estimate (for inclusion/exclusion)
      lion_hy_dist_range <- lion_hy_dist_range %>%
        mutate(dist.dif = (max.dist - min.dist))
    
    ## e) check sd of lion_hy_dist
      dist.sd = sd(lion_hy_dist$distance)
      dist.avg = mean(lion_hy_dist$distance)
      
#********************** Data Inclusion/Exclusion Criteria **********************       
    ## f) Exlclude data where the distance range is greater than 
      # approximately 1/2 a SD, or 25m
      lion_hy_dist_range <- lion_hy_dist_range %>%
        filter(dist.dif <= 25)
  #*** NOTE *** This is a data inclusion cut-off decision. 
#********************** Data Inclusion/Exclusion Criteria **********************       
      
    ## g) Determine the mean of min and max dist
      lion_hy_dist_range <- lion_hy_dist_range %>%
        mutate(distance = (max.dist + min.dist)/2)
      
    ## h) Drop extra columns
      lion_hy_dist_range <- lion_hy_dist_range %>%
        select(-c(min.dist, max.dist, dist.dif))
       
    ## i) Subset data where distance is an inequality  
      lion_hy_dist_grt_less <- lion_hy_dist_raw %>%
        filter(grepl('>', distance) | grepl('<', distance))
      
    ## j) Separate <> sign and dist number into spearate variables/columns
      lion_hy_dist_grt_less <- lion_hy_dist_grt_less %>%
        separate(col = distance, into = c('grt.lss.sign'), 
               sep = '[[:alnum:]$]',
               remove = F) %>% # when false, keeps the original col
        mutate(distance = (str_extract(distance, '[0-9]+')))
   
    ## k) Convert distance to numeric
      lion_hy_dist_grt_less$distance <- 
        as.numeric(lion_hy_dist_grt_less$distance)
 
#********************** Data Inclusion/Exclusion Criteria **********************            
    ## l) Exlclude data where the < distance is less than 
      # approximately 1/2 a SD - the mean, or 25m and the > distance
      # is greater than approximately 1/2 a SD + the mean, or 75m
      lion_hy_dist_grt_less <- lion_hy_dist_grt_less %>%
        filter((grepl('<', grt.lss.sign) &
                  lion_hy_dist_grt_less$distance <= 25) |
                 (grepl('>', grt.lss.sign) & 
                    lion_hy_dist_grt_less$distance >= 75))
      #*** NOTE *** This is a data inclusion cut-off decision. 
#********************** Data Inclusion/Exclusion Criteria ********************** 
           
    ## m) Drop extra columns
      lion_hy_dist_grt_less <- lion_hy_dist_grt_less %>%
        select(-c(grt.lss.sign))

      
  ### 3.8 Tidy data lion_hy_dist     
    ## a) Rbind the two filtered data frames in which distances were 
      # originally recorded as ranges on to the data frame containing 
      # single precise distance measures (lion_hy_dist)
      lion_hy_dist <- rbind(lion_hy_dist, lion_hy_dist_range, 
                            lion_hy_dist_grt_less)
      
    ## b) Convert distance to numeric
      lion_hy_dist$distance <- as.numeric(as.character(lion_hy_dist$distance))
      
    ## c) Calculate number of a hyena's approaches towards lions per session  
      num_apprchs <- lion_hy_dist %>%
        group_by(session, hy.id) %>%
        summarise(n.per.sessn = sum(!is.na(distance)))
      
    ## d) Group data by session by hyena, retaining the minimum approach
      # distance per session per hyena
      lion_hy_dist <- lion_hy_dist %>%
        group_by(session, hy.id) %>%
        filter(rank(distance, ties.method='first')==1) %>% # select 1st row per
        # per session 
        # where dist. is min
        ungroup() # ungroup the data frame.
      
    ## e) Left join num_apprchs info to lion_hy_dist dataframe
      lion_hy_dist <- lion_hy_dist %>%
        left_join(num_apprchs,  
                  by = c('session' = 'session', 'hy.id' = 'hy.id')) 
      
#********************** Data Inclusion/Exclusion Criteria ********************** 
    ## f) Exclude data where hyean distances greater 100m
      lion_hy_dist <- lion_hy_dist %>%
        filter (distance <= 100)  
      #*** NOTE *** This is a data inclusion cut-off decision. 
          # 200m is typically used as a sessioning cut-off and but 100m is 
          # likely limit of accurate distance estimation
#********************** Data Inclusion/Exclusion Criteria ********************** 
      
#********************** Data Inclusion/Exclusion Criteria **********************      
    ## g) Remove lion hyena sessions during which no interaction occured 
      lion_hy_dist <- lion_hy_dist %>%
        filter(no.intx.yes.no == 'FALSE')
    #*** NOTE *** This is a data inclusion cut-off decision.
#********************** Data Inclusion/Exclusion Criteria ********************** 
      

      
###############################################################################
##############  4: Derterminants of Toxo: Join and re-tidy data  ##############
###############################################################################      

  ### 4.1 Join access fisi backend tables to toxo_data
    ## a) Left join life_hist to toxo_data,
      toxo_data <- toxo_data %>% 
        left_join(select(life_hist, c(hy.id, dob.date, dob.event.data, 
                                      disappeared.date, 
                                      disappeared.event.status,
                                      disappeared.event.data)),
                  by = c('hy.id' = 'hy.id'))
      
    ## b) Left join hyenas to toxo_data,
      toxo_data <- toxo_data %>%
        left_join(select(hyenas, c(hy.id, sex, status, mom, dad, 
                                   number.littermates, litrank)),
                  by = c('hy.id' = 'hy.id')) 
      
   
  ### 4.2 Re-tidy toxo_data 
    ## a) Create a binary factor for diagnosis as toxo_status
      toxo_data <- toxo_data %>%
        mutate(toxo.status = as.factor(ifelse(diagnosis == 'negative' |
                                                diagnosis == 'doubtful', 
                                              0, 1)))
      
    ## b) Drop rows where there is not toxo data
      toxo_data <- toxo_data %>%
        filter(!is.na(toxo.status))
      
    ## c) Create an estimated age in months by subtracting dob.date from
      # dart.date using lubridate
      toxo_data <-toxo_data %>%
        mutate(age.mon.dart = round((interval
                                     (dob.date, dart.date) %/% 
                                       days(1)/ 30.44), 1))
      ## NOTE: To convert from age in days to age in months divde by average 
      # number of days per month  (30.44)  
      
    ## d) Extract year from dob
      # Use lubridate to extract the year during which a hyena was born 
      # and make a new variable  
      toxo_data$dob.yr <- year(toxo_data$dob.date)
      
    ## e) Update categorical age variable based dart.age.mon 
      # dart.date using lubridate
      # based on Holekamp and Smale 1998
      toxo_data <-toxo_data %>%
        mutate(age.cat.dart = case_when(sex == 'm' & age.mon.dart <= 12 
                                        ~ c('cub'),
                                        sex == 'm' & age.mon.dart > 12 & 
                                          age.mon.dart <=24 
                                        ~ c('subadult'),
                                        sex == 'm' & age.mon.dart > 24 
                                        ~ c('adult'),
                                        sex == 'f' & age.mon.dart <= 12 
                                        ~ c('cub'),
                                        sex == 'f' & age.mon.dart > 12 & 
                                          age.mon.dart <=24 
                                        ~ c('subadult'),
                                        sex == 'f' & age.mon.dart > 24 
                                        ~ c('adult')))
      
    ## f) replace NA in age_cat column where there is a value in 
      # age column
      # first convert age to character
      toxo_data$age <- as.character(toxo_data$age)
      toxo_data$age.cat.dart <- ifelse(is.na(toxo_data$age.cat.dart),
                                       toxo_data$age,
                                       toxo_data$age.cat.dart)
      
    ## g) drop old age columns
      toxo_data <-toxo_data %>%
        select (- c(age)) 
      
    ## h) Reorder age variable for graphing
      toxo_data$age.cat.dart <- factor(toxo_data$age.cat.dart, 
                                       levels = c('cub', 'subadult', 'adult'))
      
    ## i) Make a new human disturbance variable
      # create a 2-level ordinal factor indicating human pastoralist presence /
      # disturbance based Green et. al 2017
      toxo_data <- toxo_data %>%
        mutate(hum.dist.dob = case_when(toxo_data$dob.event.data
                                    %in% c('talek', 'talek.e') &
                                      toxo_data$dob.yr >= 2012
                                    ~ c('hi'),
                                    toxo_data$dob.event.data 
                                    %in% c('talek', 'talek.e') &
                                      toxo_data$dob.yr <= 2000 
                                    ~ c('low'),
                                    toxo_data$dob.event.data 
                                    %in% c('serena.n', 'serena.s',
                                           'happy.zebra')
                                    ~ c('low')))
      
    ## i) Re-code hum.dist as nominal factor and set level (order)
      toxo_data <- transform(toxo_data,
                             hum.dist.dob = factor(hum.dist.dob,
                                               levels = c('hi','low')))
      
      
  ### 4.3 Subset toxo_data into cub vs sub/adult data sets and the join with
      # female_ranks
    ## a) Left Join female_ranks to toxo_data (subs and adults then cubs)
      toxo_data_sub_adult <- toxo_data %>%
        filter(age.cat.dart == 'adult' | age.cat.dart == 'subadult') %>%
        left_join(select(female_ranks, c(hy.id, rank, stan.rank, rank.year)),
                  by = c('hy.id' = 'hy.id', 'dart.year' = 'rank.year'))
      
      toxo_data_cub <- toxo_data %>%
        filter(age.cat.dart == 'cub') %>%
        left_join(select(female_ranks, c(hy.id, rank, stan.rank, rank.year)), 
                  by = c('mom' = 'hy.id', 'dart.year' = 'rank.year'))
      
    ## b) combine 
      toxo_data <- rbind(toxo_data_sub_adult, toxo_data_cub)
      
    ## c) Rename rank variables
      toxo_data <- toxo_data %>%
        rename('rank.dart' = 'rank') %>%
        rename('stan.rank.dart' = 'stan.rank')
     
      toxo_data_sub_adult <- toxo_data_sub_adult %>%
        rename('rank.dart' = 'rank') %>%
        rename('stan.rank.dart' = 'stan.rank')
      
      toxo_data_cub <- toxo_data_cub %>%
        rename('rank.dart' = 'rank') %>%
        rename('stan.rank.dart' = 'stan.rank')
      
   
      
###############################################################################
##############  5: Toxo Hyena Lion Dist: Join and re-tidy data   ##############
###############################################################################     
    
  ### 5.1 Join hyenas, life_hist, and female_ranks tables to lion_hy_dist
    ## a) Join hyenas id, sex, etc. to lion_hy_dist
      lion_hy_dist <- lion_hy_dist %>%
        left_join(select(hyenas, c(hy.id, sex, status, mom, dad, 
                                   number.littermates, litrank)), 
                  by = c('hy.id' = 'hy.id'))
      
    ## b) Join life_hist to lion_hy_dist
      lion_hy_dist <- lion_hy_dist %>%
        left_join(life_hist, by = c('hy.id' = 'hy.id'))
      
      
  ### 5.2 Tidy lion_hy_dist and prepare data for analysis   
    ## a) Create an estimated age in months by subtracting dob.date from
      # lion.date using lubridate
      lion_hy_dist <- lion_hy_dist %>%
        mutate(age.mon.lion = round((interval(dob.date, 
                                          lion.date) %/% days(1)/ 30.44), 1))
      ## NOTE: To convert from age in days to age in months divde by average 
        # number of days per month  (30.44)  
      
    ## b) Create a categorical age variable based age_mon_lion
      # based on Holekamp and Smale 1998 age categories
      lion_hy_dist <- lion_hy_dist %>%
        mutate(age.cat.lion = case_when(age.mon.lion <= 12 ~ 
                                          c('cub'),
                                        age.mon.lion > 12 & 
                                          age.mon.lion <=24 ~ 
                                          c('subadult'),
                                        age.mon.lion > 24 ~ 
                                          c('adult')))
      
    ## c) replace NA in age.cat.lion column with adult; for animals with no 
      # known dob (i.e. females who enter study pop as adults and imm. males)
      # first convert age to character
      lion_hy_dist$age.cat.lion <- as.character(lion_hy_dist$age.cat.lion)
      lion_hy_dist$age.cat.lion <- ifelse(is.na(lion_hy_dist$age.cat.lion),
                                       'adult',
                                       lion_hy_dist$age.cat.lion)
      
    ## d) Reorder age variable for graphing
      lion_hy_dist$age.cat.lion <- factor(lion_hy_dist$age.cat.lion, 
                                       levels = c('cub', 'subadult', 'adult'))
      
    ## e) Extract year from lion interaction date
      # Use lubridate to extract the year during which a hyena interacts with 
      # a lion and make a new variable  
      lion_hy_dist$lion.yr <- year(lion_hy_dist$lion.date)
      
    ## f) HACK: remove hyena lion interactions in which animal is negative
      # years old on date of interaction
      # Includes:
        # hyena KS, sessions s3620 and s5891.1
        # hyena MRPH, sessions t17.1 and t53.5
      lion_hy_dist <- lion_hy_dist %>%
        filter(age.mon.lion >= 0 | is.na(age.mon.lion))
      
    ## g) Make a new human disturbance variable
      # create a 2-level ordinal factor indicating human pastoralist presence /
      # disturbance based Green et. al 2017
      lion_hy_dist <- lion_hy_dist %>%
        mutate(hum.dist.lion = case_when(lion_hy_dist$clan
                                        %in% c('talek', 'talek.w') &
                                          lion_hy_dist$lion.yr >= 2012
                                        ~ c('hi'),
                                        lion_hy_dist$clan 
                                        %in% c('talek', 'talek.w') &
                                          lion_hy_dist$lion.yr <= 2000 
                                        ~ c('low'),
                                        lion_hy_dist$clan 
                                        %in% c('serena.n', 'serena.s',
                                               'happy.zebra')
                                        ~ c('low')))
      
    ## i) Re-code hum.dist as nominal factor and set level (order)
      lion_hy_dist <- transform(lion_hy_dist,
                             hum.dist.lion = factor(hum.dist.lion,
                                               levels = c('hi','low')))
      
  ### 5.3 Subset lion_hy_dist into cub vs sub/adult data sets and the join with
      # female_ranks
    ## a) Left Join female_ranks to lion_hy_dist (subs and adults then cubs)
      lion_hy_dist_sub_adult <- lion_hy_dist %>%
        filter(age.cat.lion == 'adult' | age.cat.lion == 'subadult') %>%
        left_join(select(female_ranks, c(hy.id, rank, stan.rank, rank.year)),
                  by = c('hy.id' = 'hy.id', 'lion.yr' = 'rank.year'))
      lion_hy_dist_cub <- lion_hy_dist %>%
        filter(age.cat.lion == 'cub') %>%
        left_join(select(female_ranks, c(hy.id, rank, stan.rank, rank.year)), 
                  by = c('mom' = 'hy.id', 'lion.yr' = 'rank.year'))
      
    ## b) combine age specific data sets
      lion_hy_dist <- rbind(lion_hy_dist_sub_adult,  lion_hy_dist_cub)
      #*** NOTE: rank is during the same year as hyena lion intx. ***
      
    ## c) Rename variables
      lion_hy_dist <- lion_hy_dist %>%
        rename('stan.rank.lion' = 'stan.rank') 
  

  ### 5.4 Join lion_hy_dist with toxo_data      
    ## a) Join toxo_data (diagnosis, dart.date, etc.) to lion_hy_dist 
      lion_hy_dist_toxo <- lion_hy_dist %>%
        left_join(select(toxo_data, c(hy.id, hum.pop.den, diagnosis, dart.date, 
                                      toxo.status, age.mon.dart, 
                                      age.cat.dart, hum.dist.dob)), 
                  by = c('hy.id' = 'hy.id'))
      
    ## b) Generate a flag to indicate for every hyena lion interaction, 
      # if that interaction comes before or after diagnosis (darting date)
      lion_hy_dist_toxo$diagnosis.time <- 
        ifelse((lion_hy_dist_toxo$lion.date >=
                  lion_hy_dist_toxo$dart.date), 'after',
               (ifelse(is.na(lion_hy_dist_toxo$lion.date),
                       NA, 'before')))
      
    ## c) Remove lion data with no matching toxo diagnostic
      lion_hy_dist_toxo <- lion_hy_dist_toxo %>%
        filter(!is.na(toxo.status))
      
    ## d) Avoid misdiagnosed behaviors
      # Subset data to include only lion data before negative infection
      # and after postive infection
      lion_hy_dist_toxo_restrict <- lion_hy_dist_toxo %>%
        filter ((grepl('1', toxo.status) & 
                   grepl('after', diagnosis.time)) | 
                  (grepl('0', toxo.status) & 
                     grepl('before', diagnosis.time)))
      #*** NOTE *** This is a data inclusion cut-off decision.
      # ('doubtful' included with 'negative' diagnosis)
      
      
  ### 5.5 Subset lion_hy_dist_toxo data by age
    ## a) Stratify lion_hy_dist_toxo by age group making a cub data set 
      lion_hy_dist_toxo_cub_rpt <- lion_hy_dist_toxo  %>%
        filter(age.cat.lion == 'cub'& age.cat.dart == 'cub')
      
    ## b) Stratify lion_hy_dist_toxo by age group making a sub/adult data
      lion_hy_dist_toxo_sub_adult <- lion_hy_dist_toxo  %>%
        filter(age.cat.lion %in% c('subadult', 'adult') &
                 age.cat.dart %in% c('subadult', 'adult'))
      
    ## c) Stratify lion_hy_dist_toxo_restrict by age group making a 
      # sub/adult restricted data set (approach dist after dart for seropos...)
      lion_hy_dist_toxo_restrict_sub_adult <- lion_hy_dist_toxo_restrict  %>%
        filter(age.cat.lion %in% c('subadult', 'adult') &
                 age.cat.dart %in% c('subadult', 'adult'))
      
    
  ### 5.6 Summarize data
    ## a)  Summarize lion_hy_dist_toxo_cub_rpt becuse on only one animal has 
      # repeated measurements (i.e. take avg. approach distance 
      # reducing repeat distance measures to a single average value per hyena
      # per toxo diagnosis status instead of using a mixed model)
      lion_hy_dist_toxo_cub <- lion_hy_dist_toxo_cub_rpt %>%
          arrange(lion.date) %>%
          group_by (hy.id) %>% # set grouping same ID
          summarise (id.reps.lion = sum(!is.na(distance)), # n per hyena id
                     # closest hyena ever gets to lion
                     min.dist.lion = min(distance),
                     # max distance hyena ever gets to lion
                     max.dist.lion = max(distance),
                     # average of minimum approach distances
                     avg.min.dist.lion = round(mean(distance, na.rm = T), 2),
                     # use 'first' to retain other variables
                     #first.lion.intx.age.class = first(age.class),
                     age.mon.lion = round(mean(age.mon.lion),2),
                     age.cat.lion.first  = first(age.cat.lion),
                     age.cat.lion.last = last(age.cat.lion),
                     # mode or most frequent level for categorical variable
                     mode.age.cat.lion = names(table(age.cat.lion)
                                         [which.max(table(age.cat.lion))]),
                     # double check first and last lion hy intx before for neg
                     # and after for positive
                     diagnosis.time.first = first(diagnosis.time),
                     diagnosis.time.fin = last(diagnosis.time),
                     # mode or most frequent level for categorical variable
                     mode.fd.pres = names(table(food.present)
                                          [which.max(table(food.present))]),
                     avg.stan.rank = round(mean(stan.rank.lion),2))%>%
          ungroup()

    ## b) Join lion_hy_dist_cub_sum with cub_dist_sens
      lion_hy_dist_toxo_cub <- lion_hy_dist_toxo_cub %>%
        left_join(cub_dist_sens, 
                  by = c('hy.id' = 'hy.id'))
      
    ## c) Join lion_hy_dist_cub_sum to toxo_data
      lion_hy_dist_toxo_cub <- lion_hy_dist_toxo_cub %>%
        left_join(toxo_data, 
                  by = c('hy.id' = 'hy.id'))
          
    
        
###############################################################################
##############  6: Toxo Hyena Mortality: Join and re-tidy data   ##############
###############################################################################      
    
  ### 6.1 Subset / tidy luma_data_lf_hist for mortality analyses
    ## a) Subset the toxo data to include only hyenas which are known dead
      toxo_mort <- toxo_data  %>% 
        filter(!grepl('^\\s*$', disappeared.event.data) & 
                 !grepl('unk*|^double|infe*|disper*',
                        disappeared.event.data))
      
    ## b) Collapse mortality variable into binary form based on when 
      # hyena's die by lions vs other known source
      toxo_mort <- toxo_mort  %>% 
        mutate(mort.bin = ifelse(grepl('lion', disappeared.event.data), 1, 0)) 
      

      
###############################################################################
##############               7. Export data files                ##############
###############################################################################
      
  ### 7.1 Export data to an RData file
      # Files are saved in the 'data' folder in the working directory as an
      # RData file.
      
    ## a) Save and export tidy data tables for determinants of toxo analyses
      save(file = here('data/02_toxo_detrmnt_data.RData'),
           list = c('toxo_data',
                    'toxo_data_sub_adult',
                    'toxo_data_cub',
                    'toxo_data_sens'))
      
    ## b) Save and export tidy data tables for hyena toxo. and approach 
      # distance from lions analyses 
      save(file = here('data/03_toxo_hy_lion_dist_data.RData'),
           list = c('lion_hy_dist_toxo',
                    'lion_hy_dist_toxo_cub',
                    #'lion_hy_dist_toxo_cub_rpt',
                    'lion_hy_dist_toxo_sub_adult',
                    'lion_hy_dist_toxo_restrict_sub_adult'))
      
    ## c) Save and export tidy data tables for hyena toxo. cause of mortality
      # analyses
      save(file = here('data/06_toxo_hy_mort_data.RData'),
           list = c('toxo_mort'))
      
      

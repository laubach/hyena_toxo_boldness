###############################################################################
##############       Spotted Hyena Toxoplasmosis Analysis        ##############
##############         By: Zach Laubach and Eben Gerring         ##############
##############               created: 12 July 2018               ##############
##############            last modified: 13 Oct 2019             ##############
###############################################################################

### PURPOSE: This code is desingned to tidy and model Toxoplasma gondii  
### infection data in spotted hyeans from the Mara Hyena project.


  # Code Blocks
    # 1: Configure workspace
    # 2: Import data
    # 3: Data management: Tidy basic data tables 
    # 4: Data management: Join and re-tidy data
    # 5: Univariate analysis 
    # 6. Bivariate data exploration
    # 7: Data transformations 
    # 8. Bivariate analyses
    # 9: Determinants of T. gondii infection models
    # 10: Lions and hyena T. gondii infection models
    # 11: Hyena fitness and T. gondii infection models
    # 12: Export data files
  

###############################################################################
##############              1. Configure workspace               ##############
###############################################################################

  ### 1.1 Global options
    ## a) clear global environment
      rm(list = ls())

    ## b) prevent R from automatically reading charater strins as factors
      options(stringsAsFactors = FALSE)
  
      
  ### 1.2 Install and load Mara Hyena Project packages 
    ## a) Load the Mara Hyena Project data files from github
      # Check for devtools and install if not already installed
    #   if(!'devtools' %in% row.names(installed.packages())){
    #     install.packages('devtools')
    #   }
    # ## b) Use devtools to install hyeanadata package from the MaraHyenaProject
    #   # on github
    #   devtools::install_github('MaraHyenaProject/hyenadata',
    #                   auth_token = '')
    # load hyenadata package
      library('hyenadata')


  ### 1.3 Install and load CRAN packages
    ## a) Data Manipulation and Descriptive Stats Packages
      # Check for tidyverse and install if not already installed
        if (!'tidyverse' %in% installed.packages()[,1]){
          install.packages ('tidyverse')
        }
        # load tidyverse packages
        library ('tidyverse')
      
      # # Check for sqldf and install if not already installed
      #   if (!'sqldf' %in% installed.packages()[,1]){
      #     install.packages ('sqldf')
      #   }
      #   options(gsubfn.engine = 'R') #fixes tcltk bug; run before require sqldf
      # # load tidyverse packages
      #   library ('sqldf')
      # 
      # Check for lubridate and install if not already installed
        if (!'lubridate' %in% installed.packages()[,1]){
          install.packages ('lubridate')
        }
      # load lubridate packages
        library ('lubridate')
        
      # # Check for naniar and install if not already installed
      #   # used to replace values with NA
      #   if (!'naniar' %in% installed.packages()[,1]){
      #     install.packages ('naniar')
      #   }
      # # load naniar package
      #   library ('naniar') 
      
      # Check for here and install if not already installed
      if (!'here' %in% installed.packages()[,1]){
        install.packages ('here')
      }
      # load here packages
      library ('here')
      
    ## b) Graph Plotting and Visualization Packages
      # Check for ggplot2 and install if not already installed
        if (!'ggplot2' %in% installed.packages()[,1]){
          install.packages ('ggplot2')
        }
      # load ggplot2 packages
        library ('ggplot2')
    
      # Check for gridExtra and install if not already installed
        if (!'gridExtra' %in% installed.packages()[,1]){
          install.packages ('gridExtra')
        }
      # load gridExtra packages
        library ('gridExtra')
   
      # # Check for ggfortify and install if not already installed (graphing
      #   # survival, PCA, hierarchial models etc.)
      #   if (!'ggfortify' %in% installed.packages()[,1]){
      #     install.packages ('ggfortify')
      #   }
      # # load ggfortify packages
      #   library ('ggfortify')
        
        
      # Check for dotwhisker and install if not already installed
        # used with broom to graph beta estimates
        if (!'dotwhisker' %in% installed.packages()[,1]){
          install.packages ('dotwhisker')
        }
      # load dotwhisker packages
        library ('dotwhisker')
        
    ## c) Modeling Packages
      # Check for broom and install if not already installed
        if (!'broom' %in% installed.packages()[,1]){
          install.packages ('broom')
        }
      # load broom packages
        library ('broom')   
        
      # Check for broom.mixed and install if not already installed
      if (!'broom.mixed' %in% installed.packages()[,1]){
        install.packages ('broom.mixed')
      }
      # load broom.mixed packages
      library ('broom.mixed')   
      
      
      # Check for lmerTest and install if not already installed
#        if (!'lmerTest' %in% installed.packages()[,1]){
#          install.packages ('lmerTest')
#        }
      # load lmerTest packages
#        library ('lmerTest')   
      
      # Check for nlme and install if not already installed
#        if (!'nlme' %in% installed.packages()[,1]){
#          install.packages ('nlme')
#        }
      # load nlme packages
#        library ('nlme')
      
      # Check for lme4 and install if not already installed
        if (!'lme4' %in% installed.packages()[,1]){
          install.packages ('lme4')
        }
      # load lme4 packages
        library ('lme4')
        
      # Check for bbmle and install if not already installed
        if (!'bbmle' %in% installed.packages()[,1]){
          install.packages ('bbmle')
        }
      # load bbmle packages
        library ('bbmle')
        
      # Check for emmeans and install if not already installed
        if (!'emmeans' %in% installed.packages()[,1]){
          install.packages ('emmeans')
        }
      # load emmeans packages
        library ('emmeans')
      
      # Check for effects and install if not already installed
        if (!'effects' %in% installed.packages()[,1]){
          install.packages ('effects')
        }
      # load effects packages
        library ('effects')
        
      # Check for aod and install if not already installed
       if (!'aod' %in% installed.packages()[,1]){
         install.packages ('aod')
       }
      # load aod packages (used to for Wald test)
       library ('aod')
        
      # Check for car and install if not already installed
        if (!'car' %in% installed.packages()[,1]){
          install.packages ('car')
        }
      # load car packages (used for type II and type III SS test)
        library ('car')
        #options(contrasts = c('contr.sum', 'contr.poly'))
     
           
  ### 1.4 Get Version and Session Info
    R.Version()
    sessionInfo()
    
    # Developed in:   
    # R version 4.0.2 (2020-06-22)
    # Platform: x86_64-apple-darwin17.0 (64-bit)
    # Running under: macOS Catalina 10.15.7
    
    
  ### 1.5 Set working directory 
    setwd(here())

    
  ### 1.6 Set file paths for data importing and exporting
    
    ## a) Create path to toxo data folder
      toxo_data_path <- '~/Git/fisi_lab/hyena_toxo_boldness/data/'
    
    ## b) Create path to a toxo output folder
      toxo_data_out_path <- paste('Git/fisi_lab/hyena_toxo_boldness/output', 
                                  sep = '') 
    
    ## c) Source scripts path
      source_path <- paste('~/Git/source_code/')  
      
  
  ### 1.7 Source functions
    ## a) all_char_to_lower function
      source(file = paste0(source_path, 'all_char_to_lower.R'))
      
    ## b) format_var.names function
      source(file = paste0(source_path, 'format_var_names.R'))
      
    ## c) format_var.names_dash function
      source(file = paste0(source_path, 'format_var_names_dash.R'))  
    
    
  
    
###############################################################################
##############                  2. Import data                   ##############
###############################################################################    

  ### 2.1 Import toxo data files
    ## a) Import toxo_diagnostics data, which has undergone QAQC 
      toxo_data <- read_csv(paste(toxo_data_path,
                                  'toxo_diagnostics.csv', sep = ''))
      
      lion_hy_dist_toxo_cub_sum_sens <- read_csv(paste(toxo_data_path,
                                  'lion_hy_dist_toxo_cub_sum_sens.csv', 
                                  sep = ''))
      
    ## b) Import lion boldness
      lion_bold <- read_csv(paste(toxo_data_path,
                                  'lion_bold.csv', sep = ''))
      
    ## c) Import full lion distances data
      lion_hy_dist_raw <- read_csv(paste(toxo_data_path,
                                  'LHDistances.csv', sep = ''))  
      
  

  ### 2.2 Import Access Fisi data files
    ## a) Import Access data backend from Mara Hyena Project data package
      # Use hyenadata package 'load_all_tables' function to load all tables
      hyenadata::load_all_tables()
      
      
     
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
      
  # ***********DATA CLEANING*************
      # remove 'gil', kaycode 87, which has negative sp ratio 
      # and 'baj', kaycode 84, which is extreme outlier based on SPratio vs.
      # IFAT correlation
      toxo_data <- toxo_data %>%
        filter(kay.code != 87 & kay.code != 84)
      
           
  ### 3.2 Tidy tblHyenas
    ## a) rename data frame
      hyenas <- tblHyenas
      
    ## b) Convert all text to lower case
      hyenas <- AllCharactersToLower(hyenas)
      
    ## c) Format variable names (lowercase and separated by '.')
      hyenas <- FormatVarNames(hyenas)  
      
    ## d) Rename id as hy.id 
      hyenas <-  hyenas %>%
        rename('hy.id' = 'id') 
      
     
  ### 3.3 Tidy life_hist
    ## a) rename data frame
      life_hist <- tblLifeHistory.wide
      
    ## b) Convert all text to lower case
      life_hist <- AllCharactersToLower(life_hist)
      
    ## c) Format variable names (lowercase and separated by '.')
      life_hist <- FormatVarNames(life_hist) 
      
    ## d) Rename variables
      life_hist <- life_hist %>%
        rename('hy.id' = 'id') 
      
      life_hist <- life_hist %>%
        rename('dob.date' = 'dob')
      
      life_hist <- life_hist %>%
        rename('disappeared.date' = 'disappeared') 
      
    ## e) convert hy.id to character class  
      life_hist$hy.id <- as.character(life_hist$hy.id) 
      

  ### 3.4 Tidy tblFemaleRanks
    ## a) rename data frame
      ranks <- tblFemaleRanks
      
    ## b) Convert all text to lower case
      ranks <- AllCharactersToLower(ranks)
      
    ## c) Format variable names (lowercase and separated by '.')
      ranks <- FormatVarNames(ranks) 
      
    ## d) convert characters to integer
      ranks$rank <- as.integer(ranks$rank)
    
    ## e) Rename variables
      ranks <- ranks %>%
        rename('rank.year' = 'year')
      
      ranks <- ranks %>%
        rename('hy.id' = 'id')
      
      ranks <- ranks %>%
        rename('rank.clan' = 'clan')
    
    ## f) convert rank.year to numeric
      ranks$rank.year <- as.numeric(as.character(ranks$rank.year))
      
    ## g) convert stan.rank to numeric
      ranks$stan.rank <- as.numeric(as.character(ranks$stan.rank))
 
      
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
        left_join(select(tblSessions, c(clan, session, location, date, 
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
    ## a) Subset data where distance is a range (need manually checked)  
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
      
    ## g) Determine the mean of min and max dist
      lion_hy_dist_range <- lion_hy_dist_range %>%
        mutate(distance = (max.dist + min.dist)/2)
      
    ## h) Drop extra columns
      lion_hy_dist_range <- lion_hy_dist_range %>%
        select(-c(min.dist, max.dist, dist.dif))
       
    ## i) Subset data where distance is an inequality (need manually checked)  
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
    ## f) Exclude data where hyean distances greater 200m
      lion_hy_dist <- lion_hy_dist %>%
        filter (distance <= 100)  
      #*** NOTE *** This is a data inclusion cut-off decision. 
          # 200m is typically used as a sessioning cut-off and but 100m is 
          # likely limit of accurate distance estimation
      
  ### 3.9 Clean global environment
    ## a) Remove extra tables/dataframes
      rm(list = ls(pattern = 'tbl'))
      
      
      
###############################################################################
##############     4: Data management: Join and re-tidy data     ##############
###############################################################################      

#***************************  Determinants of Toxo  **************************** 
  ### 4.1 Join access fisi backend tables to toxo_data
    ## a) Left join life_hist to toxo_data,
      toxo_data <- toxo_data %>% 
        left_join(select(life_hist, c(hy.id, dob.date, dob.event.data, 
                                      disappeared.date, disappeared.event.status,
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
                                                diagnosis == 'doubtful', 0, 1)))
      
      toxo_data_sub_adult <- toxo_data_sub_adult %>%
        mutate(toxo.status = as.factor(ifelse(diagnosis == 'negative' |
                                                diagnosis == 'doubtful', 0, 1)))
      
      toxo_data_cub <- toxo_data_cub %>%
        mutate(toxo.status = as.factor(ifelse(diagnosis == 'negative' |
                                                diagnosis == 'doubtful', 0, 1)))
      
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
      
      
  ### 4.3 Join         
    ## a)  Left Join ranks to toxo_data (subs and adults then cubs)
      toxo_data_sub_adult <- toxo_data %>%
        filter(age.cat.dart == 'adult' | age.cat.dart == 'subadult') %>%
        left_join(select(ranks, c(hy.id, rank, stan.rank, rank.year)),
                  by = c('hy.id' = 'hy.id', 'dart.year' = 'rank.year'))
      toxo_data_cub <- toxo_data %>%
        filter(age.cat.dart == 'cub') %>%
        left_join(select(ranks, c(hy.id, rank, stan.rank, rank.year)), 
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
      
   
#**************************  Toxo Lion Hyena Dist.  ****************************  
    
  ### 4.4 Join hyenas, life_hist, and ranks tables to lion_hy_dist
    ## a) Join hyenas id, sex, etc. to lion_hy_dist
      lion_hy_dist <- lion_hy_dist %>%
        left_join(select(hyenas, c(hy.id, sex, status, mom, dad, 
                                   number.littermates, litrank)), 
                  by = c('hy.id' = 'hy.id'))
      
    ## b) Join life_hist to lion_hy_dist
      lion_hy_dist <- lion_hy_dist %>%
        left_join(life_hist, by = c('hy.id' = 'hy.id'))
      
      
  ### 4.5 Tidy lion_hy_dist and prepare data for analysis   
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
      
    ## c) replace NA in age.cat.lion column where there is a value in 
      # age column
      # first convert age to character
      lion_hy_dist$age.cat.lion <- as.character(lion_hy_dist$age.cat.lion)
      lion_hy_dist$age.cat.lion <- ifelse(is.na(lion_hy_dist$age.cat.lion),
                                       'adult',
                                       lion_hy_dist$age.cat.lion)
      
    ## d) Reorder age variable for graphing
      lion_hy_dist$age.cat.lion <- factor(lion_hy_dist$age.cat.lion, 
                                       levels = c('cub', 'subadult', 'adult'))
      
    ## e) Extract year from lion interaction date
      # Use lubridate to extract the year during which a hyena was born 
      # and make a new variable  
      lion_hy_dist$lion.yr <- year(lion_hy_dist$lion.date)
      
    ## f) HACK: remove hyena lion interactions in which animal is negative
      # years old on date of interaction
      # Includes:
        # hyena KS, sessions s3620 and s5891.1
        # hyena MRPH, sessions t17.1 and t53.5
      lion_hy_dist <- lion_hy_dist %>%
        filter(age.mon.lion >= 0 | is.na(age.mon.lion))
      
    ## f) Make a new human disturbance variable
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
      
  ### 4.6 Join lion_hy_dist with toxo_data   
    ## a)  Left Join ranks to lion_hy_dist (subs and adults then cubs)
      lion_hy_dist_sub_adult <- lion_hy_dist %>%
        filter(age.cat.lion == 'adult' | age.cat.lion == 'subadult') %>%
        left_join(select(ranks, c(hy.id, rank, stan.rank, rank.year)),
                  by = c('hy.id' = 'hy.id', 'lion.yr' = 'rank.year'))
      lion_hy_dist_cub <- lion_hy_dist %>%
        filter(age.cat.lion == 'cub') %>%
        left_join(select(ranks, c(hy.id, rank, stan.rank, rank.year)), 
                  by = c('mom' = 'hy.id', 'lion.yr' = 'rank.year'))
      
      ## b) combine age specific data sets
      lion_hy_dist <- rbind(lion_hy_dist_sub_adult,  lion_hy_dist_cub)
      #*** NOTE: rank is during the same year as hyena lion intx. ***
      
      ## c) Rename variables
      lion_hy_dist <- lion_hy_dist %>%
        rename('stan.rank.lion' = 'stan.rank') 
  

  ### 4.7 Join lion_hy_dist with toxo_data      
    ## a) Join toxo_data diagnosis dart.date to lion_hy_dist as new dataframe
      lion_hy_dist_toxo <- lion_hy_dist %>%
        left_join(select(toxo_data, c(hy.id, hum.pop.den, diagnosis, dart.date, 
                                      age.mon.dart, age.cat.dart, hum.dist.dob)), 
                  by = c('hy.id' = 'hy.id'))
      
    ## b) Generate a flag to indicate for every hyena lion interaction, 
      # if that interaction comes before or after diagnosis (darting date)
      lion_hy_dist_toxo$diagnosis.time <- 
        ifelse((lion_hy_dist_toxo$lion.date >=
                  lion_hy_dist_toxo$dart.date), 'after',
               (ifelse(is.na(lion_hy_dist_toxo$lion.date),
                       NA, 'before')))
      
    ## c) Create a binary factor for diagnosis as toxo_status
      lion_hy_dist_toxo <- lion_hy_dist_toxo %>%
        mutate(toxo.status = as.factor(ifelse(diagnosis == 'negative' | 
                                                diagnosis == 'doubtful', 
                                              0, 1))) 
      
    ## d) Remove lion data with no matching toxo diagnostic
      lion_hy_dist_toxo <- lion_hy_dist_toxo %>%
        filter(!is.na(toxo.status))
      
#********************** Data Inclusion/Exclusion Criteria **********************      
    ## e) Remove lion hyena sessions during which no interaction occured 
      lion_hy_dist_toxo <- lion_hy_dist_toxo %>%
        filter(no.intx.yes.no == 'FALSE')
      #*** NOTE *** This is a data inclusion cut-off decision.
     
#********************** Data Inclusion/Exclusion Criteria **********************        
    ## f) Avoid misdiagnosed behaviors
      # Subset data to include only lion data before negative infection
      # and after postive infection
      lion_hy_dist_toxo_restrict <- lion_hy_dist_toxo %>%
        filter ((grepl('1', toxo.status) & 
                   grepl('after', diagnosis.time)) | 
                  (grepl('0', toxo.status) & 
                     grepl('before', diagnosis.time)))
      #*** NOTE *** This is a data inclusion cut-off decision.
      # ('doubtful' included with 'negative' diagnosis)
      
  
## ****** NOTE: After determing significant age structure in hyena approach 
      # distances towards lions, we age standardized distances. Section 4.7
      # of this code (below) used to be section 7.2 but was move here
      
  ### 4.7 (previously 7.2) Age (at hyena lion interaction) z-score 
      # standardization of approach distances
      
      #** NOTE use restricted data set to avoid inaccurate paring of infection
      # status with approach distances
      # steps 1) age standardize approach distances 2) restrict data, 
      # 3) extract subadult and adult (lion age), 4 summarize/avg stndrd dist.
      
    ## a) Age standardized distance for lion_hy_dist
      lion_hy_dist <- lion_hy_dist  %>%
        group_by(age.cat.lion) %>%
        mutate(dist.age.stndrzd = scale(distance))%>%
        ungroup() # ungroup the data frame.
      
      #NOTE: check that scaling occurs within category of lion age  
      # lion_hy_dist_cub <- filter(lion_hy_dist, age.cat.lion == 'cub')
      # lion_hy_dist_cub <- lion_hy_dist_cub  %>%
      #   mutate(age.stndrdz.lion = scale(lion_hy_dist_cub[ ,'distance']))
      
    ## b) Age standardized distance for lion_hy_dist_toxo
      lion_hy_dist_toxo <- lion_hy_dist_toxo  %>%
        group_by(age.cat.lion) %>%
        mutate(dist.age.stndrzd = scale(distance))%>%
        ungroup() # ungroup the data frame.
      
    ## c) Age standardized distance for lion_hy_dist_toxo_restrict
      lion_hy_dist_toxo_restrict <- lion_hy_dist_toxo_restrict  %>%
        group_by(age.cat.lion) %>%
        mutate(dist.age.stndrzd = scale(distance))%>%
        ungroup() # ungroup the data frame. 
    
    ## d) Stratify lion_hy_dist_toxo by age group making a cub data set 
      # and sub/adult
      lion_hy_dist_toxo_cub <- lion_hy_dist_toxo  %>%
        filter(age.cat.lion == 'cub'& age.cat.dart == 'cub')
      
    ## e) Stratify lion_hy_dist_toxo by age group making a sub/adult data
      lion_hy_dist_toxo_sub_adult <- lion_hy_dist_toxo  %>%
        filter(age.cat.lion %in% c('subadult', 'adult') &
                 age.cat.dart %in% c('subadult', 'adult'))
      
    ## f) Stratify lion_hy_dist_toxo_restrict by age group making a 
      # sub/adult restricted data set (approach dist after dart for seropos...)
      lion_hy_dist_toxo_restrict_sub_adult <- lion_hy_dist_toxo_restrict  %>%
        filter(age.cat.lion %in% c('subadult', 'adult') &
                 age.cat.dart %in% c('subadult', 'adult'))

      
  ### 4.8 Summarize lion_hy_dist for toxo analyses
    ## a)  Summarize lion_hy_dist_toxo
      # reduce repeat distance measures to a single average value per hyena
      # per toxo diagnosis status
      lion_hy_dist_sum <- lion_hy_dist_toxo %>%
        arrange(lion.date) %>%
        group_by (hy.id) %>% # set grouping same
        summarise (id.reps.lion = sum(!is.na(distance)), # n per hyena id
                   # closest hyena ever gets to lion
                   min.dist.lion = min(distance),
                   # max distance hyena ever gets to lion
                   max.dist.lion = max(distance),
                   # average of minimum approach distances
                   avg.min.dist.lion = round(mean(distance, na.rm = T), 2),
                   # average of the rank.dist/totWdist per hyena
#                   std.rank.min.dist.lion = round(mean((rank.dist/tot.wdist),
#                                                       na.rm = T), 2),
                   avg.dist.age.stndrzd = round(mean(dist.age.stndrzd,
                                                     na.rm = T), 2),
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
      
    ## b)  Summarize lion_hy_dist_toxo_cub
      # reduce repeat distance measures to a single average value per hyena
      # per toxo diagnosis status 
      lion_hy_dist_cub_sum <- lion_hy_dist_toxo_cub %>%
        arrange(lion.date) %>%
        group_by (hy.id) %>% # set grouping same ID 
        summarise (id.reps.lion = sum(!is.na(distance)), # n per hyena id  
                   # closest hyena ever gets to lion
                   min.dist.lion = min(distance), 
                   # max distance hyena ever gets to lion
                   max.dist.lion = max(distance), 
                   # average of minimum approach distances
                   avg.min.dist.lion = round(mean(distance, na.rm = T), 2),
                   avg.dist.age.stndrzd = round(mean(dist.age.stndrzd, 
                                                     na.rm = T), 2),
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
      
    # ## c)  Summarize lion_hy_dist_toxo_sub_adult
    #   # reduce repeat distance measures to a single average value per hyena
    #   # per toxo diagnosis status 
      lion_hy_dist_sub_adult_sum <-
        lion_hy_dist_toxo_sub_adult %>%
        arrange(lion.date) %>%
        group_by (hy.id) %>% # set grouping same ID
        summarise (id.reps.lion = sum(!is.na(distance)), # n per hyena id
                   # closest hyena ever gets to lion
                   min.dist.lion = min(distance),
                   # max distance hyena ever gets to lion
                   max.dist.lion = max(distance),
                   # average of minimum approach distances
                   avg.min.dist.lion = round(mean(distance, na.rm = T), 2),
                   # average of the rank.dist/totWdist per hyena
                   #  std.rank.min.dist.lion = round(mean((rank.dist/tot.wdist),
                   #                                             na.rm = T), 2),
                   avg.dist.age.stndrzd = round(mean(dist.age.stndrzd,
                                                     na.rm = T), 2),
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
      
#     ## d)  Summarize lion_hy_dist_restrict
#       # reduce repeat distance measures to a single average value per hyena
#       # per toxo diagnosis status
#       lion_hy_dist_restrict_sum <- lion_hy_dist_toxo_restrict %>%
#         arrange(lion.date) %>%
#         group_by (toxo.status, hy.id) %>% # set grouping same ID w/in toxo diag
#         summarise (id.reps.lion = sum(!is.na(distance)), # n per hyena id
#                    # closest hyena ever gets to lion
#                    min.dist.lion = min(distance),
#                    # max distance hyena ever gets to lion
#                    max.dist.lion = max(distance),
#                    # average of minimum approach distances
#                    avg.min.dist.lion = round(mean(distance, na.rm = T), 2),
#                    # average of the rank.dist/totWdist per hyena
# #                   std.rank.min.dist.lion = round(mean((rank.dist/tot.wdist),
# #                                                       na.rm = T), 2),
#                    avg.dist.age.stndrzd = round(mean(dist.age.stndrzd,
#                                                      na.rm = T), 2),
#                    # use 'first' to retain other variables
#                    #first.lion.intx.age.class = first(age.class),
#                    age.mon.lion = round(mean(age.mon.lion),2),
#                    age.cat.lion.first  = first(age.cat.lion),
#                    age.cat.lion.last = last(age.cat.lion),
#                    # mode or most frequent level for categorical variable
#                    mode.age.cat.lion = names(table(age.cat.lion)
#                                              [which.max(table(age.cat.lion))]),
#                    # double check first and last lion hy intx before for neg
#                    # and after for positive
#                    diagnosis.time.first = first(diagnosis.time),
#                    diagnosis.time.fin = last(diagnosis.time),
#                    # mode or most frequent level for categorical variable
#                    mode.fd.pres = names(table(food.present)
#                                         [which.max(table(food.present))]),
#                    avg.stan.rank = round(mean(stan.rank.lion),2))%>%
#         ungroup()
#       
      
  ### 4.8 Join toxo_data with lion_hy_dist_sum
    ## a) Join lion_hy_dist_sum to toxo_data
      lion_hy_dist_toxo_sum <- toxo_data %>%
        left_join(lion_hy_dist_sum,
                  by = c('hy.id' = 'hy.id'))
    #   
    # ## b) Reorder age variable for graphing
    #   lion_hy_dist_toxo_sum$mode.age.cat.lion <- 
    #     factor(lion_hy_dist_toxo_sum$mode.age.cat.lion, 
    #            levels = c('cub', 'subadult', 'adult'))
      
    ## c) Join lion_hy_dist_cub_sum to toxo_data
      lion_hy_dist_toxo_cub_sum <- lion_hy_dist_cub_sum %>%
        left_join(toxo_data, 
                  by = c('hy.id' = 'hy.id'))
      
    # ## e) Join lion_hy_dist_sum to toxo_data
    #   lion_hy_dist_toxo_sub_adult_sum <- 
    #     lion_hy_dist_sub_adult_sum %>%
    #     left_join(toxo_data, 
    #               by = c('hy.id' = 'hy.id'))
    #   
    # ## f) Reorder age variable for graphing
    #   lion_hy_dist_toxo_sub_adult_sum$mode.age.cat.lion <- 
    #     factor(lion_hy_dist_toxo_sub_adult_sum$mode.age.cat.lion, 
    #            levels = c('cub', 'subadult', 'adult'))
    #   
    # ## g) Join lion_hy_dist_restrict_sum to toxo_data
    #   lion_hy_dist_toxo_restrict_sum <- toxo_data %>%
    #     left_join(lion_hy_dist_restrict_sum,
    #               by = c('hy.id' = 'hy.id'))
    # 
    # ## h) Reorder age variable for graphing
    #   lion_hy_dist_toxo_restrict_sum$mode.age.cat.lion <-
    #     factor(lion_hy_dist_toxo_restrict_sum$mode.age.cat.lion,
    #            levels = c('cub', 'subadult', 'adult'))
    #   
    # ## c) Join lion_hy_dist_no_cub to toxo_data
    #   lion_hy_dist_no_fd_toxo <- toxo_data %>%
    #     left_join(select(lion_hy_dist_no_fd, -c(toxo.status)), 
    #               by = c('hy.id' = 'hy.id'))
    #   
    # ## d) Join lion_hy_dist_no_cub to toxo_data
    #   lion_hy_dist_no_cub_toxo <- toxo_data %>%
    #     left_join(select(lion_hy_dist_no_cub, -c(toxo.status)), 
    #               by = c('hy.id' = 'hy.id'))
      
      
#***************************  Toxo Hyena Survival  *****************************      
    
  ### 4.9 Subset /tidy luma_data_lf_hist for mortality analyses
    # ## a) Create variable to include data when a hyena's disappearance is 
    #   # known or still alive
    #   toxo_mort_all <- toxo_data  %>% 
    #     mutate(mortality = case_when(grepl('unk*|^double', 
    #                                         disappeared.event.data)  
    #                                  ~ c('unknown'),
    #                                  grepl('lion', disappeared.event.data)
    #                                  ~ c('dead.lion'),
    #                                  (grepl('dead', disappeared.event.status) &
    #                                    !grepl('lion', disappeared.event.data)) |
    #                                    grepl('inferred dead', 
    #                                          disappeared.event.data) |
    #                                    grepl('human', 
    #                                          disappeared.event.data)
    #                                  ~ c('dead.other')))
    #   
    # ## b) Collapse mortality variable into binary form based on when 
    #   # hyena's disappearance is known or still alive
    #   toxo_mort_all <- toxo_mort_all  %>% 
    #     mutate(mort.bin = case_when(grepl('other', 
    #                                        mortality) | grepl('unknown',
    #                                                           mortality) 
    #                                  ~ 0,
    #                                  grepl('lion', mortality)
    #                                  ~ 1))
      
    ## c) Subset the toxo data to include only hyenas which are known dead
      toxo_mort <- toxo_data  %>% 
        filter(!grepl('^\\s*$', disappeared.event.data) & 
                 !grepl('unk*|^double|infe*|disper*',
                        disappeared.event.data))
      
    ## d) Collapse mortality variable into binary form based on when 
      # hyena's die by lions vs other known source
      toxo_mort <- toxo_mort  %>% 
        mutate(mort.bin = ifelse(grepl('lion', disappeared.event.data), 1, 0)) 
      
      
  ### 4.10 Clean global environment
    ## a) Remove extra tables/dataframes 
      rm(list = setdiff(ls(), c('toxo_data',
                                'toxo_data_sub_adult',
                                'toxo_data_cub',
                                'lion_hy_dist_toxo',
                                'lion_hy_dist_toxo_sum',
                                'lion_hy_dist_toxo_cub_sum',
                                'lion_hy_dist_toxo_restrict_sub_adult',
                                'lion_hy_dist_toxo_sub_adult',
                                'lion_hy_dist_toxo_restrict',
                                'toxo_mort')))
      #rm(list = ls(pattern = 'data'))
      #rm(female_ranks, hyenas, life_hist, repro_state)
      
      
      
###############################################################################
##############              5. Univariate analysis               ##############
###############################################################################

                          # Organization of section 5      
#***************************  Determinants of Toxo  ****************************
#***************************  Toxo Lion Hyena Dist. ****************************

      
#***************************  Determinants of Toxo  ****************************
  ### 5.1 Univariate stats toxo 
    
    ## a) Descriptive stats toxo status
      univar_toxo_stat <- toxo_data %>%
        group_by(toxo.status) %>%
        summarise(n.status = sum(!is.na(toxo.status))) %>%
        mutate(freq = n.status / sum(n.status, na.rm = T))
        
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/univar_toxo_status.pdf'), height = 4, width = 8)
      grid.table(univar_toxo_stat)
      dev.off()
    
      
#**************************  Toxo Lion Hyena Dist.  ****************************  
      
  ### 5.2 Univariate stats avg minimum approach towards lions 
    ## a) Descriptive stats hyena min approach distances towards lions 
      # when data are restricted (before seronegative, after seropositive)
      # calculate the mean, median and standard dev. of lion hy intx. ratios
      # Univariate descriptive stats (with no grouping) 
    #   univar_min_app_dist_restrict <- lion_hy_dist_toxo_restrict_sum %>%
    #     summarize(n.app.dist = sum(!is.na(avg.min.dist.lion)),
    #               avg.app.dist = round(mean
    #                                      (avg.min.dist.lion, 
    #                                        na.rm = T), 2),
    #               median.app.dist =  round(quantile(avg.min.dist.lion, 
    #                                                   c(.5), na.rm = T), 2),
    #               sd.app.dist = round(sd(avg.min.dist.lion, 
    #                                        na.rm = T), 2))
    #   
    # ## b) save the data frame of summary stats out as a pdf into output file
    #   pdf(here('output/univar_min_app_dist.pdf'), height = 3, width = 8)
    #   grid.table(univar_min_app_dist_restrict)
    #   dev.off()
    #   
    ## c) Descriptive stats hyena min approach distances towards lions
      # full data set
      # calculate the mean, median and standard dev. of lion hy intx. ratios
      # Univariate descriptive stats (with no grouping)  
      univar_min_app_dist_sum <- lion_hy_dist_toxo_sum %>%
        summarize(n.app.dist = sum(!is.na(avg.min.dist.lion)),
                  avg.app.dist = round(mean
                                       (avg.min.dist.lion, 
                                         na.rm = T), 2),
                  median.app.dist =  round(quantile(avg.min.dist.lion, 
                                                    c(.5), na.rm = T), 2),
                  sd.app.dist = round(sd(avg.min.dist.lion, 
                                         na.rm = T), 2))
      
    # ## d) Descriptive stats hyena min approach distances towards lions
    #   # repeat data set
    #   # calculate the mean, median and standard dev. of lion hy intx. ratios
    #   # Univariate descriptive stats (with no grouping)  
    #   univar_min_app_dist <- lion_hy_dist_toxo %>%
    #     summarize(n.app.dist = sum(!is.na(distance)),
    #               avg.app.dist = round(mean
    #                                    (distance, 
    #                                      na.rm = T), 2),
    #               median.app.dist =  round(quantile(distance, 
    #                                                 c(.5), na.rm = T), 2),
    #               sd.app.dist = round(sd(distance, 
    #                                      na.rm = T), 2))
    # ## e) save the data frame of summary stats out as a pdf into output file
    #   pdf(here('output/univar_min_app_dist.pdf'), height = 3, width = 8)
    #   grid.table(univar_min_app_dist)
    #   dev.off()
      
    ## e) Histogram Outcome (avg minimum approach distance)
      ggplot(data=lion_hy_dist_toxo_sum, aes(x=avg.min.dist.lion)) + 
        geom_histogram(aes(y = ..count..),
                       breaks=seq(0, 300, by = 10), 
                       col='black',
                       fill = 'dark grey') +
        xlim(c(0,300)) +
        labs(title = 'Histogram of hyena average minimum 
             approach distance towards lions') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        labs(x='Avg. Min. Approach Dist. (m)', y='Frequency') 
      
    ## f) Save Plot
      # use ggsave to save the plot
      ggsave('life_min_app_dist_histogram.pdf', plot = last_plot(), 
             device = NULL, 
             path = here('output/'), scale = 1, width = 5, 
             height = 3, 
             units = c('in'), dpi = 300, limitsize = TRUE)   


       
###############################################################################
##############            6. Bivariate data exploration          ##############
###############################################################################      

      
#***************************  Determinants of Toxo  ****************************  
  
  ### 6.1 Descriptive bivariate stats toxo status by sex
    ## a) Sex ratio summary 
      sex_ratio_summary <- toxo_data %>%
        #group_by (id) %>%
        group_by (sex, toxo.status) %>%
        summarise(n=n_distinct(hy.id)) %>%
        mutate(freq = n / sum(n))
      
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/sex_ratio_summary.pdf'), 
          height = 3, width = 5)
      grid.table(sex_ratio_summary)
      dev.off() 
      
      
  ### 6.2 Descriptive bivariate stats toxo by age.mon.dart    
    ## a) Age summary  
      age_var_summary <- toxo_data %>%
        group_by (age.cat.dart) %>%
        summarise (n.age.dart = sum(!is.na(age.mon.dart)),
                   avg.age.dart = round (mean(age.mon.dart, na.rm = T),2),
                   stdev.age.dart = round (sd(age.mon.dart, na.rm = T), 2)) %>%
        mutate (freq_age =  (n.age.dart / sum(n.age.dart)))
      
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/age_var_summary.pdf'), 
          height = 4, width = 5)
      grid.table(age_var_summary)
      dev.off() 
      
    ## c) Age categories summary 
      age_cat_summary <- toxo_data %>%
        #group_by (id) %>%
        group_by (age.cat.dart, toxo.status) %>%
        summarise(n=n_distinct(hy.id)) %>%
        mutate(freq = n / sum(n))
      
    ## d) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/age_cat_summary.pdf'), 
          height = 3, width = 5)
      grid.table(sex_ratio_summary)
      dev.off() 
      
   
    ## e) Plot age at darting by toxo status
      # graph of raw data for spratio by age
      ggplot(data = toxo_data,
             aes (x = toxo.status, y = age.mon.dart,
                  color = toxo.status)) +
        geom_boxplot() +
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs (title = 'Age at diagnosis by toxo status') +
        ylab ('Age at diagnosis (mon)') +
        xlab ('Toxo status (0 = uninfected)')
      
    ## f) Save Plot
      # use ggsave to save the boxplot
      ggsave('age_dart_by_toxo_plot.pdf', plot = last_plot(), device = NULL, 
             path = here('output/'), scale = 1, width = 7, 
             height = 5, 
             units = c('in'), dpi = 300, limitsize = TRUE)  
      
      
 ### 6.3 Descriptive bivariate stats toxo by rank
    ## a) rank summary
      rank_summary <- toxo_data %>%
        group_by  (toxo.status) %>%
        summarise (n.rank =  sum(!is.na(stan.rank.dart)),
                   #n_rank = n(), # n including na
                   avg.rank = round (mean(stan.rank.dart, na.rm = T),2),
                   stdev.rank = round (sd(stan.rank.dart, na.rm = T), 2))

    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/rank_summary.pdf'),
          height = 4, width = 8)
      grid.table(rank_summary)
      dev.off()
      
    ## c) rank summary among adult females
      rank_summary_sub_adult <- toxo_data_sub_adult %>%
        group_by  (toxo.status) %>%
        summarise (n.rank =  sum(!is.na(stan.rank.dart)),
                   #n_rank = n(), # n including na
                   avg.rank = round (mean(stan.rank.dart, na.rm = T),2),
                   stdev.rank = round (sd(stan.rank.dart, na.rm = T), 2))
      
    ## d) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/rank_summary_sub_adult.pdf'),
          height = 4, width = 8)
      grid.table(rank_summary_sub_adult)
      dev.off()
      
    ## e) rank summary among cubs
      rank_summary_cub <- toxo_data_cub %>%
        group_by  (toxo.status) %>%
        summarise (n.rank =  sum(!is.na(stan.rank.dart)),
                   #n_rank = n(), # n including na
                   avg.rank = round (mean(stan.rank.dart, na.rm = T),2),
                   stdev.rank = round (sd(stan.rank.dart, na.rm = T), 2))
      
     ## f) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/rank_summary_cub.pdf'),
          height = 4, width = 8)
      grid.table(rank_summary_cub)
      dev.off()
      

    
  ### 6.4 Descriptive bivariate stats toxo by human pop size
    ## a) Human pop size (proxy) summary 
      hum_pop_summary <- toxo_data %>%
        #group_by (id) %>%
        group_by (hum.pop.den, toxo.status) %>%
        summarise(n=n_distinct(hy.id)) %>%
        mutate(freq = n / sum(n))
      
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/hum_pop_summary.pdf'), 
          height = 3, width = 5)
      grid.table(hum_pop_summary)
      dev.off() 
      
   
      
##******************* Determinants hyena bevavior w/ lions *******************##  
  
  ### 6.5 Bivariate descriptive stats for approach distances
      
      # NOTE: The data lion_hy_dist_toxo_restrict_sum, contains data that are 
      # restricted (before seronegative, after seropositive), the data
      # 'lion_hy_dist_toxo_sum' is the full data set including any approach 
      # lion distance for diagnosed hyenas, and the data 'lion_hy_dist_repeat' 
      # contains all lion-hyena distances with repeated samples for the 
      # same inividuals
      
    ## a) Bivariate descriptive stats for approach distance by sex
      # Use filter version if you have missing data for grouping variable; 'sex'
      # calculate n, mean, median and standard deviation grouped by sex
      bivar_dist_by_sex_sum <- lion_hy_dist_toxo_sum %>%
        #filter(sex == 'm' | sex == 'f') %>%
        group_by(sex) %>%
        summarize (n.app.dist = sum(!is.na(avg.min.dist.lion)),
                   avg.app.dist = round(mean(avg.min.dist.lion, na.rm = T), 2),
                   median.app.dist =  round(quantile(avg.min.dist.lion, c(.5),
                                                     na.rm = T), 2),
                   sd.app.dist = round(sd(avg.min.dist.lion, na.rm = T), 2))

    # ## b) save the data frame of summary stats out as a pdf into output file
    #   pdf(here('output/dist_by_sex_summary.pdf'),
    #       height = 3, width = 7)
    #   grid.table(bivar_dist_by_sex_sum)
    #   dev.off()
# 
#       
#     ## c) Bivariate descriptive stats for approach distance by sex
#       # when data are restricted (before seronegative, after seropositive)
#       # Use filter version if you have missing data for grouping variable; 'sex'
#       # calculate n, mean, median and standard deviation grouped by sex
#       bivar_dist_by_sex_restrict <- lion_hy_dist_toxo_restrict_sum %>%
#         #filter(sex == 'm' | sex == 'f') %>%
#         group_by(sex) %>%
#         summarize (n.app.dist = sum(!is.na(avg.min.dist.lion)),
#                    avg.app.dist = round(mean(avg.min.dist.lion, na.rm = T), 2),
#                    median.app.dist =  round(quantile(avg.min.dist.lion, c(.5), 
#                                                      na.rm = T), 2),
#                    sd.app.dist = round(sd(avg.min.dist.lion, na.rm = T), 2))
#       
#     ## d) Bivariate descriptive stats for approach distance by sex
#       # when data not limited by toxo diagnosis and pseudo replicated
#       # Use filter version if you have missing data for grouping variable; 'sex'
#       # calculate n, mean, median and standard deviation grouped by sex
#       bivar_dist_by_sex <- lion_hy_dist_toxo %>%
#         #filter(sex == 'm' | sex == 'f') %>%
#         group_by(sex) %>%
#         summarize (n.app.dist = sum(!is.na(distance)),
#                    avg.app.dist = round(mean(distance, na.rm = T), 2),
#                    median.app.dist =  round(quantile(distance, c(.5), 
#                                                      na.rm = T), 2),
#                    sd.app.dist = round(sd(distance, na.rm = T), 2))
#       
#       # when data are restricted (before seronegative, after seropositive)
#       
#       
    ## c) Plot approach distance by sex
      # NOTE: These summaries contain non-independent measures of app distances
      ggplot(data = subset(lion_hy_dist_toxo, !is.na(x = sex)),

#   **************** Boxplot START ****************
             aes(x = sex, y = distance,
                 color = sex)) +
        geom_boxplot() +
        geom_point(position=position_dodge(width=0.0),
                   aes(color=toxo.status), size = 1)+
#   ***************** Boxplot END *****************
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = 'Hyena avg. minumum apporach distance towards lions by sex',
             subtitle = 'Data are pseudoreplicated. Red pts = unifected, green pts = infected.') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) +
        theme(legend.position = 'none') + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'darkgrey',
                                       size = 1, linetype = 'solid')) +
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face='bold', color='black',
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0,
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black',
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0,
                                                         b = 0, l = 10))) +
        ylab('Avg. min. approach dist. (m)') +
        xlab('Hyena sex')

    ## d) Save Plot
      # use ggsave to save the plot
      ggsave('app_dist_by_sex_plot.pdf', plot = last_plot(), device = NULL,
             path = paste0(here(),'/output'),
             scale = 1, width = 11, height = 7,
             units = c('in'), dpi = 300, limitsize = TRUE)
#       
#     ## e) Bivariate descriptive stats for approach distance by age.cat.lion
#       # calculate n, mean, median and standard deviation grouped by age.
#       bivar_dist_by_age_toxo <- lion_hy_dist_toxo_sum %>%
#         group_by(age.cat.lion.first) %>%
#         summarize (n.app.dist = sum(!is.na(avg.min.dist.lion)),
#                    avg.app.dist = round(mean(avg.min.dist.lion, na.rm = T), 2),
#                    median.app.dist =  round(quantile(avg.min.dist.lion, c(.5), 
#                                                      na.rm = T), 2),
#                    sd.app.dist = round(sd(avg.min.dist.lion, na.rm = T), 2))
#       
#     ## f) save the data frame of summary stats out as a pdf into output file
#       pdf(here('output/dist_by_age_summary.pdf'), 
#           height = 3, width = 7)
#       grid.table(bivar_dist_by_age_toxo)
#       dev.off() 
#       
#     ## g) Bivariate descriptive stats for approach distance by age
#       # when data are restricted (before seronegative, after seropositive)
#       # calculate n, mean, median and standard deviation grouped by age
#       bivar_dist_by_age_restrict <- lion_hy_dist_toxo_restrict_sum %>%
#         group_by(age.cat.lion.first) %>%
#         summarize (n.app.dist = sum(!is.na(avg.min.dist.lion)),
#                    avg.app.dist = round(mean(avg.min.dist.lion, na.rm = T), 2),
#                    median.app.dist =  round(quantile(avg.min.dist.lion, c(.5), 
#                                                      na.rm = T), 2),
#                    sd.app.dist = round(sd(avg.min.dist.lion, na.rm = T), 2))
#       
    ## h) Bivariate descriptive stats for approach distance by age
      # calculate n, mean, median and standard deviation grouped by age
      bivar_dist_by_age_cat <- lion_hy_dist_toxo %>%
        group_by(age.cat.lion) %>%
        summarize (n.app.dist = sum(!is.na(distance)),
                   avg.app.dist = round(mean(distance, na.rm = T), 2),
                   median.app.dist =  round(quantile(distance, c(.5), 
                                                     na.rm = T), 2),
                   sd.app.dist = round(sd(distance, na.rm = T), 2))
      
    ## i) Plot approach distance by age (categorical)
      # NOTE: These summaries contain non-independent measures of app distances
      ggplot(data = subset(lion_hy_dist_toxo, !is.na(x = age.cat.lion)),
             
#   **************** Boxplot START ****************      
             aes(x = age.cat.lion, y = distance,
                 color = age.cat.lion)) +
        geom_boxplot() +
        geom_point(position=position_dodge(width=0.0), 
                   aes(color=toxo.status), size = 1)+
#   ***************** Boxplot END *****************
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = 'Hyena avg. minumum apporach distance towards lions by age',
             subtitle = 'Data are pseudoreplicated. Red pts = unifected, green pts = infected.
             Age is based on date of hyena lion interaction.') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        theme(legend.position = 'none') + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'darkgrey', 
                                       size = 1, linetype = 'solid')) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab('Avg. min. approach dist. (m)') +
        xlab('Hyena age (categorical)')
      
    ## j) Save Plot
      # use ggsave to save the plot
      ggsave('app_dist_by_age_cat_plot.pdf', plot = last_plot(), device = NULL, 
             path = paste0(here(),'/output'), 
             scale = 1, width = 11, height = 7,
             units = c('in'), dpi = 300, limitsize = TRUE)     
      
      
    ## k) Plot approach distance by age.mon.lion
      # NOTE: These summaries contain non-independent measures of app distances
      ggplot(data = subset(lion_hy_dist_toxo, !is.na(x = age.mon.lion)),
#   ************** Scatterplot START **************
             aes(x = age.mon.lion, y = distance)) +
        geom_point(aes(color=toxo.status), size = 1) +
        geom_smooth(method = loess, se = F) + # Add smooth curve best fit lines
#   *************** Scatterplot END ***************
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = 'Hyena avg. minumum apporach distance towards lions by age',
             subtitle = 'Data are pseudoreplicated. Red pts = unifected, green pts = infected.
             Age is based on date of hyena lion interaction.') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        theme(legend.position = 'none') + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'darkgrey', 
                                       size = 1, linetype = 'solid')) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab('Avg. min. approach dist. (m)') +
        xlab('Hyena age (mon)')
      
    ## l) Save Plot
      # use ggsave to save the plot
      ggsave('app_dist_by_age_mon_plot.pdf', plot = last_plot(), device = NULL, 
             path = paste0(here(),'/output'), 
             scale = 1, width = 11, height = 7,
             units = c('in'), dpi = 300, limitsize = TRUE) 
      
      
    ## m) Plot approach distance by social rank
      # NOTE: These summaries contain non-independent measures of app distances
      ggplot(data = subset(lion_hy_dist_toxo, !is.na(x = stan.rank.lion)),
#   ************** Scatterplot START **************
             aes(x = stan.rank.lion, y = distance)) +
        geom_point(aes(color=toxo.status), size = 1) +
        geom_smooth(method = loess, se = F) + # Add smooth curve best fit lines
#   *************** Scatterplot END ***************
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = 'Hyena avg. minumum apporach distance towards lions by standardize rank',
             subtitle = 'Data are pseudoreplicated. Red pts = unifected, green pts = infected.
             Hyean rank assessed during same year as lion hyena interaction.') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        theme(legend.position = 'none') + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'darkgrey', 
                                       size = 1, linetype = 'solid')) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab('Avg. min. approach dist. (m)') +
        xlab('Hyena standardized rank (-1 = lowest rank)')
      
    ## n) Save Plot
      # use ggsave to save the plot
      ggsave('app_dist_by_stnd_rank_plot.pdf', plot = last_plot(), device = NULL, 
             path = paste0(here(),'/output'), 
             scale = 1, width = 11, height = 7,
             units = c('in'), dpi = 300, limitsize = TRUE) 
      
    # ## o) Bivariate descriptive stats for approach distance by food present.
    #   # calculate n, mean, median and standard deviation grouped by food pres.
    #   bivar_dist_by_fd_prsnt <- lion_hy_dist_toxo %>%
    #     group_by(food.present) %>%
    #     summarize (n.app.dist = sum(!is.na(distance)),
    #                avg.app.dist = round(mean(distance, na.rm = T), 2),
    #                median.app.dist =  round(quantile(distance, c(.5), 
    #                                                  na.rm = T), 2),
    #                sd.app.dist = round(sd(distance, na.rm = T), 2))
    #   
    #   
    # ## p) save the data frame of summary stats out as a pdf into output file
    #   pdf(here('output/dist_by_fd_pres_summary.pdf'), 
    #       height = 3, width = 7)
    #   grid.table(bivar_dist_by_fd_prsnt)
    #   dev.off() 
      
    ## q) Bivariate descriptive stats for approach distance by food present
      # calculate n, mean, median and standard deviation grouped by food pres.
      bivar_dist_by_mode_fd_pres_full <- lion_hy_dist_toxo_sum %>%
        group_by(mode.fd.pres) %>%
        summarize (n.app.dist = sum(!is.na(avg.min.dist.lion)),
                   avg.app.dist = round(mean(avg.min.dist.lion, na.rm = T), 2),
                   median.app.dist =  round(quantile(avg.min.dist.lion, c(.5),
                                                     na.rm = T), 2),
                   sd.app.dist = round(sd(avg.min.dist.lion, na.rm = T), 2))
      
    ## p) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/bivar_dist_by_mode_fd_pres_full'), 
          height = 3, width = 7)
      grid.table(bivar_dist_by_mode_fd_pres_full)
      dev.off() 
      
    # ## r) Bivariate descriptive stats for approach distance by food present
    #   # when data are restricted (before seronegative, after seropositive)
    #   # calculate n, mean, median and standard deviation grouped by food pres.
    #   bivar_dist_by_mode_fd_pres_restrict <- lion_hy_dist_toxo_restrict_sum %>%
    #     group_by(mode.fd.pres) %>%
    #     summarize (n.app.dist = sum(!is.na(avg.min.dist.lion)),
    #                avg.app.dist = round(mean(avg.min.dist.lion, na.rm = T), 2),
    #                median.app.dist =  round(quantile(avg.min.dist.lion, c(.5), 
    #                                                  na.rm = T), 2),
    #                sd.app.dist = round(sd(avg.min.dist.lion, na.rm = T), 2))
      
    ## s) Plot approach distance food present
      # NOTE: These summaries contain non-independent measures of app distance
      ggplot(data = subset(lion_hy_dist_toxo, !is.na(x = food.present)),
             #   **************** Boxplot START ****************      
             aes(x = food.present, y = distance,
                 color = food.present)) +
        geom_boxplot() +
        geom_point(position=position_dodge(width=0.0), 
                   aes(color=toxo.status), size = 1)+
        #   ***************** Boxplot END *****************
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = 'Hyena avg. minumum apporach distance towards lions by food presence',
             subtitle = 'Data are pseudoreplicated. Red pts = unifected, green pts = infected.
             Food presence during lion hyena interaction.') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        theme(legend.position = 'none') + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'darkgrey', 
                                       size = 1, linetype = 'solid')) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab('Avg. min. approach dist. (m)') +
        xlab('Food present')
      
    ## t) Save Plot
      # use ggsave to save the plot
      ggsave('app_dist_by_fd_pres_plot.pdf', plot = last_plot(), device = NULL, 
             path = paste0(here(),'/output'), 
             scale = 1, width = 11, height = 7,
             units = c('in'), dpi = 300, limitsize = TRUE)  

    ## u) Bivariate descriptive stats for approach distance by human disturbance
      # calculate n, mean, median and standard deviation grouped by age.
      bivar_dist_by_hum_dist_sum <- lion_hy_dist_toxo_sum %>%
        group_by(hum.pop.den) %>%
        summarize (n.app.dist = sum(!is.na(avg.min.dist.lion)),
                   avg.app.dist = round(mean(avg.min.dist.lion, na.rm = T), 2),
                   median.app.dist =  round(quantile(avg.min.dist.lion, c(.5),
                                                     na.rm = T), 2),
                   sd.app.dist = round(sd(avg.min.dist.lion, na.rm = T), 2))

    ## v) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/dist_by_hum_dist_summary.pdf'),
          height = 3, width = 7)
      grid.table(bivar_dist_by_hum_dist_sum)
      dev.off()

    # ## w) Bivariate descriptive stats for approach distance by human disturbance
    #   # when data are restricted (before seronegative, after seropositive)
    #   # calculate n, mean, median and standard deviation grouped by hum.pop.den
    #   bivar_dist_by_hum_dist_restrict <- lion_hy_dist_toxo_restrict_sum %>%
    #     group_by(hum.pop.den) %>%
    #     summarize (n.app.dist = sum(!is.na(avg.min.dist.lion)),
    #                avg.app.dist = round(mean(avg.min.dist.lion, na.rm = T), 2),
    #                median.app.dist =  round(quantile(avg.min.dist.lion, c(.5), 
    #                                                  na.rm = T), 2),
    #                sd.app.dist = round(sd(avg.min.dist.lion, na.rm = T), 2))
    
    ## x) Bivariate descriptive stats for approach distance by human disturb.
      # calculate n, mean, median and standard deviation grouped by hum.dist.lion
      bivar_dist_by_hum_dist <- lion_hy_dist_toxo %>%
        group_by(hum.dist.lion) %>%
        summarize (n.app.dist = sum(!is.na(distance)),
                   avg.app.dist = round(mean(distance, na.rm = T), 2),
                   median.app.dist =  round(quantile(distance, c(.5), 
                                                     na.rm = T), 2),
                   sd.app.dist = round(sd(distance, na.rm = T), 2))
      
    ## v) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/dist_by_hum_dist_summary.pdf'), 
          height = 3, width = 7)
      grid.table(bivar_dist_by_hum_dist)
      dev.off()
      
    ## y) Plot approach distance by hum disturbance
      # NOTE: These summaries contain non-independent measures of app distance
      ggplot(data = subset(lion_hy_dist_toxo, !is.na(x = hum.dist.lion)),
#   **************** Boxplot START ****************      
             aes(x = hum.dist.lion, y = distance,
                 color = hum.dist.lion)) +
        geom_boxplot() +
        geom_point(position=position_dodge(width=0.0), 
                   aes(color=toxo.status), size = 1)+
#   ***************** Boxplot END *****************
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = 'Hyena avg. minumum apporach distance towards lions by human disturbance',
             subtitle = 'Data are pseudoreplicated. Red pts = unifected, green pts = infected.
             Human disturbance assessed the year a hyena was born.') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        theme(legend.position = 'none') + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        theme(axis.line = element_line(colour = 'darkgrey', 
                                       size = 1, linetype = 'solid')) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab('Avg. min. approach dist. (m)') +
        xlab('Human disturbance')
      
    ## z) Save Plot
      # use ggsave to save the plot
      ggsave('app_dist_by_hum_dist_plot.pdf', plot = last_plot(), device = NULL, 
             path = paste0(here(),'/output'), 
             scale = 1, width = 11, height = 7,
             units = c('in'), dpi = 300, limitsize = TRUE)     
    
      
      
  ### 6.6 Descriptive stats min approach distance towards lions
    ## a) min approach distance towards lions summary 
      # when data set is full
      app_dist_by_toxo_full_summary <- lion_hy_dist_toxo_sum %>%
        group_by(toxo.status) %>%
        summarise (n.avg.min.dist =  sum(!is.na
                                         (avg.min.dist.lion)),
                   #n_rank = n(), # n including na
                   avg_life_avg_min_dist = round(mean
                                                 (avg.min.dist.lion,
                                                   na.rm = T), 2),
                   stdev_life_avg_min_dist = round(sd
                                                   (avg.min.dist.lion, 
                                                     na.rm = T), 2)) 
      
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/app_dist_by_toxo_full_summary.pdf'), 
          height = 3, width = 10)
      grid.table(app_dist_by_toxo_full_summary)
      dev.off()
      
    # ## c) min approach distance towards lions summary 
    #   # when data are restricted (before seronegative, after seropositive)
    #   app_dist_by_toxo_restrict_summary <- lion_hy_dist_toxo_restrict_sum %>%
    #     group_by(toxo.status) %>%
    #     summarise (n.avg.min.dist =  sum(!is.na
    #                                      (avg.min.dist.lion)),
    #                #n_rank = n(), # n including na
    #                avg_life_avg_min_dist = round(mean
    #                                              (avg.min.dist.lion,
    #                                                na.rm = T), 2),
    #                stdev_life_avg_min_dist = round(sd
    #                                                (avg.min.dist.lion, 
    #                                                  na.rm = T), 2))
      
  ### 6.8 Bivariate descriptive stats for AGE STRATIFIED approach distances
      
    # NOTE: The data 'lion_hy_dist_toxo_cub_sum' is the full data set  
    # including any hyena cub approach lion distance for diagnosed hyenas,
    # while 'lion_hy_dist_toxo_sub_adult_sum' includes distances when hyeans,
    # were subadults or adults. Repeated samples within age strata are 
    # averaged to summarize into a single value.
      
    ## a) Bivariate descriptive stats for cub age category summarized 
      # approach distance by
      bivar_cub_dist_by_toxo_sum <- lion_hy_dist_toxo_cub_sum %>%
        group_by(toxo.status) %>%
        summarize (n.app.dist = sum(!is.na(avg.min.dist.lion)),
                   avg.app.dist = round(mean(avg.min.dist.lion, na.rm = T), 2),
                   median.app.dist =  round(quantile(avg.min.dist.lion, c(.5), 
                                                     na.rm = T), 2),
                   sd.app.dist = round(sd(avg.min.dist.lion, na.rm = T), 2))
      
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/bivar_cub_dist_by_toxo_sum'), 
          height = 3, width = 7)
      grid.table(bivar_cub_dist_by_toxo_sum)
      dev.off() 
      
      
    ## c) Bivariate descriptive stats for subadult and adult
      # age category summarized approach distance by toxo status
      bivar_sub_adult_dist_by_toxo_sum <- 
        lion_hy_dist_toxo_sub_adult_sum %>%
        group_by(toxo.status) %>%
        summarize (n.app.dist = sum(!is.na(avg.dist.age.stndrzd)),
                   avg.app.dist.std = round(mean(avg.dist.age.stndrzd, 
                                             na.rm = T), 2),
                   avg.app.dist = round(mean(avg.min.dist.lion, 
                                             na.rm = T), 2),
                   median.app.dist.std =  round(quantile(avg.dist.age.stndrzd, 
                                                     c(.5),na.rm = T), 2),
                   median.app.dist =  round(quantile(avg.min.dist.lion, 
                                                     c(.5),na.rm = T), 2),
                   sd.app.dist.std = round(sd(avg.dist.age.stndrzd, 
                                              na.rm = T), 2),
                   sd.app.dist = round(sd(avg.min.dist.lion, 
                                          na.rm = T), 2))
      
    ## d) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/bivar_sub_adult_dist_by_toxo_sum'), 
          height = 3, width = 7)
      grid.table(bivar_sub_adult_dist_by_toxo_sum)
      dev.off()  
      
           
#
###############################################################################
##############              7. Data transformations              ##############
###############################################################################
  
  # ### 7.1 Bin hyena approach lion distances into a binary variable
  #     # Based on freq distribution of distances from lion_hy_dist_toxo
  #     # data set; should help deal with inaccuracy in distance estiimates
  #   ## a) Full data set (with pseudoreplication)    
  #     lion_hy_dist_toxo <- lion_hy_dist_toxo %>%
  #       mutate(distance.binary = as.factor(ifelse(distance < 10, 'close',
  #                                                'far')))
  #     
  #   ## b) Restricted data set (with pseudoreplication)    
  #     lion_hy_dist_toxo_restrict <- lion_hy_dist_toxo_restrict %>%
  #       mutate(distance.binary = as.factor(ifelse(distance < 10, 'close',
  #                                                 'far')))  

##****** NOTE: After identifiying age structure in hyena approach distance
      # we age standardized distances...section 7.2 is now run as section 4.7
     
  # ### 7.2 Age (at hyena lion interaction) z-score standardization of 
  #     # approach distances
  #     
  #   ## a) Age standardized distance for lion_hy_dist
  #     lion_hy_dist <- lion_hy_dist  %>%
  #       group_by(age.cat.lion) %>%
  #       mutate(dist.age.stndrzd = scale(distance))%>%
  #       ungroup() # ungroup the data frame.
  #     
  #     #NOTE: check that scaling occurs within category of lion age  
  #     # lion_hy_dist_cub <- filter(lion_hy_dist, age.cat.lion == 'cub')
  #     # lion_hy_dist_cub <- lion_hy_dist_cub  %>%
  #     #   mutate(age.stndrdz.lion = scale(lion_hy_dist_cub[ ,'distance']))
  #     
  #   ## b) Age standardized distance for lion_hy_dist_toxo
  #     lion_hy_dist_toxo <- lion_hy_dist_toxo  %>%
  #       group_by(age.cat.lion) %>%
  #       mutate(dist.age.stndrzd = scale(distance))%>%
  #       ungroup() # ungroup the data frame.
  #     
  #   ## c) Age standardized distance for lion_hy_dist_toxo_restrict
  #     lion_hy_dist_toxo_restrict <- lion_hy_dist_toxo_restrict  %>%
  #       group_by(age.cat.lion) %>%
  #       mutate(dist.age.stndrzd = scale(distance))%>%
  #       ungroup() # ungroup the data frame.  
   
          
  ### 7.x Center and Transform Predictive Variables
    ## a) center function based on column means    
#      center_fxn <- function(x) {
#        xcenter = colMeans(x, na.rm = T)
#        x - rep(xcenter, rep.int(nrow(x), ncol(x)))
#      }
      
    ## b) make a list of variable names to center, value = T is necessary
      # or column positions will be returned
#      vars_to_center <- c('age_mon','mass')
   
    ## c) Center continous predictors  
#      toxo_data[ ,c(vars_to_center)] <- 
#        center_fxn(toxo_data[ ,c(vars_to_center)])
      
    ## d) Z-score standardize
      # Use 'scale' function to z-score standardization function based on 
      # subtracting mean from each x and dividing 
      # by 1 sd in the select columns; raw data are replaced with z-scores 
      # Here Prey densities have been z-score transformed
      # Z-score standardizetoxo_data
      # standardizetoxo_data
      #  toxo_data[ ,c(vars_to_center)] <- 
      #    scale(toxo_data[ ,c(vars_to_center)])
        
      
  ### 7.3 Clean global environment
    ## a) Remove extra tables/dataframes 
      rm(list = setdiff(ls(), c('toxo_data',
                                'toxo_data_sub_adult',
                                'toxo_data_cub',
                                'lion_hy_dist_toxo', 
                                'lion_hy_dist_toxo_sum', 
                                'lion_hy_dist_toxo_cub_sum',
                                'lion_hy_dist_toxo_sub_adult_sum',
                                'lion_hy_dist_toxo_restrict_sub_adult',
                                'lion_hy_dist_toxo_sub_adult',
                                'lion_hy_dist_toxo_restrict',
                                'lion_hy_dist_toxo_restrict_sum', 
                                'toxo_mort')))
      
      
###############################################################################
##############  8. Identify confounding and precision variables  ##############
###############################################################################     
    
  ### 8.1 Boldness towards lions: Approach lion distance by sex
    ## a) Repeated measures minimum approach distance FULL data
      # min dist lions data by sex
      min.dist.by.sex.full <- lmer(distance ~ sex + 
                                      (1|hy.id),
                                    data = lion_hy_dist_toxo)
      
      summary(min.dist.by.sex.full) # model parameter estimates
      confint(min.dist.by.sex.full) # 95% CIs
      #plot(min.dist.by.sex.full) 
      
    ## b) Repeated measures minimum approach distance RESTRICTED data
      # min dist lions data by sex
      min.dist.by.sex.restrict <- lmer(distance ~ sex +
                                           (1|hy.id),
                                       data = lion_hy_dist_toxo_restrict) 
      
      summary(min.dist.by.sex.restrict) # model parameter estimates
      confint(min.dist.by.sex.restrict) # 95% CIs
      #plot(min.dist.by.sex.restrict)
   
      
  ### 8.2 Boldness towards lions: Approach lion distance by age
    ## a) Repeated measures minimum approach distance FULL data
      # min dist lions data by age
      min.dist.by.age.full <- lmer(distance ~ age.cat.lion + 
                                      (1|hy.id),
                                    data = lion_hy_dist_toxo)
      
      summary(min.dist.by.age.full) # model parameter estimates
      confint(min.dist.by.age.full) # 95% CIs
      #plot(min.dist.by.sex.full)
      
    ## b) Repeated measures minimum approach distance RESTRICTED data
      # min dist lions data by age
      min.dist.by.age.restrict <- lmer(distance ~ age.cat.lion +
                                          (1|hy.id),
                                        data = lion_hy_dist_toxo_restrict) 
      
      summary(min.dist.by.age.restrict) # model parameter estimates
      confint(min.dist.by.age.restrict) # 95% CIs
      #plot(min.dist.by.age.restrict)
      

  ### 8.3 Boldness towards lions: Approach lion distance by stan.rank
    ## a) Repeated measures minimum approach distance FULL data
      # min dist lions data by standardized rank (rank is for year of intx)
      life.dist.by.rank <- lmer(distance ~
                                  stan.rank.lion
                                + (1|hy.id),
                                data = lion_hy_dist_toxo)

      summary(life.dist.by.rank) # model parameter estimates
      confint(life.dist.by.rank) # 95% CIs
      #plot(life.dist.by.rank.)
      
    ## b) Repeated measures minimum approach distance RESTRICTED data
      # min dist lions data by whether or not food was present during 
      #those interactions
      life.dist.by.rank.restrict <- lmer(distance ~
                                           stan.rank.lion
                                         + (1|hy.id),
                                         data = lion_hy_dist_toxo_restrict)
      
      summary(life.dist.by.rank.restrict) # model parameter estimates
      confint(life.dist.by.rank.restrict) # 95% CIs
      #plot(life.dist.by.rank.restrict)
      
    ## c) Subadult, adult Repeated measures minimum approach distance FULL data
      # min dist lions data by standardized rank (rank is for year of intx)
      sub.adult.dist.by.rank <- lmer(distance ~
                                 stan.rank.lion
                                + (1|hy.id),
                                data = lion_hy_dist_toxo_sub_adult)
      
      summary(sub.adult.dist.by.rank) # model parameter estimates
      confint(sub.adult.dist.by.rank) # 95% CIs
      #plot(sub.adult.dist.by.rank)
      
      # Cub minimum approach distance FULL data
      # min dist lions data by standardized rank (rank is for year of intx)
      cub.dist.by.rank <- glm(avg.min.dist.lion ~
                                avg.stan.rank,
                                     data = lion_hy_dist_toxo_cub_sum)
      
      summary(cub.dist.by.rank) # model parameter estimates
      confint(cub.dist.by.rank) # 95% CIs
      #plot(cub.dist.by.rank)

      
  ### 8.4 Boldness towards lions: Approach lion distance by food present
    ## a) Repeated measures minimum approach distance FULL data
      # min dist lions data by whether or not food was present during 
      #those interactions
      min.dist.by.fd.pres <- lmer(distance ~ food.present
                                  + (1|hy.id),
                                  data = lion_hy_dist_toxo) 
      
      summary(min.dist.by.fd.pres) # model parameter estimates
      confint(min.dist.by.fd.pres) # 95% CIs
      #plot(min.dist.by.mode.fd.pres.restrict)
      
    ## b) Repeated measures minimum approach distance RESTRICTED data
      # min dist lions data by whether or not food was present during 
      #those interactions
      min.dist.by.fd.pres.restrict <- lmer(distance ~ 
                                             food.present
                                           + (1|hy.id),
                                           data = lion_hy_dist_toxo_restrict) 
      
      summary(min.dist.by.fd.pres.restrict) # model parameter estimates
      confint(min.dist.by.fd.pres.restrict) # 95% CIs
      #plot(min.dist.by.mode.fd.pres.restrict)
      
  ### 8.5 Boldness towards lions: Approach lion distance by hum.dist.lion
    ## a) # Repeated measures minimum approach distance FULL data
      # min dist lions data by human disturbance (the year each hy-lion intx) 
      min.dist.by.hum.pop.den <- lmer(distance ~ 
                                        hum.dist.lion
                                      + (1|hy.id),
                                      data = lion_hy_dist_toxo) 
      
      summary(min.dist.by.hum.pop.den) # model parameter estimates
      confint(min.dist.by.hum.pop.den) # 95% CIs
      #plot(min.dist.by.hum.pop.den) 
      
    ## b) Repeated measures minimum approach distance RESTRICTED data
      # min dist lions data by human disturbance (the year each hy-lion intx) 
      min.dist.by.hum.pop.den.restrict <- lmer(distance ~ 
                                                 hum.dist.lion
                                               + (1|hy.id),
                                               data = lion_hy_dist_toxo_restrict) 
      
      summary(min.dist.by.hum.pop.den.restrict) # model parameter estimates
      confint(min.dist.by.hum.pop.den.restrict) # 95% CIs
      #plot(min.dist.by.hum.pop.den.restrict) 
      

      
#***************************  Determinants of Toxo  ****************************     

  # ### 8.6 Lion variable and toxo confounding
  #     # Check if lion related covariates are associated with toxo status. 
  #     # There could be confounding due to sampling bias and not necessarily
  #     # biolgical confounding.
  #     
  #   ## a) Bivariate model restricted data: toxo status by 
  #     # whether or not food was present during lion hyena interactions
  #     toxo.by.fd.pres.restrict <- glmer(toxo.status ~ 
  #                                             food.present
  #                                           + (1|hy.id),
  #                                           data = lion_hy_dist_toxo_restrict, 
  #                                       family = binomial) 
  #     
  #     summary(toxo.by.fd.pres.restrict) # model parameter estimates
  #     confint(toxo.by.fd.pres.restrict) # 95% CIs
  #     #plot(life.dist.by.mode.fd.pres.restrict)
  #     
  #     # # Summarized data toxo status by the mode
  #     # # of whether or not food was present during those interactions
  #     # toxo.by.mode.fd.pres.restrict.sum <- glm(toxo.status ~ 
  #     #                                                 mode.fd.pres,
  #     #                                   data = lion_hy_dist_toxo_restrict_sum, 
  #     #                                   family = binomial) 
  #     # 
  #     # summary(toxo.by.mode.fd.pres.restrict.sum) # model parameter estimates
  #     # confint(toxo.by.mode.fd.pres.restrict.sum) # 95% CIs
  #     # #plot(life.dist.by.mode.fd.pres.restrict.sum)
  #     
    ## b) Bivariate model full data: toxo status by
      # whether or not food was present during those interactions
      toxo.by.fd.pres <- glmer(toxo.status ~ food.present
                                   + (1|hy.id),
                                   data = lion_hy_dist_toxo,
                                   family = binomial)

      summary(toxo.by.fd.pres) # model parameter estimates
      confint(toxo.by.fd.pres) # 95% CIs
      #plot(toxo.by.mode.fd.pres.restrict)

  #     # # Summarized data toxo status by the mode
  #     # # of whether or not food was present during those interactions
  #     #  
  #     # toxo.by.mode.fd.pres.full <- glm(toxo.status ~  
  #     #                                         mode.fd.pres, 
  #     #                                       data = lion_hy_dist_toxo_sum, 
  #     #                                       family = binomial) 
  #     # 
  #     # summary(toxo.by.mode.fd.pres.full) # model parameter estimates
  #     # confint(toxo.by.mode.fd.pres.full) # 95% CIs
  #     # #plot(toxo.by.mode.fd.pres.full)
  #     
      
  ### 8.7 Clean global environment
      ## a) Remove extra tables/dataframes 
      # rm(list = setdiff(ls(), c('toxo_data', 
      #                           # 'lion_hy_dist_toxo', 
      #                           # 'lion_hy_dist_toxo_sum', 
      #                           'lion_hy_dist_toxo_cub_sum',
      #                           'lion_hy_dist_toxo_sub_adult_sum',
      #                           'lion_hy_dist_toxo_restrict_sub_adult',
      #                           'lion_hy_dist_toxo_sub_adult',
      #                           # 'lion_hy_dist_toxo_restrict',
      #                           # 'lion_hy_dist_toxo_restrict_sum', 
      #                           'toxo_mort')))
      # 
      
      
      
###############################################################################
##############   9. Determinants of T. gondii infection models   ##############
###############################################################################    
      
  ### 9.1 Toxo by sex     
   ## a) Unadjusted logistic regression toxo_status by Sex
      sex.log <- glm(toxo.status ~ sex , 
                     subset(toxo_data,
                            !is.na(x = sex)),family = binomial) 
      
      summary(sex.log) # print model summary (log odds scale)
      confint(sex.log) # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(sex.log), confint (sex.log)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(sex.log), Sigma = vcov(sex.log), Terms = 2) 
      
    ## b) Adjusted logistic regression toxo_status by sex
      sex.log.adj <- glm(toxo.status ~ sex + age.mon.dart + hum.pop.den 
                         , 
                         data = toxo_data,
                         #data = toxo_data_no_gil_baj, # sensitivity
                         family = binomial) 
      #****NOTE...control for continuous age to save power and because ****
      summary(sex.log.adj) # print model summary (log odds scale)
      confint(sex.log.adj) # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(sex.log.adj), 
                 confint (sex.log.adj)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(sex.log.adj), 
                Sigma = vcov(sex.log.adj), 
                Terms = 2) 
      
      
 
  ### 9.2 Toxo by age 
    ## a) Unadjusted logistic regression toxo_status by age
      age.log <- glm(toxo.status ~ age.cat.dart , 
                     subset(toxo_data,
                            !is.na(x = age.cat.dart)),family = binomial) 
      
      summary(age.log) # print model summary (log odds scale)
      confint(age.log) # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(age.log), confint (age.log)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(age.log), Sigma = vcov(age.log), Terms = 2:3) 
      
    ## b) Adjusted logistic regression toxo_status by age.cat.dart
      age.cat.dart.log.adj <- glm(toxo.status ~ age.cat.dart + hum.pop.den + sex  
                                  , 
                                  data = toxo_data,
                                  #data = toxo_data_no_gil_baj, # sensitivity
                                  family = binomial)  
  
      summary(age.cat.dart.log.adj) # print model summary (log odds scale)
      confint(age.cat.dart.log.adj) # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(age.cat.dart.log.adj), 
                 confint (age.cat.dart.log.adj)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(age.cat.dart.log.adj), 
                Sigma = vcov(age.cat.dart.log.adj), 
                Terms = 2) 
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(age.cat.dart.log.adj), 
                Sigma = vcov(age.cat.dart.log.adj), 
                Terms = 3) 
 
      
  ### 9.3 Toxo by standardized rank 
    ## a) Unadjusted logistic regression toxo_status by stanrank.dart
      stanrank.log <- glm(toxo.status ~ stan.rank.dart ,
                                subset(toxo_data,
                                       !is.na(x = stan.rank.dart)),
                                family = binomial)
      
      
      summary(stanrank.log) # print model summary (log odds scale)
      confint(stanrank.log) # 95% CIs (log odds scale)

      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(stanrank.log),
                 confint (stanrank.log)))

      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(stanrank.log),
                Sigma = vcov(stanrank.log), Terms = 2)
      
    ## b) Unadjusted logistic regression toxo_status by stanrank.dart
      sub.adult.stanrank.log <- glm(toxo.status ~ stan.rank.dart ,
                          subset(toxo_data_sub_adult,
                                 !is.na(x = stan.rank.dart)),
                          family = binomial)
      
      summary(sub.adult.stanrank.log) # print model summary (log odds scale)
      confint(sub.adult.stanrank.log) # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(stanrank.log),
                 confint (stanrank.log)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(stanrank.log),
                Sigma = vcov(stanrank.log), Terms = 2)
      
    ## c) Unadjusted logistic regression toxo_status by avg.stan.rank among cubs  
      cub.stanrank.log <- glm(toxo.status ~ avg.stan.rank ,
                          subset(lion_hy_dist_toxo_cub_sum,
                                 !is.na(x = avg.stan.rank)),
                          family = binomial)
      
      summary(cub.stanrank.log) # print model summary (log odds scale)
      confint(cub.stanrank.log) # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(cub.stanrank.log),
                 confint (cub.stanrank.log)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(cub.stanrank.log),
                Sigma = vcov(cub.stanrank.log), Terms = 2)
      
      
    ## d) Adjusted logistic regression toxo_status by stanrank.dart
      stanrank.log.adj <- glm(toxo.status ~ stan.rank.dart +
                                age.mon.dart + hum.pop.den + sex  
                              , 
                          subset(toxo_data,
                                 !is.na(x = stan.rank.dart)),
                          family = binomial)
      
      
      summary(stanrank.log.adj) # print model summary (log odds scale)
      confint(stanrank.log.adj) # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(stanrank.log.adj),
                 confint (stanrank.log.adj)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(stanrank.log.adj),
                Sigma = vcov(stanrank.log.adj), Terms = 2)
      
      
  
  ### 9.4 Toxo by human disturbance   
    ## a) Unadjusted logistic regression toxo_status by hum_pop_den
      hum.pop.log <- glm(toxo.status ~ hum.pop.den , 
                         data = toxo_data,
                         #data = toxo_data_no_gil_baj, # sensitivity
                         #data = lion_hy_dist_toxo_sub_adult_sum, #sensitivity
                         family = binomial) 
      
      summary(hum.pop.log) # print model summary (log odds scale)
      confint(hum.pop.log) # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(hum.pop.log), confint (hum.pop.log)))
     
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(hum.pop.log), 
                Sigma = vcov(hum.pop.log), Terms = 2) 
      
      
    ## b) Adjusted Logistic regression toxo_status by hum.pop.den
      hum.pop.log.adj <- glm(toxo.status ~ hum.pop.den + sex + age.mon.dart
                             , 
                             data = toxo_data,
                             #data = toxo_data_no_gil_baj, # sensitivity
                             family = binomial) 
      #****NOTE...control for continuous age to save power and because all cubs
      # are also low disturbance; blank cell for cub age cat and hi disturb****
      summary(hum.pop.log.adj) # print model summary (log odds scale)
      confint(hum.pop.log.adj) # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(hum.pop.log.adj), confint (hum.pop.log.adj)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(hum.pop.log.adj), Sigma = vcov(hum.pop.log.adj), 
                Terms = 2) 
      
    ## c) Subadult and adult unadjusted logistic regression 
      # toxo_status by hum_pop_den 
      hum.pop.log.sub.adult <- glm(toxo.status ~ hum.pop.den 
                         , 
                         data = toxo_data_sub_adult, #sensitivity
                         family = binomial) 
      
      summary(hum.pop.log.sub.adult) # print model summary (log odds scale)
      confint(hum.pop.log.sub.adult) # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(hum.pop.log.sub.adult), 
                 confint (hum.pop.log.sub.adult)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(hum.pop.log.sub.adult), 
                Sigma = vcov(hum.pop.log.sub.adult), Terms = 2) 
   
      
  
###############################################################################
##############  10. Lions and hyena T. gondii infection models   ##############
###############################################################################      

  ### 10.1 Boldness towards lions: CUB approach lion distance by toxo status
    
    #** NOTE: all animals darted as cubs are from the low.hum.disturb period
    #** hyena, JAB had multiple approach distances from lions, so approach
      # distance and the age in months are averaged
      
    ## a) Unadjusted model cub data: lifestage min. approach dist. from lions 
      # by toxo status
      min.dist.unadj.cub <- glm(avg.min.dist.lion ~ toxo.status,
                                data = lion_hy_dist_toxo_cub_sum,
              #data = subset(lion_hy_dist_toxo_cub_sum, age.cat.dart =='cub'),
                                family = gaussian)
      
    ## b) Parameter estimates
      summary(min.dist.unadj.cub)  # model parameter estimates
      confint(min.dist.unadj.cub)  # 95% CIs
      #plot(min.dist.unadj.cub) # view fitted vs residuals
      
      
    ## d) Adjusted model cub data: lifestage min. approach dist. from lions 
      # by toxo status
      min.dist.adj.cub <- glm(avg.min.dist.lion ~ toxo.status + sex
                              + age.mon.lion
                             #+ mode.fd.pres    # sensitivity analyses
                             #* toxo.status     # sensitivity analyses
                             #+ male.lion.pres  # sensitivity analyses
                             #* toxo.status     # sensitivity analyses
                             #+ avg.stan.rank   # sensitivity analyses
                              ,
                              data = lion_hy_dist_toxo_cub_sum,
              #data = subset(lion_hy_dist_toxo_cub_sum, age.cat.dart =='cub'),
                                family = gaussian)
      
      ## b) Parameter estimates
      summary(min.dist.adj.cub)  # model parameter estimates
      confint(min.dist.adj.cub)  # 95% CIs
      #plot(min.dist.adj.cub) # view fitted vs residuals    
      
      
  ### 10.2 Boldness towards lions: SUB and ADULT 
      # lifestage average approach lion distance by toxo status
      
      #** NOTE: Some data were restricted such that average approach distances  
      # are based on approaches after diagnosis for seropositive animals and 
      # approaches before diagnosis for seronegative animals
      
    # ## a) Unadjusted sub adult model restricted data: min dist lions
    #       # by toxo status
    #       lion.dist.unadj.restrct.sub.adult <- lmer(distance ~ toxo.status +
    #                                       #dist.age.stndrzd  ~ toxo.status +
    #                                        (1|hy.id),
    #                 data = lion_hy_dist_toxo_restrict_sub_adult)
    #           #data = subset(lion_hy_dist_toxo_restrict, age.cat.lion !='cub'))
    # 
    # ## b) Parameter estimates
    #       summary(lion.dist.unadj.restrct.sub.adult)  # model parameter estimates
    #       confint(lion.dist.unadj.restrct.sub.adult)  # 95% CIs
    #       #plot(lion.dist.unadj.restrct) # view fitted vs residuals

    ## c) Unadjusted sub adult model FULL data: min dist lions
          # by toxo status
          lion.dist.unadj.sub.adult <- lmer(distance ~ toxo.status +
                                        #dist.age.stndrzd  ~ toxo.status +
                                           # stan.rank.lion +
                                            (1|hy.id),
                                data = lion_hy_dist_toxo_sub_adult)
         
          
    ## d) Parameter estimates
          summary(lion.dist.unadj.sub.adult)  # model parameter estimates
          confint(lion.dist.unadj.sub.adult)  # 95% CIs
          #plot(lion.dist.unadj.restrct) # view fitted vs residuals
          
    # ## e) Unadjusted model sub and adult data: lifestage age standardized
    #   # min. approach dist. from lions by toxo status
    #   min.dist.unadj.sub.adult <- glm(avg.min.dist.lion ~ toxo.status,
    #                       data = lion_hy_dist_toxo_sub_adult_sum,
    #                             family = gaussian)
    #   
    # ## f) Parameter estimates
    #   summary(min.dist.unadj.sub.adult)  # model parameter estimates
    #   confint(min.dist.unadj.sub.adult)  # 95% CIs
    #   #plot(min.dist.unadj.cub) # view fitted vs residuals
      
      
      # ## c) Bootstrap parameter estimates   
      #   # bootstrapping number of resampling simulations
      #   boot.min.dist.unadj.cub <- 
      #     bootMer(x = min.dist.unadj.cub,
      #             FUN = fixef, nsim = 1000,
      #             use.u = F, type = 'parametric')
      #   
      #   tidy(boot.min.dist.unadj.cub) # beta estimates and SE
      #   
      #   # use 'boot' package to generate 95% CI  
      #   bt.ci.min.dist.unadj.cub <- 
      #     boot.ci(boot.min.dist.unadj.cub, 
      #                                       type = c('perc', 'norm', 'basic'),
      #                                       index = 2) # CI for 1st betas
      #   
      #   print(bt.ci.min.dist.unadj.cub)
      #   
      #   
      
    ## g) Adjusted model sub and adult data: lifestage
      # min. approach dist. from lions by toxo status
      lion.dist.adj.sub.adult <- lmer(
                                 distance ~ toxo.status 
                                # dist.age.stndrzd  ~ toxo.status +
                                + sex  # not sig in bivariate
                                #+ age.mon.lion # many NA
                                + age.cat.dart
                                + age.cat.lion
                                #+ hum.pop.den 
                                # + food.present    sensitivity
                                #+ stan.rank.lion   # sensitivity, since only 
                                        # female ranks, can't control for sex
                                + (1|hy.id),
                                data = lion_hy_dist_toxo_sub_adult)
                                # sensitivity
                                #data = lion_hy_dist_toxo_restrict_sub_adult)
                                # data = subset(lion_hy_dist_toxo_sub_adult,
                                #               diagnosis != 'doubtful')) 
      #** NOTE there is a singular fit when including other variables, 
        # results still null, so we report only adjustment for age.cat.lion
        # and food.present
   
    ## h) Parameter estimates
      summary(lion.dist.adj.sub.adult)  # model parameter estimates
      confint(lion.dist.adj.sub.adult)  # 95% CIs
      #plot(lion.dist.unadj.restrct) # view fitted vs residuals
      vif(lion.dist.adj.sub.adult)
      
    # ## g) Adjusted model sub and adult RESTRICTED data: lifestage
    #   # min. approach dist. from lions by toxo status
    #   lion.dist.adj.restrct.sub.adult <- lmer(
    #     distance ~ toxo.status 
    #     # dist.age.stndrzd  ~ toxo.status +
    #     #+ sex  
    #     #+ age.mon.lion # many NA
    #     + age.cat.lion
    #     #+ hum.pop.den  
    #     + food.present 
    #     + (1|hy.id),
    #     data = lion_hy_dist_toxo_restrict_sub_adult)
    #   #data = subset(lion_hy_dist_toxo_restrict, age.cat.lion !='cub'))
    #   
    # ## h) Parameter estimates
    #   summary(lion.dist.adj.restrct.sub.adult)  # model parameter estimates
    #   confint(lion.dist.adj.restrct.sub.adult)  # 95% CIs
    #   #plot(lion.dist.unadj.restrct) # view fitted vs residuals  
      
    # ## i) Adjusted model sub and adult data: lifestage age standardized
    #   # min. approach dist. from lions by toxo status
    #   min.dist.adj.sub.adult <- glm(avg.min.dist.lion ~ toxo.status 
    #                                 + sex
    #                                 + hum.pop.den
    #                                # + mode.fd.pres
    #                                 + age.mon.dart,
    #                           data = lion_hy_dist_toxo_sub_adult_sum,
    #                                   family = gaussian)
    #   
    # ## j) Parameter estimates
    #   summary(min.dist.adj.sub.adult)  # model parameter estimates
    #   confint(min.dist.adj.sub.adult)  # 95% CIs
    #   #plot(min.dist.unadj.cub) # view fitted vs residuals
      
   
  ### 10.3 Graphs of cub approach distance from lions by toxo status    
      
    ## a) Unadjusted cub model
      min.dist.unadj.cub.tdy <- tidy(min.dist.unadj.cub) %>%
        filter(term != '(Intercept)') %>%
        relabel_predictors(c(toxo.status1 = 'Seropositive'))
      
    # ## b) add bootsrapped (percentile) 95% CI to tidy data
    #   meth.fec.cort.tdy.unadjust$conf.low <- 
    #     meth.fec.cort.ci.unadjust['mean.meth.z', '2.5 %']
    #   meth.fec.cort.tdy.unadjust$conf.high <- 
    #     meth.fec.cort.ci.unadjust['mean.meth.z', '97.5 %']
      
    ## c) Add cub model category variable
      min.dist.unadj.cub.tdy$model <- 'Unadjusted cub model'
      
    ## d) Fully adjusted cub model
      min.dist.adj.cub.tdy <- tidy(min.dist.adj.cub) %>%
        filter(term != '(Intercept)' &
                 term != 'sexm' &
                 term != 'age.mon.lion' &
                 term != 'mode.fd.presTRUE') %>%
        relabel_predictors(c(toxo.status1 = 'Seropositive'))
      
    # ## e) add bootstrapped (percentile) 95% CI to tidy data
    #   meth.fec.cort.tdy.adjust$conf.low <- 
    #     meth.fec.cort.ci.adjust['mean.meth.z', '2.5 %']
    #   meth.fec.cort.tdy.adjust$conf.high <- 
    #     meth.fec.cort.ci.adjust['mean.meth.z', '97.5 %']
      
    ## f) add cub model category variable
      min.dist.adj.cub.tdy$model <- 'Adjusted cub model'
        
    ## g) Combine tidy tables of lmer estimates into a single tidy table
      cub.lion.dist.est <- bind_rows(min.dist.unadj.cub.tdy, 
                                     min.dist.adj.cub.tdy)
      
    ## h) rename varaible for graphing with dwplot
      cub.lion.dist.est <- lion.dist.est %>%
        rename('estimate' = 'statistic')
      
    ## i) Manually add rows of data for the reference categorgy  
      cub.lion.dist.est <- cub.lion.dist.est %>% 
        add_row(term = 'Seronegative (reference)', estimate = 0, std.error = 0,
                p.value = NA, model = 'Unadjusted cub model') %>%
        add_row(term = 'Seronegative (reference)', estimate = 0, std.error = 0,
                p.value = NA, model = 'Adjusted cub model')

    ## j) convert 'model' from charater to factor
      cub.lion.dist.est$model <- as.factor(cub.lion.dist.est$model)
      
    ## k) Re-code *nominal* factor (with ordered levels)  
      # Set levels (odering) of the 'model' variable 
      cub.lion.dist.est <-
        transform(cub.lion.dist.est, 
                  model = factor(model, levels = c('Unadjusted cub model', 
                                        'Adjusted cub model')))
 
    # ## Re-code *nominal* factor (with ordered levels)  
    #   # Set levels (odering) of the 'term' variable
    #   cub.lion.dist.est <- 
    #     transform(cub.lion.dist.est, 
    #               term = factor(term,levels = c('Seronegative (reference)', 
    #                                         'Seropositive')))
    
    ## l) re-order dataframe according to the factor levels
      # cub.lion.dist.est <- cub.lion.dist.est[order(cub.lion.dist.est$model, 
      #                                              cub.lion.dist.est$term),]
      
      cub.lion.dist.est$term <- as.character(cub.lion.dist.est$term) 
      
      
    ## m) create a new dataframe of LS means for graphing
      cub.lion.dist.ls.means <- data.frame(term = c('Seronegative',
                                                    'Seropositive'),
                                           estimate = c(-89.5875, -43.6683),
                                           #std.error = c(10.9588, 9.3418),
                # Note forcing dwplot to graph SE by specificy the upper and
                # lower bounds instead of automatically calculating actual
                # 95% CI from SE
                                           conf.low = c((-89.5875 + -10.2894), 
                                                        (-43.6683 + -9.6752)),
                                           conf.high = c((-89.5875 + 10.2894), 
                                                         (-43.6683 + 9.6752)))

    ## n) Graph results using dotwhisker, broom, dplyr, and ggplot2 packages
      dwplot(cub.lion.dist.ls.means, 
             vline = geom_vline(xintercept = 0, colour = 'gray20', 
                                linetype = 2), # line at zero behind coefs
             dot_args = list(size = 3),
             whisker_args = list(size = 1),
             dodge_size = 1) + 
        #coord_flip() + # flip x and y axes
        xlim (-100,0) +
        labs(title = 'Associations between hyena cub approach 
distance from lions by T. gondii infection status.',
             subtitle = ('')) +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=18, face = 'bold')) +
        # theme(legend.justification = c(1,1), legend.position = c(1,0.25),
        #       legend.background = element_rect(fill = 'white'),
        #       legend.title = element_blank(), 
        #       legend.key = element_rect(fill = 'white')) +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        #theme(axis.line = element_line(colour = 'lightgrey', 
        #                               size = 1, linetype = 'solid')) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=18, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        #scale_color_grey (start = 0, end = 0) + # make color estimates black
        # color the dot and whiskers
        scale_color_manual(values=c('red3')) + 
        # NOTE: we flipped x and y axes above, so the 'xlab' is actually
        # 'ylab' and vice versa. 
        xlab(expression(atop(bold('Mean ± SE'), 
                             paste(italic('Minimum approach distance from lions (m)'))))) +
        #scale_y_discrete(labels = c('Seropostive hyenas')) +
        ylab('')
        #ylab(expression(italic('(seronegative hyenas = reference)')))
        #ylab(expression(atop(bold('Seropostive hyenas (by age category)'))))
                            # paste(italic('(reference = seronegative)')))))
      
      
    ## o) Save Plot
      # use ggsave to save the linearization plot
      ggsave('cub_app_lion_dist_plot.pdf', plot = last_plot(), 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 11,
             height = 6,
             units = c('in'), dpi = 300, limitsize = TRUE)
      
      
      
      
  ### 10.4 Graphs of cub approach distance from lions by toxo status       
      
    ## a) Unadjusted subadult/adult model
      lion.dist.unadj.sub.adult.tdy <- tidy(lion.dist.unadj.sub.adult) %>%
        filter(term != '(Intercept)' &
                 term != 'sd_(Intercept).hy.id' &
                 term != 'sd_Observation.Residual') %>%
        relabel_predictors(c(toxo.status1 = 'Seropositive'))
      
    ## b) Add subadult/adult model category variable
      lion.dist.unadj.sub.adult.tdy$model <- 'Unadjusted subadult/adult model'
      
    ## c) Fully adjusted subadult/adult model
      lion.dist.adj.sub.adult.tdy <- tidy(lion.dist.adj.sub.adult) %>%
        filter(term != '(Intercept)' &
                 term != 'age.cat.lionadult' &
                 term != 'food.presentTRUE' &
                 term != 'sd_(Intercept).hy.id' &
                 term != 'sd_Observation.Residual') %>%
        relabel_predictors(c(toxo.status1 = 'Seropositive'))
      
      # ## d) add bootsrapped (percentile) 95% CI to tidy data
      #   meth.fec.cort.tdy.adjust$conf.low <- 
      #     meth.fec.cort.ci.adjust['mean.meth.z', '2.5 %']
      #   meth.fec.cort.tdy.adjust$conf.high <- 
      #     meth.fec.cort.ci.adjust['mean.meth.z', '97.5 %']
      
    ## e) add model category variable
      lion.dist.adj.sub.adult.tdy$model <- 'Adjusted subadult/adult model'
      
    ## f) Combine tidy tables of lmer estimates into a single tidy table
      sub.adult.lion.dist.est <- bind_rows(lion.dist.unadj.sub.adult.tdy,
                                           lion.dist.adj.sub.adult.tdy)

    ## g) rename varaible for graphing with dwplot
      sub.adult..lion.dist.est <- lion.dist.est %>%
        rename('estimate' = 'statistic')

    ## h) Re-code *nominal* factor (with ordered levels)  
      # Set levels (odering) of 'model' variable 
      sub.adult.lion.dist.est <-
        transform(lion.dist.est, 
            model = factor(model,
                           levels = c('Unadjusted subadult/adult model', 
                                      'Adjusted subadult/adult model')))
      
    ## i) create a new dataframe of LS means for graphing
      sub.adult.lion.dist.ls.means <- data.frame(term = c('Seronegative',
                                                    'Seropositive'),
                                           estimate = c(-22.3751, -23.7789),
                                           #std.error = c(2.6152, 1.8382),
             # Note forcing dwplot to graph SE by specificy the upper and
             # lower bounds instead of automatically calculating actual
             # 95% CI from SE
                                           conf.low = c((-22.3751 + -2.5217), 
                                                        (-23.7789 + -1.6969)),
                                           conf.high = c((-22.3751 + 2.5217), 
                                                         (-23.7789 + 1.6969)))
      
    ## j) Graph results using dotwhisker, broom, dplyr, and ggplot2 packages
      dwplot(sub.adult.lion.dist.ls.means, 
             vline = geom_vline(xintercept = 0, colour = 'gray20', 
                                linetype = 2), # line at zero behind coefs
             dot_args = list(size = 3),
             whisker_args = list(size = 1),
             dodge_size = 1) + 
        #coord_flip() + # flip x and y axes
        xlim (-100,0) +
        labs(title = 'Associations between hyena subadult and adult approach 
distance from lions by T. gondii infection status.',
             subtitle = ('')) +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(plot.subtitle = element_text(hjust = 0.5, size = 14)) + 
        # bold and size title and axes labels
        theme(text = element_text(size=18, face = 'bold')) +
        # theme(legend.justification = c(1,1), legend.position = c(1,0.25),
        #       legend.background = element_rect(fill = 'white'),
        #       legend.title = element_blank(), 
        #       legend.key = element_rect(fill = 'white')) +
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = 'white')) +
        # add major axes
        #theme(axis.line = element_line(colour = 'lightgrey', 
        #                               size = 1, linetype = 'solid')) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face='bold', color='black', 
                                         size=18, angle=0,
                                         margin = margin(t = 10, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face='bold', color='black', 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        #scale_color_grey (start = 0, end = 0) + # make color estimates black
        # color the dot and whiskers
        scale_color_manual(values=c('goldenrod3')) + 
        # NOTE: we flipped x and y axes above, so the 'xlab' is actually
        # 'ylab' and vice versa. 
        xlab(expression(atop(bold('Mean ± SE'), 
                             paste(italic('Minimum approach distance from lions (m)'))))) +
        #scale_y_discrete(labels = c('Seropostive hyenas')) +
        ylab('')
      #ylab(expression(italic('(seronegative hyenas = reference)')))
      #ylab(expression(atop(bold('Seropostive hyenas (by age category)'))))
      # paste(italic('(reference = seronegative)')))))
      
      
    ## k) Save Plot
      # use ggsave to save the linearization plot
      ggsave('sub_adult_app_lion_dist_plot.pdf', plot = last_plot(), 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 11,
             height = 6,
             units = c('in'), dpi = 300, limitsize = TRUE)
      
      
    
         
  # ### 10.1 Boldness towards lions: Approach lion distance by toxo status  
  #   ## a) Unadjusted model restricted data: lifetime min dist lions 
  #     # by toxo status
  #     lion.dist.unadj.restrct <- lmer(#distance ~ toxo.status +
  #                                     dist.age.stndrzd  ~ toxo.status +
  #                                      (1|hy.id), 
  #               data = lion_hy_dist_toxo_restrict)
  #               #data = subset(lion_hy_dist_toxo_restrict, age.cat.lion !='cub'))
  #     
  #   ## b) Parameter estimates
  #     summary(lion.dist.unadj.restrct)  # model parameter estimates
  #     confint(lion.dist.unadj.restrct)  # 95% CIs
  #     #plot(lion.dist.unadj.restrct) # view fitted vs residuals
  #      
  #     
  #   # ## c) Bootstrap parameter estimates   
  #   #   # bootstrapping number of resampling simulations
  #   #   boot.lion.dist.by.sex.lmm.restrct <- 
  #   #     bootMer(x = lion.dist.by.sex.lmm.restrct,
  #   #             FUN = fixef, nsim = 1000,
  #   #             use.u = F, type = 'parametric')
  #   #   
  #   #   tidy(boot.lion.dist.by.sex.lmm.restrct) # beta estimates and SE
  #   #   
  #   #   # use 'boot' package to generate 95% CI  
  #   #   bt.ci.lion.dist.by.sex.lmm.restrct <- 
  #   #     boot.ci(boot.lion.dist.by.sex.lmm.restrct, 
  #   #                                         type = c('perc', 'norm', 'basic'),
  #   #                                         index = 2) # CI for 1st betas
  #   #   
  #   #   print(bt.ci.lion.dist.by.sex.lmm.restrct)
  #   #   
  #   #   
  #     
  #   ## c) Unadjusted model full data: lifetime min dist lions 
  #     # by toxo status
  #     lion.dist.unadj <- lmer(#distance ~ toxo.status +
  #                             dist.age.stndrzd  ~ toxo.status +
  #                               (1|hy.id), 
  #                     data = lion_hy_dist_toxo)
  #                     #data = subset(lion_hy_dist_toxo, age.cat.lion !='cub'))
  #     
  #   ## d) Parameter estimates
  #     summary(lion.dist.unadj)  # model parameter estimates
  #     confint(lion.dist.unadj)  # 95% CIs
  #     #plot(lion.dist.unadj) # view fitted vs residuals
  #     
  #     class(lion_hy_dist_restrict_sum$age.mon.lion)
  #     
  # ### 10.2 Boldness towards lions: Approach lion distance by toxo status  
  #   ## a) Adjusted model full data: lifetime min dist lions 
  #     # by toxo status
  #     lion.dist.adj.restrct <- lmer(#distance ~ toxo.status + 
  #                                   dist.age.stndrzd  ~ toxo.status +  
  #                                             age.cat.dart +
  #                                             # age.mon.dart + # many NA
  #                                             # age.mon.lion # many NA
  #                                             + sex + hum.pop.den + 
  #                                             food.present +  
  #                                            (1|hy.id), 
  #             data = lion_hy_dist_toxo_restrict)
  #             #data = subset(lion_hy_dist_toxo_restrict, age.cat.lion !='cub'))
  #     
  #     ## b) Parameter estimates
  #     summary(lion.dist.adj.restrct)  # model parameter estimates
  #     confint(lion.dist.adj.restrct)  # 95% CIs
  #     #plot(lion.dist.adj.restrct) # view fitted vs residuals
  #     vif(lion.dist.adj.restrct) # check multicollinearity 
  #     
  #   # ## c) Bootstrap parameter estimates   
  #   #   # bootstrapping number of resampling simulations
  #   #   boot.lion.dist.by.sex.lmm <- 
  #   #     bootMer(x = lion.dist.by.sex.lmm,
  #   #             FUN = fixef, nsim = 1000,
  #   #             use.u = F, type = 'parametric')
  #   #   
  #   #   tidy(boot.lion.dist.by.sex.lmm) # beta estimates and SE
  #   #   
  #   #   # use 'boot' package to generate 95% CI  
  #   #   bt.ci.lion.dist.by.sex.lmm <- 
  #   #     boot.ci(boot.lion.dist.by.sex.lmm, 
  #   #             type = c('perc', 'norm', 'basic'),
  #   #             index = 2) # CI for 1st betas
  #   #   
  #   #   print(bt.ci.lion.dist.by.sex.lmm)
  #   #   
  #     
  #   ## c) Adjusted model full data: lifetime min dist lions 
  #     # by toxo status
  #     lion.dist.adj <- lmer(#distance ~ toxo.status +
  #                           dist.age.stndrzd  ~ toxo.status + 
  #                                   age.cat.dart +
  #                                   # age.mon.dart + # many NA
  #                                   # age.mon.lion # many NA 
  #                                   + sex + hum.pop.den + 
  #                                    food.present +  
  #                                    (1|hy.id), 
  #                       data = lion_hy_dist_toxo)
  #                       #data = subset(lion_hy_dist_toxo, age.cat.lion !='cub'))
  #     
  #     ## d) Parameter estimates
  #     summary(lion.dist.adj)  # model parameter estimates
  #     confint(lion.dist.adj)  # 95% CIs
  #     #plot(lion.dist.adj) # view fitted vs residuals
  #     vif(lion.dist.adj) # check multicollinearity 
  #     
  #     
  # ### 10.3 Boldness towards lions: Approach lion distance by toxo status
  #   ## a) Unadjusted model restricted data: lifetime min dist lions 
  #     # by toxo status
  #     life.dist.by.toxo.restrict.unadj <- glm(#avg.min.dist.lion ~ 
  #                                             avg.dist.age.stndrzd ~
  #                                               toxo.status,
  #             data = lion_hy_dist_toxo_restrict_sum,
  #                                             family = gaussian) 
  #     
  #     summary(life.dist.by.toxo.restrict.unadj) # model parameter estimates
  #     confint(life.dist.by.toxo.restrict.unadj) # 95% CIs
  #     #plot(life.dist.by.toxo.restrict.unadj)
  #     
  #     # Wald Chi-square test of significance using 'aod'
  #     wald.test(b = coef(life.dist.by.toxo.restrict.unadj), 
  #               Sigma = vcov(life.dist.by.toxo.restrict.unadj), 
  #               Terms = 2)  
  #     
  #     
  #   ## b) Unadjusted model full data: lifetime min dist lions 
  #     # by toxo status
  #     life.dist.by.toxo.full.unadj <- glm(#avg.min.dist.lion ~
  #                                         avg.dist.age.stndrzd ~
  #                                           toxo.status,
  #                                         data = lion_hy_dist_toxo_sum,
  #                                         family = gaussian)  
  #     
  #     summary(life.dist.by.toxo.full.unadj) # model parameter estimates
  #     confint(life.dist.by.toxo.full.unadj) # 95% CIs
  #     #plot(life.dist.by.toxo.full.unadj)
  #     
  #     # Wald Chi-square test of significance using 'aod'
  #     wald.test(b = coef(life.dist.by.toxo.full.unadj), 
  #               Sigma = vcov(life.dist.by.toxo.full.unadj), 
  #               Terms = 2)  
  #     
  #     
  # ### 10.4 Boldness towards lions: Approach lion distance by toxo status
  #   ## a) Unadjusted model restricted data: lifetime min dist lions 
  #     # by toxo status
  #     life.dist.by.toxo.restrict.adj <- glm(#avg.min.dist.lion ~
  #                                           avg.dist.age.stndrzd ~
  #                                             toxo.status + sex +
  #                                             age.mon.dart +
  #                                            # mode.fd.pres  
  #                                           + hum.pop.den
  #                                           ,
  #                                     data = lion_hy_dist_toxo_restrict_sum, 
  #                                           family = gaussian) 
  #     
  #     # data = subset(lion_hy_dist_toxo_restrict_sum,
  #     #               mode.fd.pres == 'TRUE'),
  #     
  #     summary(life.dist.by.toxo.restrict.adj) # model parameter estimates
  #     confint(life.dist.by.toxo.restrict.adj) # 95% CIs
  #     #plot(life.dist.by.toxo.restrict.adj)
  #     vif(life.dist.by.toxo.restrict.adj) # check multicollinearity 
  #     
  #     # Wald Chi-square test of significance using 'aod'
  #     wald.test(b = coef(life.dist.by.toxo.restrict.adj), 
  #               Sigma = vcov(life.dist.by.toxo.restrict.adj), 
  #               Terms = 2)  
  #     
  #     
  #   ## b) Adjusted model full data: lifetime min dist lions 
  #     # by toxo status
  #     life.dist.by.toxo.full.adj <- glm(#dist.age.stndrzd ~
  #                                       avg.dist.age.stndrzd ~
  #                                         toxo.status + #sex +
  #                                         #age.mon.dart + 
  #                                          #mode.fd.pres + 
  #                                         + hum.pop.den
  #                                       ,
  #                                       data = lion_hy_dist_toxo_sum, 
  #                                       family = gaussian)  
  #     
  #     summary(life.dist.by.toxo.full.adj) # model parameter estimates
  #     confint(life.dist.by.toxo.full.adj) # 95% CIs
  #     #plot(life.dist.by.toxo.full.adj)
  #     vif(life.dist.by.toxo.full.adj) # check multicollinearity 
  #     
  #     # Wald Chi-square test of significance using 'aod'
  #     wald.test(b = coef(life.dist.by.toxo.full.adj), 
  #               Sigma = vcov(life.dist.by.toxo.full.adj), 
  #               Terms = 2)  
  #     

      
###############################################################################
#############  11: Hyena fitness and T. gondii infection models  ##############
###############################################################################   
      
  ### 11.1 Mortality lions vs other: toxo_status
    ## a) Logistic regression mortality source byt toxo_status
      mortality.log <- glm(mort.bin ~ toxo.status, 
                           data = toxo_mort, family = binomial) 
      
      summary(mortality.log)    # model summary (log odds scale)
      confint(mortality.log)    # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(mortality.log), confint (mortality.log)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(mortality.log), Sigma = vcov(mortality.log), 
                Terms = 2) 
    
    
    ## b) Summarize mortality data for chi-square/Fisher's exact test  
      mortality_table_ref <- toxo_mort %>%
        group_by(toxo.status, mort.bin) %>%
        summarize (counts = sum(!is.na(mort.bin)))
        
    ## c) make a contingency table for chi-sq / Fisher's exact test
      mortality_table <- matrix(c(2, 13, 6, 12), 
                                nrow = 2,
                                dimnames = list (independ = c('uninfected', 
                                                              'infected'),
                                                 depend = c('lion', 
                                                            'other')))
      
    ## d) Fisher exact test because less than 5 counts in some cells
      fisher.test(mortality_table, alternative = 'less')

      
    # CUBS ONLY: make a contingency table for chi-sq / Fisher's exact test
      cub_mortality_table <- matrix(c(1, 5, 5, 0), 
                                nrow = 2,
                                dimnames = list (independ = c('uninfected', 
                                                              'infected'),
                                                 depend = c('lion', 
                                                            'other')))
      
    # CUBS ONLY Fisher exact test because less than 5 counts in some cells
      fisher.test(cub_mortality_table, alternative = 'less')
      
      
    ## e) Logistic regression mortality source byt toxo_status
      mortality.log.adj <- glm(mort.bin ~ toxo.status + sex  
                               # + hum.pop.den # no death by lions in hi hum.
                                               # dist. for small data set
                           , 
                           data = toxo_mort, family = binomial) 
      
      summary(mortality.log.adj)    # model summary (log odds scale)
      confint(mortality.log.adj)    # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(mortality.log.adj), confint (mortality.log.adj)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(mortality.log.adj), Sigma = vcov(mortality.log.adj), 
                Terms = 2) 
      
      
 
###############################################################################
##############                12. Export data files               ##############
###############################################################################
      
  ### 12.1 Export toxo data to csv     
      # Save and export tables as a .cvs spreadsheet and named with today's
      # date. Files are saved in the 'output' folder in the working directory.
   
    ## a) Generate file name for toxo_data_covariates.csv
      csv.file.name.toxo.covar <- paste0(here
                                         ('output', 'toxo_data_covariates.csv'))
      
    # ## b) Generate file name for lion_hy_dist_sum.csv
    #   csv.file.name.lion.dist.repeat <- paste0(here
    #                                      ('data', 'lion_hy_dist_repeat.csv')) 
      
    ## b) Generate file name for lion_hy_dist_toxo.csv
      csv.file.name.lion.dist.toxo <- paste0(here
                                               ('data', 
                                                 'lion_hy_dist_toxo.csv'))
      
    ## c) Generate file name for lion_hy_dist_toxo_cub_sum.csv
      csv.file.name.lion.dist.toxo.cub.sum <- paste0(here
                                             ('data', 
                                              'lion_hy_dist_toxo_cub_sum.csv'))
      
    ## d) Generate file name for lion_hy_dist_toxo_sub_adult.csv
      csv.file.name.lion.dist.toxo.sub.adult <- 
        paste0(here('data', 'lion_hy_dist_toxo_sub_adult.csv'))
      
    ## e) Generate file name for lion_hy_dist_toxo_restrict_sum.csv
      csv.file.name.lion.dist.toxo.restrict.sum <- 
        paste0(here('data', 'lion_hy_dist_toxo_restrict_sum.csv'))
    
    ## f) Generate file name for toxo_data.csv
      csv.file.name.toxo.data <- 
        paste0(here('data', 'toxo_data.csv'))
      
    ## g) Generate file name for toxo_data.csv
      csv.file.name.toxo.mort <- 
        paste0(here('data', 'toxo_mort.csv'))
      
      
  ### 12.2 Save tables      
      # Save data frame as a .csv file (a spreadsheet/table) into the 
      # output data folder in the working directory.
      
    ## a) Save toxo_data_covariates table
      write.csv (toxo_data_covariates, file = csv.file.name.toxo.covar)
      
    # ## b) Save intermediate table
    #   write.csv (lion_hy_dist_repeat, file = csv.file.name.lion.dist.repeat)
 
    ## b) Save intermediate table
      write.csv (lion_hy_dist_toxo, file = csv.file.name.lion.dist.toxo)
      
    ## c) Save intermediate table
      write.csv (lion_hy_dist_toxo_cub_sum, 
                 file = csv.file.name.lion.dist.toxo.cub.sum)
      
    ## d) Save intermediate table
      write.csv (lion_hy_dist_toxo_sub_adult, 
                 file = csv.file.name.lion.dist.toxo.sub.adult)
      
    ## e) Save intermediate table
      write.csv (lion_hy_dist_toxo_restrict_sum, 
                 file = csv.file.name.lion.dist.toxo.restrict.sum)
      
    ## f) Save intermediate table
      write.csv (toxo_data, 
                 file = csv.file.name.toxo.data)
      
    ## g) Save intermediate table
      write.csv (toxo_mort, 
                  file = csv.file.name.toxo.mort)
      

    
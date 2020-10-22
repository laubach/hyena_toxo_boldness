###############################################################################
##############          Spotted hyena LUMA DNA methylation:      ##############
##############                Life history traits                ##############
##############                 By: Zach Laubach                  ##############
##############               created: 1 March 2019               ##############
##############            last updated: 02 April 2019            ##############
###############################################################################


  ### PURPOSE: This code is desingned to analyze LUMA data and associations of 
    # global DNA methylation with life history traits in hyenas.
  
  
  # Code Blocks
    # 1: Configure workspace
    # 2: Import data
    # 3: Tidy individual tables
    # 4: Join tables and re-tidy fecal luma data
    # 5: Univariate analysis 
    # 6: Bivariate analysis 
    # 7: Data transformations 
    # 8: Model luma and life history
    # 9: Export data files



###############################################################################
##############             1.  Configure workspace               ##############
###############################################################################

  ### 1.1 clear global environment
    rm(list = ls())


  ### 1.2 Install and load Mara Hyena Project packages 
    ## a) Load the Mara Hyena Project data files from github
      # Check for devtools and install if not already installed
      if(!'devtools' %in% row.names(installed.packages())){
        install.packages('devtools')
      }
    ## b) Use devtools to install hyeanadata package from the MaraHyenaProject
      # on github
      devtools::install_github('MaraHyenaProject/hyenadata',
                      auth_token = "e31091e1f6277964e3a52cf3e62ffdc59ea1cadc")
      # load hyenadata package
      library('hyenadata')
  

  ### 1.3 Install and load CRAN packages   
    ## a) Data Manipulation and Descriptive Stats Packages
      # Check for tidyverse and install if not already installed
        if (!'tidyverse' %in% installed.packages()[,1]){
          install.packages ('tidyYverse')
        }
      # load tidyverse packages
        library ('tidyverse')
      
      # Check for sqldf and install if not already installed
        if (!'sqldf' %in% installed.packages()[,1]){
          install.packages ('sqldf')
        }
        options(gsubfn.engine = "R") #fixes tcltk bug; run before require sqldf
      # load tidyverse packages
        library ('sqldf')
      
      # Check for lubridate and install if not already installed
        if (!'lubridate' %in% installed.packages()[,1]){
          install.packages ('lubridate')
        }
      # load lubridate packages
        library ('lubridate') 
        
      # Check for naniar and install if not already installed
        # used to replace values with NA ***CONVERTS FACTORS to NUMBERS***
        if (!'naniar' %in% installed.packages()[,1]){
          install.packages ('naniar')
        }
      # load naniar package
        library ('naniar') 
        
      # Check for rcompanion and install if not already installed
        # used to compute a Tukey power transformation
        if (!'rcompanion' %in% installed.packages()[,1]){
          install.packages ('rcompanion')
        }
      # load rcompanion packages
        library ('rcompanion') 
        
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
        
      # Check for ggfortify and install if not already installed (graphing
      # survival, PCA, hierarchial models etc.)
        if (!'ggfortify' %in% installed.packages()[,1]){
          install.packages ('ggfortify')
        }
      # load ggfortify packages
        library ('ggfortify')
        
      # Check for survminer and install if not already installed (graphing
      # survival curves with ggplot)
        if (!'survminer' %in% installed.packages()[,1]){
          install.packages ('survminer')
        }
      # load survminer packages
        library ('survminer')
        
        
    ## c) Modeling Packages
      # Check for broom and install if not already installed
        if (!'broom' %in% installed.packages()[,1]){
          install.packages ('broom')
        }
      # load broom packages
        library ('broom')
        
      # Check for nlme and install if not already installed
        if (!'nlme' %in% installed.packages()[,1]){
          install.packages ('nlme')
        }
      # load nlme packages
        library ('nlme')
        
      # Check for lme4 and install if not already installed
        if (!'lme4' %in% installed.packages()[,1]){
          install.packages ('lme4')
        }
      # load lme4 packages
        library ('lme4')
        
      # Check for car and install if not already installed
        # includes vif function
        if (!'car' %in% installed.packages()[,1]){
          install.packages ('car')
        }
      # load car packages
        library ('car')
        
      # Check for bbmle and install if not already installed
        # for AICtab
        if (!'bbmle' %in% installed.packages()[,1]){
          install.packages ('bbmle')
        }
      # load bbmle packages
        library ('bbmle')
        
      # Check for survival and install if not already installed
        if (!'survival' %in% installed.packages()[,1]){
          install.packages ('survival')
        }
      # load survival packages
        library ('survival') 
        

  ### 1.3 Get Version and Session Info
    R.Version()
    sessionInfo()
    
    # Developed in:   
    # R version 3.5.2 (2018-12-20)
    # Platform: x86_64-apple-darwin15.6.0 (64-bit)
    # Running under: macOS  Mojave 10.14.3
    
  
  ### 1.4 Set working directory 
    setwd(here())
  
  
  ### 1.5 Set file paths for data importing and exporting
    ## a) The path to cleand LUMA data
      luma_data_path <- paste0(here("data"))
    
    ## b) The path to other variables to be modeled with luma, stress, and 
      # life history data
      general_data_path <- paste("~/R/R_wd/fisi/project/0_data/",
                              sep = '')
    
    ## c) Source scripts path
      source_path <- paste("~/Git/source_code/")
      
  
  ### 1.6 Source functions
    ## a) all_char_to_lower function
      source(file = paste0(source_path, "all_char_to_lower.R"))
      
    ## b) format_var_names function
      source(file = paste0(source_path, "format_var_names.R"))
      
    ## c) format_var_names_dash function
      source(file = paste0(source_path, "format_var_names_dash.R"))  
      

      
###############################################################################
##############                  2. Import data                   ##############
###############################################################################    
      
  ### 2.1 Import LUMA data files
    ## a) Import LUMA data, which has undergone QAQC (see R script 
      # luma_prep_analysis.R) and has been joined to covariates from
      # tblHyenas and tblLifehistory (see R script hy_luma_explntry_var.R)
      luma_data <- read_csv(paste0(luma_data_path,
                                  "/luma_data.csv"))
      
    ## b) remove first numbering column
      luma_data <- luma_data %>%
        subset(select = -c(X1))
      
    ## c) Import LUMA data, which has undergone QAQC (see R script 
      # luma_prep_analysis.R).
      # NOTE: Methylation data have been averaged across repeated samples,
      # and have been joined to covariates from tblHyenas and tblLifehistory 
      #(see R script hy_luma_explntry_var.R).
      luma_data_avg <- read_csv(paste0(luma_data_path,
                                       "/luma_data_avg.csv"))
      
    ## d) remove first numbering column
      luma_data_avg <- luma_data_avg %>%
        subset(select = -c(X1))
      
    ## e) Import LUMA data, which has undergone QAQC (see R script 
      # luma_prep_analysis.R).
      # NOTE: Methylation data have been averaged across repeated samples 
      # within categorical age groupings (i.e. mult cub samps averaged per ID),
      # and have been joined to covariates from tblHyenas and tblLifehistory 
      #(see R script hy_luma_explntry_var.R).
      luma_data_avg_age <- read_csv(paste0(luma_data_path,
                                       "/luma_data_avg_age.csv"))
      
    ## f) remove first numbering column
      luma_data_avg_age <- luma_data_avg_age %>%
        subset(select = -c(X1))
      
    ## g) Import LUMA data, which has undergone QAQC (see R script 
      # luma_prep_analysis.R).
      # NOTE: Methylation data repeats have been 'averaged' using BLUPs,
      # and have been joined to covariates from tblHyenas and tblLifehistory 
      #(see R script hy_luma_explntry_var.R).
      luma_data_blup <- read_csv(paste0(luma_data_path,
                                       "/luma_data_blup.csv"))
    
    ## h) remove first numbering column
      luma_data_blup <- luma_data_blup %>%
        subset(select = -c(X1))
      
  ### 2.2 Import Access Fisi data files
    ## a) Import Access data backend from Mara Hyena Project data package
      # Use hyenadata package 'load_all_tables' function to load all tables
      hyenadata::load_all_tables()
      
    ## b) Import working version of tblReprostats
      repro_state <- read_csv(paste(general_data_path,
                                    "reprostates_EDS.csv", sep = ''))
      
    ## c) remove first numbering column
      repro_state  <- repro_state  %>%
        subset(select = -c(X1))
      
  

###############################################################################
##############              3. Tidy individual tables            ##############
###############################################################################

  ### 3.1 Tidy repro_state
    ## a) Convert all text to lower case
      repro_state <- AllCharactersToLower(repro_state)
      
    ## b) Format variable names (lowercase and separated by '.')
      repro_state <- FormatVarNames(repro_state)  
    
    ## c) Rename mom as hy.id 
      repro_state <- repro_state %>%
        rename('hy.id' = 'mom')
      
    ## d) Subset data to include only where repro state is l 
      # (which indicates start of lactating)
      repro_state <- repro_state %>%
        filter(grepl("l", state)) 
    
    ## e) Convert repro_state from long to wide    
      repro_state <- repro_state %>%
        # combine information from multiple columns into a single column
        unite(col = info, cycle.start, cycle.stop, state, trimester,
              cub1, cub2, notes, sep = ";") %>%
        # indicate that data rows distinguished by individual and keep parity
        # info
        distinct(hy.id, parity, .keep_all = T) %>%
        # indicate columns distinguished by parity
        spread(key = parity, value = info)
      
    ## f) Separate united info into spearate variables/columns
      repro_state <- repro_state %>%
        #summarize <- hy.id = first('hy.id') %>%
        separate(col = a, into = c('a.cycle.start', 'a.cycle.stop', 'a.state',
                                   'a.trimester', 'a.cub1', 'a.cub2', 
                                   'a.notes'), sep = ";", remove = T) %>%
        separate(col = b, into = c('b.cycle.start', 'b.cycle.stop', 'b.state',
                                   'b.trimester', 'b.cub1', 'b.cub2', 
                                   'b.notes'), sep = ";", remove = T) %>%
        separate(col = c, into = c('c.cycle.start', 'c.cycle.stop', 'c.state',
                                   'c.trimester', 'c.cub1', 'c.cub2', 
                                   'c.notes'), sep = ";", remove = T) %>%
        separate(col = d, into = c('d.cycle.start', 'd.cycle.stop', 'd.state',
                                   'd.trimester', 'd.cub1', 'd.cub2', 
                                   'd.notes'), sep = ";", remove = T) %>%
        separate(col = e, into = c('e.cycle.start', 'e.cycle.stop', 'e.state',
                                   'e.trimester', 'e.cub1', 'e.cub2', 
                                   'e.notes'), sep = ";", remove = T) %>%
        separate(col = f, into = c('f.cycle.start', 'f.cycle.stop', 'f.state',
                                   'f.trimester', 'f.cub1', 'f.cub2', 
                                   'f.notes'), sep = ";", remove = T) %>%
        separate(col = g, into = c('g.cycle.start', 'g.cycle.stop', 'g.state',
                                   'g.trimester', 'g.cub1', 'g.cub2', 
                                   'g.notes'), sep = ";", remove = T) %>%
        separate(col = h, into = c('h.cycle.start', 'h.cycle.stop', 'h.state',
                                   'h.trimester', 'h.cub1', 'h.cub2', 
                                   'h.notes'), sep = ";", remove = T) %>%
        separate(col = i, into = c('i.cycle.start', 'i.cycle.stop', 'i.state',
                                   'i.trimester', 'i.cub1', 'i.cub2', 
                                   'i.notes'), sep = ";", remove = T) %>%
        separate(col = j, into = c('j.cycle.start', 'j.cycle.stop', 'j.state',
                                   'j.trimester', 'j.cub1', 'j.cub2', 
                                   'j.notes'), sep = ";", remove = T) %>%
        separate(col = k, into = c('k.cycle.start', 'k.cycle.stop', 'k.state',
                                   'k.trimester', 'k.cub1', 'k.cub2', 
                                   'k.notes'), sep = ";", remove = T) %>%
        separate(col = l, into = c('l.cycle.start', 'l.cycle.stop', 'l.state',
                                   'l.trimester', 'l.cub1', 'l.cub2', 
                                   'l.notes'), sep = ";", remove = T) %>%
        separate(col = m, into = c('m.cycle.start', 'm.cycle.stop', 'm.state',
                                   'm.trimester', 'm.cub1', 'm.cub2', 
                                   'm.notes'), sep = ";", remove = T)
      
    ## g) Create a vector of values representing NA
      na_strings <- c('NA')
      
    ## h) Convert hy.id from factor to character so it is not changed to 
      # a number by naniar
      repro_state$hy.id <- as.character(repro_state$hy.id)
      
    ## i) Use naniar to replace all values representing NA with actual NA
      repro_state <- repro_state %>%
        replace_with_na_all(condition = ~.x %in% na_strings)
      
 
  ### 3.2 Tidy female_ranks
    ## a) rename data frame
      female_ranks <- tblFemaleRanks
      
    ## b) Convert all text to lower case
      female_ranks <- AllCharactersToLower(female_ranks)
      
    ## c) Format variable names (lowercase and separated by '.')
      female_ranks <- FormatVarNames(female_ranks) 
      
    ## d) Rename id as hy.id 
      female_ranks <- female_ranks %>%
        rename('hy.id' = 'id')
      
      
  ### 3.3 Tidy luma_data_avg_age
    ## a) subset data to include only cub sub adults
      luma_data_cubsub <- luma_data_avg_age  %>% 
        filter(grepl("cubsub", age.dart.bi)) 
    
      
  ### 3.4 Clean global environment
    ## a) Remove extra tables/dataframes
      rm(list = ls(pattern = 'tbl'))
      rm(list = c('luma_data_avg', 'luma_data_avg_age', 'luma_data_blup'))
      
      
      
###############################################################################
##############  4. Join tables and re-tidy life hist. luma data  ##############
###############################################################################        
    
  ### 4.1 Combine repro_state w luma_data_cubsub
    ## a) rename luma_data_cubsub as luma_data_lf_hist  
      luma_data_lf_hist <- luma_data_cubsub
      
    ## b) Filter data to remove males from luma_data_lf_hist 
      luma_data_lf_hist <- luma_data_lf_hist  %>%
        filter(grepl('f', sex)) 
    #***NOTE*** Data inclusion cut-off; filter data to include only females,
      # because males disperse and cannot easily be tracked
        
    ## c) Join repro state data to fecal data 
      luma_data_lf_hist <- luma_data_lf_hist %>%
        left_join(select(repro_state, c(hy.id, a.cycle.start, a.cycle.stop,
                                        a.state, a.trimester, a.cub1,
                                        a.cub2, a.notes)),
                  by = c('hy.id' = 'hy.id')) 
    
  
  ### 4.2 Tidy luma_data_lf_hist: calculate additional variables  
    ## a) Create a varialbe, 'age.1st.part.mon' which indicates a hyena's 
      # age (in months) when it first gave birth  using lubridate
      luma_data_lf_hist <- luma_data_lf_hist  %>%
        mutate(age.1st.part.mon = round((interval(dob.date,
                                          a.cycle.start) %/% days(1) 
                                 / 30.44), 1))
      ## NOTE: To convert from age in days to age in months divde by average 
      # number of days per month  (30.44) 
      
    ## b) Create a varialbe, 'age.1st.part.days' which indicates a hyena's 
      # age (in months) when it first gave birth  using lubridate  
      luma_data_lf_hist <- luma_data_lf_hist  %>%
        mutate(age.1st.part.days = (interval(dob.date,
                                     a.cycle.start) %/% days(1)))
      
    ## c) Create a varialbe, 'long.mon' which indicates how old hyena was 
      # at the date it was last seen/died in months using lubridate
      luma_data_lf_hist <- luma_data_lf_hist  %>%
        mutate(long.mon = round((interval(dob.date,
                                          disappear.date) %/% days(1) 
                                 / 30.44), 1))
      ## NOTE: To convert from age in days to age in months divde by average 
      # number of days per month  (30.44) 
      
    ## d) Create a varialbe, 'long.days,' which indicates how old hyena was 
      # at the date it was last seen/died in days lubridate
      luma_data_lf_hist <- luma_data_lf_hist  %>%
        mutate(long.days = (interval(dob.date,
                                     disappear.date) %/% days(1)))
      
    ## e) Create a varialbe, 'cur.age.mon' which indicates how old hyena was 
      # 6 months prior (to account for disappearance) to last day in 
      # tblSessions accessed (Mar 2019)
      # OR could use the date that data updates stopped for repro_state 
      # 2019-01-01
      luma_data_lf_hist <- luma_data_lf_hist  %>%
        mutate(cur.age.2017.mon = round((interval(dob.date,
                                          '2017-01-01') %/% days(1) 
                                 / 30.44), 1)) %>%
        mutate(cur.age.2019.mon = round((interval(dob.date,
                                                  '2019-01-01') %/% days(1) 
                                         / 30.44), 1))
      #*** NOTE *** DATA INCLUSION CUTOFF
      
    ## f) Extract month from dob.date
      # Use lubridate to extract the month during which a hyena was born
      # and make a new variable  
      luma_data_lf_hist$dob.mon <- month(luma_data_lf_hist$dob.date)
      
    ## g) Create a varialbe, 'migratn.seas.dob,' which indicates if a hyena
      # was born in migration (June 1 - Oct 31)
      luma_data_lf_hist <-  luma_data_lf_hist  %>%
        mutate(migratn.seas.dob = ifelse(dob.mon >= 6 & dob.mon<= 10, 
                                     'migration', 'none'))
      
    ## h) Re-code *nominal* factor (with ordered levels)  
      # Set levels (odering) of migratn.seas.dob variable and sets the reference  
      # level to 'none'
      luma_data_lf_hist <- transform( luma_data_lf_hist,
                                      migratn.seas.dob = 
                                        factor(migratn.seas.dob,
                                               levels = c("none", 
                                                          "migration")))  
 
    ## i) Extract year from dob.date
      # Use lubridate to extract the year during which a hyena was born
      # and make a new variable  
      luma_data_lf_hist$dob.yr <- year(luma_data_lf_hist$dob.date)
      
    ## j) Make a new variable hum.pres
      # create a 3-level ordinal factor indicating human pastoralist presence 
      # (mid - talek hyena born after 2000, mid - fig tree born after 2000,
      # and lo - all other hynenas for which clan is known and birthdate)
      # based Green et. al 2017
      luma_data_lf_hist <- luma_data_lf_hist  %>%
        mutate(hum.pres = case_when(luma_data_lf_hist$dob.event.data %in% 
                                      c("talek", "talek.e", "talek.w") &
                                      luma_data_lf_hist$dob.yr > 2008
                                    ~ c("hi"),
                                    luma_data_lf_hist$dob.event.data %in% 
                                      c("fig.tree") &
                                      luma_data_lf_hist$dob.yr > 2008
                                    ~ c("hi"),
                                    luma_data_lf_hist$dob.event.data %in% 
                                      c("talek", "talek.e", "talek.w") &
                                      luma_data_lf_hist$dob.yr >= 2000 & 
                                      luma_data_lf_hist$dob.yr <= 2008 
                                    ~ c("med"),
                                    luma_data_lf_hist$dob.event.data %in% 
                                      c("fig.tree") &
                                      luma_data_lf_hist$dob.yr >= 2000 &
                                      luma_data_lf_hist$dob.yr <= 2008 
                                    ~ c("med"),
                                    luma_data_lf_hist$dob.event.data %in% 
                                      c("talek", "talek.e", "talek.w", 
                                        "fig.tree") &
                                      luma_data_lf_hist$dob.yr < 2000 
                                    ~ c("low"),
                                    luma_data_lf_hist$dob.event.data %in% 
                                      c("serena.n", "serena.s","happy.zebra",
                                        "mara.river")
                                    ~ c("low"))) %>%
        transform(hum.pres = factor(hum.pres,
                                              levels = c('low', 'med', 'hi'))) 
      
    ## k) Recode number.littermates as level nomial variable (single or twin)
      luma_data_lf_hist <- luma_data_lf_hist  %>%
        transform(number.littermates = factor(number.littermates,
                                           levels = c(0, 1),
                                           labels= c('single', 'twin'))) 
                                  
      
  ### 4.3 Subset / tidy luma_data_lf_hist for lifehistory trait anlayses
    ## a) Subset data to include data to include only hyenas who lived long
      # enough to have a kid (use 24 month cut-off)...deals with left 
      # censorship
      luma_data_1st_part <- luma_data_lf_hist  %>% 
        filter(long.mon >=24 | is.na(long.mon) | 
                 !is.na(age.1st.part.days))
      
    ## b) Add a part.rt.cnsr variable where 0 = no parturiton yet (2019-1-1) 
      # and alive OR died before having a kid; 1 = has had a first parturition
      luma_data_1st_part <-  luma_data_1st_part  %>%
        mutate(part.rt.censr = ifelse(is.na
                                      (luma_data_1st_part$age.1st.part.mon),
                                       0, 1))
      
      
  ### 4.4 Subset /tidy luma_data_lf_hist for survival analyses
    ## a) Subset data to include data when a hyena's disappearance is known or
      # still alive
      luma_data_surv <- luma_data_lf_hist  %>% 
        filter(!grepl('unk*|^double', disappear.event.data)) %>%
        filter(!grepl('mac', hy.id)) ## MANUALLY remove becuase no 
      ## disappearance, but noted poisoned 
      #*** NOTE *** This is a data inclusion cut-off decision. 
      # For survival analysis require that birthdate is known, and that
      # if the hyena dissappeared it is dead or inferred dead.
      
    ## b) add a right censorship varaible 0 = fate - alive; 1 = fate - dead      
      luma_data_surv <- luma_data_surv   %>% 
        mutate(surv.rt.censr = ifelse (is.na(long.mon), 0, 1))
      
    ## c) Add a two.yr.surv variable where 0 = fate - alive at 2 yrs;
      # 1 = fate - dead by 2yrs
      luma_data_surv <- luma_data_surv  %>%
        mutate(two.yr.surv = ifelse((is.na(luma_data_surv$long.days)
                                           | long.days >= 730), 0, 1))
      
      

###############################################################################
##############              5. Univariate analyses               ##############
###############################################################################      
      
  ### 5.1 Overview
      # Visualize data and generate summary statistics for fecal corticosterone 
      # as the outcome and LUMA methylation as the explanatory variable.
        
      
  ### 5.2 Visualize and summarize (and transform as needed) outcome        
    ## a) Histogram outcome (age in months at first parturition)
      # NOTE: excludes right and left censored data among female hyenas
      ggplot(data = luma_data_1st_part, aes(x = age.1st.part.mon)) + 
        geom_histogram(aes(y = ..count..),
                       breaks = seq(20, 60, by = 4), 
                       col = "black",
                       fill = "dark grey") +
        xlim(c(20,60)) +
        geom_vline(aes(xintercept = mean(age.1st.part.mon)),
                   color = "blue", linetype = "dashed", size = 1) + 
        labs(title = "Histogram of age at first parturition") +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        labs(x = "Age at first parturition (months)", y = "Frequency") 
      
    ## b) Density plot outcome (age in months at first parturition)
      ggplot(data = luma_data_1st_part, aes(x = age.1st.part.mon)) + 
        geom_density() +
        geom_vline(aes(xintercept = mean(age.1st.part.mon)),
                   color = "blue", linetype = "dashed", size=1)
      
    ## c) Boxplot outcome (age in months at first parturition)      
      ggplot(data = luma_data_1st_part, aes(y = age.1st.part.mon)) + 
        geom_boxplot() 
      
    ## d) Summarize: Calucate the average age in months at first parturition  
      #  measure for each hyena 
      summry_1st_part <- luma_data_1st_part  %>%
        summarize(n.part = sum(!is.na(age.1st.part.mon)),
                  mean.part.age.mon = round(mean(age.1st.part.mon, 
                                                 na.rm = T), 2),
                  sd.part.age.mon = round(sd(age.1st.part.mon, 
                                             na.rm = T), 2),
                  min.part.age.mon = round(min(age.1st.part.mon, 
                                               na.rm = T), 2),
                  max.part.age.mon = round(max(age.1st.part.mon, 
                                               na.rm = T), 2))
      # save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/summry_1st_part.pdf"), 
          height = 2, width = 9)
      grid.table(summry_1st_part)
      dev.off()   
      

  ### 5.3 Visualize and summarize (and transform as needed) outcome        
    ## a) Histogram outcome survival to two years (age at death in months)
      # NOTE: includes only female hyenas that die before 2 yrs.
      ggplot(data = subset(luma_data_surv, two.yr.surv == 1), 
             aes(x = long.mon)) + 
        geom_histogram(aes(y = ..count..),
                       breaks = seq(0, 24, by = 2), 
                       col = "black",
                       fill = "dark grey") +
        xlim(c(0, 24)) +
        geom_vline(aes(xintercept = mean(long.mon)),
                   color = "blue", linetype = "dashed", size = 1) + 
        labs(title = "Histogram of age at death (among hyenas that are dead
before two years of age)") +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        labs(x = "Age at death (months)", y = "Frequency") 
      
    ## b) Density plot outcome age at death (among female hyenas that are dead
      # before two years of age)
      ggplot(data = subset(luma_data_surv, two.yr.surv == 1), 
             aes(x = long.mon)) + 
        geom_density() +
        geom_vline(aes(xintercept = mean(long.mon)),
                   color = "blue", linetype = "dashed", size=1)
      
    ## c) Boxplot outcome age at death (among female hyenas that are dead
      # before two years of age)     
      ggplot(data = subset(luma_data_surv, two.yr.surv == 1), 
             aes(y = long.mon)) + 
        geom_boxplot() 
      
    ## d) Summarize: Calculate the average age in months at death (among 
      # female hyenas that are deadbefore two years of age) 
      summry_two_yr_surv <- subset(luma_data_surv, two.yr.surv == 1)  %>%
        summarize(n.long.mon = sum(!is.na(long.mon)),
                  mean.long.mon = round(mean(long.mon, 
                                                 na.rm = T), 2),
                  sd.long.mon = round(sd(long.mon, 
                                             na.rm = T), 2),
                  min.long.mon = round(min(long.mon, 
                                               na.rm = T), 2),
                  max.long.mon = round(max(long.mon, 
                                               na.rm = T), 2))
      # save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/summry_two_yr_surv.pdf"), 
          height = 2, width = 7)
      grid.table(summry_two_yr_surv)
      dev.off()
        
    ## e) sqrt transform age at death (among female hyenas that are dead
      # before two years of age)  
      luma_data_surv <- luma_data_surv  %>%
        mutate(sqrt.long.mon = sqrt(luma_data_surv$long.mon))
        
    ## f) Density plot outcome square root of age at death 
      # (among female hyenas that are dead before two years of age)  
      ggplot(data = subset(luma_data_surv, two.yr.surv == 1), 
             aes(x = sqrt.long.mon)) + 
        geom_density() +
        geom_vline(aes(xintercept = mean(sqrt.long.mon)),
                   color = "blue", linetype = "dashed", size=1)
      
    ## g) Natural log transform longevity data
      luma_data_surv <- luma_data_surv  %>%
        mutate(log.long.mon = log(luma_data_surv$long.mon))
      
    ## h) Density plot outcome natural log of age at death (among female 
      # hyenas that are dead before two years of age)  
      ggplot(data = subset(luma_data_surv, two.yr.surv == 1), 
             aes(x = log.long.mon)) + 
        geom_density() +
        geom_vline(aes(xintercept = mean(log.long.mon)),
                   color = "blue", linetype = "dashed", size=1)  
      
      
  ### 5.4 Visualize and summarize (and transform as needed) outcome        
    ## a) Histogram outcome longevity (age at death in months)
      # NOTE: includes only female hyenas that have died by 2019-1-1.
      ggplot(data = subset(luma_data_surv, surv.rt.censr == 1), 
             aes(x = long.mon)) + 
        geom_histogram(aes(y = ..count..),
                       breaks = seq(0, 240, by = 5), 
                       col = "black",
                       fill = "dark grey") +
        xlim(c(0, 240)) +
        geom_vline(aes(xintercept = mean(long.mon)),
                   color = "blue", linetype = "dashed", size = 1) + 
        labs(title = "Histogram of age at death (longevity)") +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        labs(x = "Longevity (months)", y = "Frequency") 
      
    ## b) Density plot outcome age at death (longevity)
      ggplot(data = subset(luma_data_surv, surv.rt.censr == 1), 
             aes(x = long.mon)) + 
        geom_density() +
        geom_vline(aes(xintercept = mean(long.mon)),
                   color = "blue", linetype = "dashed", size=1)
      
    ## c) Boxplot outcome age at death (longevity)
      ggplot(data = subset(luma_data_surv, surv.rt.censr == 1), 
             aes(y = long.mon)) + 
        geom_boxplot() 
      
    ## d) Summarize: Calucate the average age in months at death (longevity) 
      summry_longevity <- subset(luma_data_surv, surv.rt.censr == 1)  %>%
        summarize(n.long.mon = sum(!is.na(long.mon)),
                  mean.long.mon = round(mean(long.mon, 
                                             na.rm = T), 2),
                  sd.long.mon = round(sd(long.mon, 
                                         na.rm = T), 2),
                  min.long.mon = round(min(long.mon, 
                                           na.rm = T), 2),
                  max.long.mon = round(max(long.mon, 
                                           na.rm = T), 2))
      # save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/summry_longevity.pdf"), 
          height = 2, width = 7)
      grid.table( summry_longevity)
      dev.off() 
      
    ## e) Density plot outcome square root of age at death (longevity)
      ggplot(data = subset(luma_data_surv, surv.rt.censr == 1), 
             aes(x = sqrt.long.mon)) + 
        geom_density() +
        geom_vline(aes(xintercept = mean(sqrt.long.mon)),
                   color = "blue", linetype = "dashed", size=1)
      
    ## f) Density plot outcome natural log of age at death (longevity)
      ggplot(data = subset(luma_data_surv, surv.rt.censr == 1), 
             aes(x = log.long.mon)) + 
        geom_density() +
        geom_vline(aes(xintercept = mean(log.long.mon)),
                   color = "blue", linetype = "dashed", size=1)  

      
  ### 5.5 Visualize and summarize additional covariates for life histroy
      # outcome models
    ## a) Migration status ratio summary 
      summry_migratn_1st_part <- luma_data_1st_part %>%
        group_by (migratn.seas.dob) %>%
        summarise(n=n_distinct(hy.id)) %>%
        mutate(freq = n / sum(n))
    # save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/summry_migratn_1st_part.pdf"), 
          height = 3, width = 5)
      grid.table(summry_migratn_1st_part)
      dev.off()   
      
    ## b) Number of littermates ratio summary 
      summry_litmate_1st_part <- luma_data_1st_part %>%
        group_by (number.littermates) %>%
        summarise(n=n_distinct(hy.id)) %>%
        mutate(freq = n / sum(n))
      # save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/summry_litmate_1st_part.pdf"), 
          height = 3, width = 5)
      grid.table(summry_litmate_1st_part)
      dev.off()  
      
    ## c) Human disturbance ratio summary 
      summry_hum_disturb_1st_part <- luma_data_1st_part %>%
        group_by (hum.pres) %>%
        summarise(n=n_distinct(hy.id)) %>%
        mutate(freq = n / sum(n))
      # save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/summry_hum_disturb_1st_part.pdf"), 
          height = 3, width = 5)
      grid.table(summry_hum_disturb_1st_part)
      dev.off()   
      

  ### 5.6 Visualize and summarize additional covariates for survival
      # outcome models
    ## a) Migration status ratio summary 
      summry_migratn_surv <- luma_data_surv %>%
        group_by (migratn.seas.dob) %>%
        summarise(n=n_distinct(hy.id)) %>%
        mutate(freq = n / sum(n))
      # save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/summry_migratn_surv.pdf"), 
          height = 3, width = 5)
      grid.table(summry_migratn_surv)
      dev.off()   
      
    ## b) Number of littermates ratio summary 
      summry_litmate_surv <- luma_data_surv %>%
        group_by (number.littermates) %>%
        summarise(n=n_distinct(hy.id)) %>%
        mutate(freq = n / sum(n))
      # save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/summry_litmate_surv.pdf"), 
          height = 3, width = 5)
      grid.table(summry_litmate_surv)
      dev.off()  
      
    ## c) Human disturbance ratio summary 
      summry_hum_disturb_surv <- luma_data_surv %>%
        group_by (hum.pres) %>%
        summarise(n=n_distinct(hy.id)) %>%
        mutate(freq = n / sum(n))
      # save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/summry_hum_disturb_surv.pdf"), 
          height = 3, width = 5)
      grid.table(summry_hum_disturb_surv)
      dev.off()  
  
      
        
###############################################################################
##############               6. Bivariate analysis               ##############
###############################################################################   
  
  ### 6.1 Check collinearity between continous covariates      
    ## a) Correlation first.meth and mean.meth (this should be high) 
      cor(luma_data_1st_part$first.meth, luma_data_1st_part$mean.meth) 
      
    ## b) Correlation mean.meth and avg.age.dart.mon (if this is low, then 
      # probably don't need control for age) 
      cor(luma_data_1st_part$mean.meth, luma_data_1st_part$avg.age.dart.mon,
          use = "complete.obs")
  
  ### 6.2  Bivariate statistics age at 1st parturition by %CCGG methylation
    ## a) Plot age.1st.part.mon by %CCGG methylation
      ggplot(data = subset(luma_data_1st_part, !is.na(x = mean.meth)),
            aes(x = mean.meth, y = age.1st.part.mon)) +
        geom_point(shape = 1) +
        geom_smooth(method = loess, se = F) + # Add smooth curve best fit lines
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = "Age at first parturition by %CCGG methylation
(excluding right censored data") +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(legend.position = "none") + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = "white")) +
        # add major axes
        theme(axis.line = element_line(colour = "darkgrey", 
                                       size = 1, linetype = "solid")) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face="bold", color="black", 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face="bold", color="black", 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab("Age at 1st parturiton (mon)") +
        xlab("%CCGG methylation")
      
    ## d) Save Plot
      # use ggsave to save the plot
      ggsave("age_part_by_meth_plot.pdf", plot = last_plot(), device = NULL, 
             path = paste0(here(),"/output"), 
             scale = 1, width = 7, height = 5,
             units = c("in"), dpi = 300, limitsize = TRUE)  
      
      
  ### 6.3  Bivariate statistics survival (two yr and longevity) by 
      # %CCGG methylation
    ## a) Plot age.1st.part.mon by %CCGG methylation
      ggplot(data = subset(luma_data_surv, !is.na(x = mean.meth)),
             aes(x = mean.meth, y = log.long.mon)) +
        geom_point(shape = 1) +
        geom_smooth(method = loess, se = F) + # Add smooth curve best fit lines
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = "Age at death (longevity) by %CCGG methylation
(excluding right censored data") +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(legend.position = "none") + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = "white")) +
        # add major axes
        theme(axis.line = element_line(colour = "darkgrey", 
                                       size = 1, linetype = "solid")) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face="bold", color="black", 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face="bold", color="black", 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab("Log longevity (mon)") +
        xlab("%CCGG methylation")
      
    ## b) Save Plot
      # use ggsave to save the plot
      ggsave("longevity_by_meth_plot.pdf", plot = last_plot(), device = NULL, 
             path = paste0(here(),"/output"), 
             scale = 1, width = 7, height = 5,
             units = c("in"), dpi = 300, limitsize = TRUE)  
      
    ## c) Summary stats %CCGG methylation by survive to two yrs (y/n) 
      meth_by_surv_two_yr <- luma_data_surv %>%
        group_by (two.yr.surv) %>%
        summarise (n.samp.id = n(),
                   n.hy.id =  n_distinct(hy.id),
                   avg = round (mean(mean.meth, na.rm = T), 2),
                   median =  round (quantile(mean.meth, 
                                             c(.5), na.rm = T), 2),
                   sd = round (sd(mean.meth, na.rm = T), 2))  
      
    ## d) save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/meth_by_surv_two_yr.pdf"),
          height = 3, width = 5)
      grid.table(meth_by_surv_two_yr)
      dev.off()
        
      
  ### 6.4  Bivariate statistics age at first part by migration status
    ## a) Summary stats age at first part by migration status
      age_1st_part_by_migratn <- luma_data_1st_part %>%
        group_by (migratn.seas.dob) %>%
        summarise (n.samp.id = n(),
                   n.hy.id =  n_distinct(hy.id),
                   avg = round (mean(age.1st.part.mon, na.rm = T), 2),
                   median =  round (quantile(age.1st.part.mon, 
                                             c(.5), na.rm = T), 2),
                   sd = round (sd(age.1st.part.mon, na.rm = T), 2))  
     
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/age_1st_part_by_migratn.pdf"),
          height = 3, width = 5)
      grid.table(age_1st_part_by_migratn)
      dev.off()
      
    ## c) Plot age at first part by migration status
      ggplot(data = subset(luma_data_1st_part, !is.na(x = migratn.seas.dob)), 
             aes(x = migratn.seas.dob, y = age.1st.part.mon, 
                 color = migratn.seas.dob)) + 
        geom_boxplot() +
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = "Age at first parturition (mon) by migration status
(on the date a hyena was born)") +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(legend.position = "none") + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = "white")) +
        # add major axes
        theme(axis.line = element_line(colour = "darkgrey", 
                                       size = 1, linetype = "solid")) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face="bold", color="black", 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face="bold", color="black", 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab("Age at 1st parturition (mon)") +
        xlab("Migration status")
      
    ## d) Save Plot
      # use ggsave to save the plot
      ggsave("age_1st_part_by_migratn_plot.pdf", plot = last_plot(), 
             device = NULL, 
             path = paste0(here(),"/output"), 
             scale = 1, width = 7, height = 5,
             units = c("in"), dpi = 300, limitsize = TRUE)  

  ### 6.5  Bivariate statistics age at first part by number littermates      
    ## a) Summary stats age at first part by number littermates
      age_1st_part_by_litmate <- luma_data_1st_part %>%
        group_by (number.littermates) %>%
        summarise (n.samp.id = n(),
                   n.hy.id =  n_distinct(hy.id),
                   avg = round (mean(age.1st.part.mon, na.rm = T), 2),
                   median =  round (quantile(age.1st.part.mon, 
                                             c(.5), na.rm = T), 2),
                   sd = round (sd(age.1st.part.mon, na.rm = T), 2))  
      
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/age_1st_part_by_litmate.pdf"),
          height = 3, width = 5)
      grid.table(age_1st_part_by_migratn)
      dev.off()
      
    ## c) Plot age at first part by number littermates
      # NOTE: These summaries contain non-independent measures of fecal cort
      ggplot(data = subset(luma_data_1st_part, !is.na(x = number.littermates)), 
             aes(x = number.littermates, y = age.1st.part.mon, 
                 color = number.littermates)) + 
        geom_boxplot() +
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = "Age at first parturition (mon) by number littermates") +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(legend.position = "none") + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = "white")) +
        # add major axes
        theme(axis.line = element_line(colour = "darkgrey", 
                                       size = 1, linetype = "solid")) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face="bold", color="black", 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face="bold", color="black", 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab("Age at 1st parturition (mon)") +
        xlab("Number littermates")
      
    ## d) Save Plot
      # use ggsave to save the plot
      ggsave("age_1st_part_by_litmate_plot.pdf", plot = last_plot(), 
             device = NULL, 
             path = paste0(here(),"/output"), 
             scale = 1, width = 7, height = 5,
             units = c("in"), dpi = 300, limitsize = TRUE) 
   
      
  ### 6.6  Bivariate statistics age at first part by human disturbance    
    ## a) Summary stats age at first part by human disturbance
      age_1st_part_by_human_dstrb <- luma_data_1st_part %>%
        group_by (hum.pres) %>%
        summarise (n.samp.id = n(),
                   n.hy.id =  n_distinct(hy.id),
                   avg = round (mean(age.1st.part.mon, na.rm = T), 2),
                   median =  round (quantile(age.1st.part.mon, 
                                             c(.5), na.rm = T), 2),
                   sd = round (sd(age.1st.part.mon, na.rm = T), 2))  
      
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/age_1st_part_by_human_dstrb.pdf"),
          height = 3, width = 5)
      grid.table( age_1st_part_by_human_dstrb)
      dev.off()
      
    ## c) Plot age at first part by human disturbance
      ggplot(data = subset(luma_data_1st_part, !is.na(x = hum.pres)), 
             aes(x = hum.pres, y = age.1st.part.mon, 
                 color = hum.pres)) + 
        geom_boxplot() +
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = "Age at first parturition (mon) by human disturbance") +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(legend.position = "none") + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = "white")) +
        # add major axes
        theme(axis.line = element_line(colour = "darkgrey", 
                                       size = 1, linetype = "solid")) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face="bold", color="black", 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face="bold", color="black", 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab("Age at 1st parturition (mon)") +
        xlab("Human disturbance")
      
    ## d) Save Plot
      # use ggsave to save the plot
      ggsave("age_1st_part_by_human_dstrb_plot.pdf", plot = last_plot(), 
             device = NULL, 
             path = paste0(here(),"/output"), 
             scale = 1, width = 7, height = 5,
             units = c("in"), dpi = 300, limitsize = TRUE) 
      

  ### 6.7  Bivariate statistics survival by migration status
    ## a) Summary stats survival by migration status
      long_by_migratn <- luma_data_surv %>%
        group_by (migratn.seas.dob) %>%
        summarise (n.samp.id = n(),
                   n.hy.id =  n_distinct(hy.id),
                   avg = round (mean(log.long.mon, na.rm = T), 2),
                   median =  round (quantile(log.long.mon, 
                                             c(.5), na.rm = T), 2),
                   sd = round (sd(log.long.mon, na.rm = T), 2))
      
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/long_by_migratn.pdf"),
          height = 3, width = 5)
      grid.table(long_by_migratn)
      dev.off()
      
    ## c) Plot survival by migration status
      ggplot(data = subset(luma_data_surv, !is.na(x = migratn.seas.dob)), 
             aes(x = migratn.seas.dob, y = log.long.mon, 
                 color = migratn.seas.dob)) + 
        geom_boxplot() +
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = "Longevity (mon) by migration status
(on the date a hyena was born)") +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(legend.position = "none") + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = "white")) +
        # add major axes
        theme(axis.line = element_line(colour = "darkgrey", 
                                       size = 1, linetype = "solid")) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face="bold", color="black", 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face="bold", color="black", 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab("Log longevity (mon)") +
        xlab("Migration status")
      
    ## d) Save Plot
      # use ggsave to save the plot
      ggsave("long_by_migratn_plot.pdf", plot = last_plot(), 
             device = NULL, 
             path = paste0(here(),"/output"), 
             scale = 1, width = 7, height = 5,
             units = c("in"), dpi = 300, limitsize = TRUE)  
      
      
  ### 6.8  Bivariate statistics survival by number littermates      
    ## a) Summary stats survival by number littermates
      long_by_litmate <- luma_data_surv %>%
        group_by (number.littermates) %>%
        summarise (n.samp.id = n(),
                   n.hy.id =  n_distinct(hy.id),
                   avg = round (mean(log.long.mon, na.rm = T), 2),
                   median =  round (quantile(log.long.mon, 
                                             c(.5), na.rm = T), 2),
                   sd = round (sd(log.long.mon, na.rm = T), 2))  
      
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/long_by_litmate.pdf"),
          height = 3, width = 5)
      grid.table(long_by_migratn)
      dev.off()
      
    ## c) Plot survival by number littermates
      ggplot(data = subset(luma_data_surv, !is.na(x = number.littermates)), 
             aes(x = number.littermates, y = log.long.mon, 
                 color = number.littermates)) + 
        geom_boxplot() +
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = "Longevity (mon) by number littermates") +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(legend.position = "none") + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = "white")) +
        # add major axes
        theme(axis.line = element_line(colour = "darkgrey", 
                                       size = 1, linetype = "solid")) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face="bold", color="black", 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face="bold", color="black", 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab("Log longevity (mon)") +
        xlab("Number littermates")
      
    ## d) Save Plot
      # use ggsave to save the plot
      ggsave("long_by_litmate_plot.pdf", plot = last_plot(), 
             device = NULL, 
             path = paste0(here(),"/output"), 
             scale = 1, width = 7, height = 5,
             units = c("in"), dpi = 300, limitsize = TRUE) 
      
      
  ### 6.9  Bivariate statistics survival by human disturbance    
    ## a) Summary stats survival by human disturbance
      long_by_human_dstrb <- luma_data_surv %>%
        group_by (hum.pres) %>%
        summarise (n.samp.id = n(),
                   n.hy.id =  n_distinct(hy.id),
                   avg = round (mean(log.long.mon, na.rm = T), 2),
                   median =  round (quantile(log.long.mon, 
                                             c(.5), na.rm = T), 2),
                   sd = round (sd(log.long.mon, na.rm = T), 2))  
      
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/long_by_human_dstrb.pdf"),
          height = 3, width = 5)
      grid.table(long_by_human_dstrb)
      dev.off()
      
    ## c) Plot survival by human disturbance
      ggplot(data = subset(luma_data_surv, !is.na(x = hum.pres)), 
             aes(x = hum.pres, y = log.long.mon, 
                 color = hum.pres)) + 
        geom_boxplot() +
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = "Longevity (mon) by human disturbance") +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        theme(legend.position = "none") + # remove legend
        theme(axis.ticks = element_blank()) + # remove axis ticks
        # remove background color
        theme(panel.background = element_rect(fill = "white")) +
        # add major axes
        theme(axis.line = element_line(colour = "darkgrey", 
                                       size = 1, linetype = "solid")) + 
        # change axes font style, color, size, angle, and margin
        theme(axis.text.x = element_text(face="bold", color="black", 
                                         size=18, angle=0,
                                         margin = margin(t = 0, r = 0, 
                                                         b = 10, l = 0)),
              axis.text.y = element_text(face="bold", color="black", 
                                         size=18, angle=0, 
                                         margin = margin(t = 0, r = 0, 
                                                         b = 0, l = 10))) +
        ylab("Log longevity (mon)") +
        xlab("Human disturbance")
      
    ## d) Save Plot
      # use ggsave to save the plot
      ggsave("long_by_human_dstrb_plot.pdf", plot = last_plot(), 
             device = NULL, 
             path = paste0(here(),"/output"), 
             scale = 1, width = 7, height = 5,
             units = c("in"), dpi = 300, limitsize = TRUE) 
      

       
###############################################################################
##############              7: Data transformations              ##############
###############################################################################  
    
  ### 7.1 Center and transform independent variables
    ## a) center function based on column means    
      center_fxn_cols <- function(df) {
        xcenter = colMeans(df, na.rm = T)
        df - rep(xcenter, rep.int(nrow(df), ncol(df)))
      }
      
    ## b) Center mean.meth
      luma_data_1st_part$mean.meth.cnt <- 
        as.numeric(scale(luma_data_1st_part$mean.meth, scale = F))
      
      luma_data_surv$mean.meth.cnt <- 
        as.numeric(scale(luma_data_surv$mean.meth, scale = F))
      
  
  ### 7.2 Bin %CCGG methylation based on non-linear relationship with 
      # age at first parturition
    ## a) Create quartiles of mean.meth
      # Use dplyr to make 5 levels (approximately same
      # number in each level)
        luma_data_1st_part <- luma_data_1st_part  %>%
          mutate(mean.meth.quint = ntile(mean.meth, 5))

    ## b) Create a nominal factor and rename and re-order the levels to
        # sets the reference level for lowest methylation values
          luma_data_1st_part <- luma_data_1st_part  %>%
            transform(mean.meth.quint = factor(mean.meth.quint,
                                               levels = c(1, 2, 3, 4, 5),
                                               labels= c('Q1 (lowest)',
                                                         'Q2','Q3',
                                                         'Q4', 'Q5')))
          
    ## c) Summary stats %CCGG methylation by survive to two yrs (y/n) 
      age_1st_part_by_meth_quint <- luma_data_1st_part %>%
        group_by (mean.meth.quint) %>%
        summarise (n.samp.id = n(),
                   n.hy.id =  n_distinct(hy.id),
                   avg = round (mean(age.1st.part.mon, na.rm = T), 2),
                   median =  round (quantile(age.1st.part.mon,
                                             c(.5), na.rm = T), 2),
                   sd = round (sd(age.1st.part.mon, na.rm = T), 2))  
          
    ## d) save the data frame of summary stats out as a pdf into output file
      pdf(paste0(here(),"/output/age_1st_part_by_meth_quint.pdf"),
          height = 3, width = 5)
      grid.table(age_1st_part_by_meth_quint)
          dev.off()
          
    ## e) Plot age at first parturition by methylation quintiles
        ggplot(data = subset(luma_data_1st_part, !is.na(x = mean.meth.quint)),
               aes(x = mean.meth.quint, y = age.1st.part.mon,
                   color = mean.meth.quint)) +
          geom_boxplot() +
          theme(text = element_text(size=20))+
          scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
          labs(title = "Age at first parturition (mon) by %CCGG methylation") +
          theme(plot.title = element_text(hjust = 0.5)) + # center title
          theme(legend.position = "none") + # remove legend
          theme(axis.ticks = element_blank()) + # remove axis ticks
          # remove background color
          theme(panel.background = element_rect(fill = "white")) +
          # add major axes
          theme(axis.line = element_line(colour = "darkgrey",
                                         size = 1, linetype = "solid")) + 
          # change axes font style, color, size, angle, and margin
          theme(axis.text.x = element_text(face="bold", color="black",
                                           size=18, angle=0,
                                           margin = margin(t = 0, r = 0,
                                                           b = 10, l = 0)),
                axis.text.y = element_text(face="bold", color="black",
                                           size=18, angle=0, 
                                            margin = margin(t = 0, r = 0,
                                                            b = 0, l = 10))) +
          ylab("Age at 1st parturition (mon)") +
          xlab("%CCGG methylation (quintiles)")
          
    ## f) Save Plot
      # use ggsave to save the plot
        ggsave("age_1st_part_by_meth_quint_plot.pdf", plot = last_plot(), 
              device = NULL, 
              path = paste0(here(),"/output"), 
              scale = 1, width = 7, height = 5,
              units = c("in"), dpi = 300, limitsize = TRUE) 
          
         
  ### 7.3 Bin %CCGG methylation based on non-linear relationship with 
      # longevity
    ## a) Create quartiles of mean.meth
      # Use dplyr to make 4 levels (approximately same
      # number in each level)
        luma_data_surv <- luma_data_surv  %>%
          mutate(mean.meth.quart = ntile(mean.meth, 4))
        
    ## b) Create a nominal factor and rename and re-order the levels to
      # sets the reference level for lowest methylation values
        luma_data_surv <- luma_data_surv  %>%
          transform(mean.meth.quart = factor(mean.meth.quart,
                                             levels = c(1, 2, 3, 4),
                                             labels= c('Q1 (lowest)',
                                                       'Q2','Q3',
                                                       'Q4')))
        
    ## c) Summary stats %CCGG methylation by survive to two yrs (y/n) 
        long_by_meth_quint <- luma_data_surv %>%
          group_by (mean.meth.quart) %>%
          summarise (n.samp.id = n(),
                     n.hy.id =  n_distinct(hy.id),
                     avg = round (mean(long.mon, na.rm = T), 2),
                     median =  round (quantile(long.mon,
                                               c(.5), na.rm = T), 2),
                     sd = round (sd(long.mon, na.rm = T), 2))  
        
    ## d) save the data frame of summary stats out as a pdf into output file
        pdf(paste0(here(),"/output/long_by_meth_quart.pdf"),
            height = 3, width = 5)
        grid.table(long_by_meth_quart)
        dev.off()
        
    ## e) Plot age at first parturition by methylation quintiles
        ggplot(data = subset(luma_data_surv, !is.na(x = mean.meth.quart)),
               aes(x = mean.meth.quart, y = log.long.mon,
                   color = mean.meth.quart)) +
          geom_boxplot() +
          theme(text = element_text(size=20))+
          scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
          labs(title = "Longevity (mon) by %CCGG methylation") +
          theme(plot.title = element_text(hjust = 0.5)) + # center title
          theme(legend.position = "none") + # remove legend
          theme(axis.ticks = element_blank()) + # remove axis ticks
          # remove background color
          theme(panel.background = element_rect(fill = "white")) +
          # add major axes
          theme(axis.line = element_line(colour = "darkgrey",
                                         size = 1, linetype = "solid")) + 
          # change axes font style, color, size, angle, and margin
          theme(axis.text.x = element_text(face="bold", color="black",
                                           size=18, angle=0,
                                           margin = margin(t = 0, r = 0,
                                                           b = 10, l = 0)),
                axis.text.y = element_text(face="bold", color="black",
                                           size=18, angle=0, 
                                           margin = margin(t = 0, r = 0,
                                                           b = 0, l = 10))) +
          ylab("Log longevity (mon)") +
          xlab("%CCGG methylation (quartiles)")
        
    ## f) Save Plot
      # use ggsave to save the plot
        ggsave("long_by_meth_quart_plot.pdf", plot = last_plot(), 
               device = NULL, 
               path = paste0(here(),"/output"), 
               scale = 1, width = 7, height = 5,
               units = c("in"), dpi = 300, limitsize = TRUE)  
            
            

###############################################################################
##############           8. Model luma and life history          ##############
###############################################################################
      
  ### 8.1 Age at first parturition: Kaplan-Meier
    ## a) Standard Kaplan-Meier survival probability
      km_1st_part <- survfit(Surv(age.1st.part.mon, part.rt.censr) ~ 1, 
                        data = luma_data_1st_part)
      
      # print model summary estimates at day 1, day 30 - day 90 and at
      # 90 day intervals after that
      summary(km_1st_part, times = c(1,30,60,90*(1:10)))
      
    ## b) Use ggfortify to plot surival to two years
      autoplot(km_1st_part)
      
    ## c) Standard Kaplan-Meier survival probability by toxo status
      km_age_1st_part_luma <- survfit(Surv(age.1st.part.mon, part.rt.censr) 
                             ~ mean.meth.quint, 
                             data = luma_data_1st_part)
      
      # print model summary estimates at month 1, 10,20 - and at
      # 10 mont intervals after that until 50
      summary(km_age_1st_part_luma, times = c(1, 5*(1:10)))
      
    ## d) Use ggfortify to plot surival to two years
      autoplot(km_age_1st_part_luma)
      
      
  ### 8.2 Age at first parturition: Cox-Proportional Hazards unadjusted
    ## a) Cox-Proportional Hazards age at first parturition probability by 
      # mean.meth ***methylation Continuous*** unadjusted models
      cox_1st_part_unadjst_cont <- coxph(Surv(age.1st.part.mon, part.rt.censr) 
                                ~ mean.meth, 
                                data = luma_data_1st_part)
      
      # print model summary estimates 
      summary(cox_1st_part_unadjst_cont)
      
    ## b) Create survival curves 
      # Use survfit to create survival curves for Cox P. H.
      cox_1st_part_unadjst_cont_fit <- survfit(cox_1st_part_unadjst_cont,
                                       data=luma_data_1st_part)
      # Use ggfortify to plot surival to age at first parturition
      autoplot(cox_1st_part_unadjst_cont_fit)
      # Use survminer to plot surival to age at first parturition
      ggsurvplot(cox_1st_part_unadjst_cont_fit)

    ## c) Cox-Proportional Hazards age at first parturition probability by 
      # mean.meth.quint unadjusted models
      cox_1st_part_unadjst <- coxph(Surv(age.1st.part.mon, part.rt.censr) 
                                    ~ mean.meth.quint, 
                                    data = luma_data_1st_part)
      
      # print model summary estimates 
      summary(cox_1st_part_unadjst)
      
    ## d) Create survival curves 
      # Use survfit to create survival curves for Cox P. H.
      cox_1st_part_unadjst_fit <- survfit(cox_1st_part_unadjst,
                                          data=luma_data_1st_part)
      # Use ggfortify to plot surival to age at first parturition
      autoplot(cox_1st_part_unadjst_fit)
      # Use survminer to plot surival to age at first parturition
      ggsurvplot(cox_1st_part_unadjst_fit)
      
      
  ### 8.3 Age at first parturition: Cox-Proportional Hazards adjusted
    ## a) Cox-Proportional Hazards age at first parturition by mean.methylation
      # ***methylation Continuous*** and adjusted for additional covariates
      cox_1st_part_adjst_cont <- coxph(Surv(age.1st.part.mon, part.rt.censr) 
                                    ~ mean.meth + migratn.seas.dob + 
                                      number.littermates + hum.pres, 
                                    data = luma_data_1st_part)

      # print model summary 
      summary(cox_1st_part_adjst_cont)
      
    ## b) Cox-Proportional Hazards age at first parturition by mean.methylation
      # and adjusted for additional covariates
      cox_1st_part_adjst <- coxph(Surv(age.1st.part.mon, part.rt.censr) 
                                  ~ mean.meth.quint + migratn.seas.dob + 
                                    number.littermates + hum.pres, 
                                  data = luma_data_1st_part)
      
      # print model summary 
      summary(cox_1st_part_adjst)

      
  # ### #.# Longevity: Aalen's Additive regression (for time-varying predictors)
  #   ## a) Aalen's Additive regression survival probability by toxo in 
  #     # unadjusted models
  #     #*** NOTE: READ MORE ABOUT THIS ***#
  #     aar_1st_part_unadjst <- aareg(Surv(age.1st.part.mon, part.rt.censr) 
  #                                   ~ mean.meth, 
  #                                   data = luma_data_1st_part)
  #     
  #     # print model summary estimates 
  #     summary(aar_1st_part_unadjst)
  #     
  #   ## b) Create survival curves 
  #     # Use survfit to create survival curves for Aalen's Add. Reg.
  #     aar_1st_part_unadjst_fit <- survfit(aar_1st_part_unadjst)
  #     # Use ggfortify to plot surival to two years
  #     autoplot(aar_1st_part_unadjst)
  #     # Use survminer to plot surival to two years
  #     ggsurvplot(aar_1st_part_unadjst_fit)
  #     
  #   ## c) Aalen's Additive regression survival probability by toxo in adjusted
  #     # models
  #     aar_long_adjst <- aareg(Surv(age.1st.part.mon, part.rt.censr)
  #                             ~ mean.meth + migratn.seas.dob + 
  #                               number.littermates + hum.pres,
  #                             data = luma_data_1st_part)
  # 
  #     # print model summary estimates 
  #         summary(aar_long_adjst) 
      

  ### 8.4 Longevity: Kaplan-Meier
    ## a) Standard Kaplan-Meier survival probability
      km_long <- survfit(Surv(long.mon, surv.rt.censr) ~ 1, 
                             data = luma_data_surv)
      
      # print model summary estimates at day 1, day 30 - day 90 and at
      # 90 day intervals after that
      summary(km_long, times = c(1,30,60,90*(1:10)))
      
    ## b) Use ggfortify to plot surival to two years
      autoplot(km_long)
      
    ## c) Standard Kaplan-Meier survival probability by luma
      km_long_luma <- survfit(Surv(long.mon, surv.rt.censr) 
                             ~ mean.meth.quart, 
                             data=luma_data_surv)
      
      # print model summary estimates at day 1, day 30 - day 90 and at
      # 90 day intervals after that
      summary(km_long_luma, times = c(1,6*(1:12)))
      
    ## d) Use ggfortify to plot surival to two years
      autoplot(km_long_luma)

      
  ### 8.5 Longevity: Cox-Proportional Hazards unadjusted  
    ## a) Cox-Proportional Hazards logenvity (probability of survival age) by 
      # mean.meth ***methylation Continuous*** unadjusted models
      cox_long_unadjst_cont <- coxph(Surv(long.mon, surv.rt.censr) 
                                    ~ mean.meth, 
                                    data = luma_data_surv)
      
      # print model summary estimates 
      summary(cox_long_unadjst_cont)
      ggforest(cox_long_unadjst_cont, data = luma_data_surv) # graph effects
      
    ## b) Create survival curves 
      # Use survfit to create survival curves for Cox P. H.
      cox_long_unadjst_cont_fit <- survfit(cox_long_unadjst_cont,
                                          data=luma_data_surv)
      # Use ggfortify to plot surival to age at first parturition
      autoplot(cox_long_unadjst_cont_fit)
      # Use survminer to plot surival to age at first parturition
      ggsurvplot(cox_long_unadjst_cont_fit)
      
    ## c) Cox-Proportional Hazards logenvity (probability of survival age) by 
      # mean.meth.quart unadjusted models
      cox_long_unadjst <- coxph(Surv(long.mon, surv.rt.censr) 
                                     ~ mean.meth.quart, 
                                     data = luma_data_surv)
      
      # print model summary estimates 
      summary(cox_long_unadjst)
      ggforest(cox_long_unadjst, data = luma_data_surv) # graph effects
      
    ## d) Create survival curves 
      # Use survfit to create survival curves for Cox P. H.
      cox_long_unadjst_fit <- survfit(cox_long_unadjst,
                                           data=luma_data_surv)
      # Use ggfortify to plot surival to age at first parturition
      autoplot(cox_long_unadjst_fit)
      # Use survminer to plot surival to age at first parturition
      ggsurvplot(cox_long_unadjst_fit)
      
      
      
  ### 8.5 Longevity: Cox-Proportional Hazards adjusted  
    ## a) Cox-Proportional Hazards longevity by mean.methylation
      # ***methylation Continuous*** and adjusted for additional covariates
      cox_long_adjst_cont <- coxph(Surv(long.mon, surv.rt.censr) 
                                  ~ mean.meth + migratn.seas.dob + 
                                    number.littermates + hum.pres, 
                                  data = luma_data_surv)
      
      # print model summary 
      summary(cox_long_adjst_cont)
      ggforest(cox_long_adjst_cont, data = luma_data_surv) # graph effects
      
    ## b) Create survival curves 
      # Use survfit to create survival curves for Cox P. H.
      cox_long_adjst_fit_cont <- survfit(cox_long_adjst_cont,
                                      data=luma_data_surv)
   
      
      
    ## c) Cox-Proportional Hazards longevity by mean.methylation
      # ***methylation Continuous*** and adjusted for additional covariates
      cox_long_adjst <- coxph(Surv(long.mon, surv.rt.censr) 
                              ~ mean.meth.quart + migratn.seas.dob + 
                                number.littermates + hum.pres, 
                              data = luma_data_surv)
      
      # print model summary 
      summary(cox_long_adjst)
      ggforest(cox_long_adjst, data = luma_data_surv) # graph effects
      
    ## d) Create survival curves 
      # Use survfit to create survival curves for Cox P. H.
      cox_long_adjst_fit <- survfit(cox_long_adjst,
                                    data=luma_data_surv)
    
      

  ### 8.6 Two year survival: logistic regression 
    ## a) Survival to two years by %CCGG methylation: unadjusted  
      glm_surv_2yr_luma_unadjst <- glm(two.yr.surv ~ mean.meth, 
                               data = luma_data_surv, family = binomial) 
      summary(glm_surv_2yr_luma_unadjst)    # model summary (log odds scale)
      confint(glm_surv_2yr_luma_unadjst)    # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(glm_surv_2yr_luma_unadjst), 
                 confint (glm_surv_2yr_luma_unadjst)))
    
    ## b) Survival to two years by %CCGG methylation: adjusted  
      glm_surv_2yr_luma_adjst <- glm(two.yr.surv ~ mean.meth + 
                                        number.littermates +
                                       hum.pres,
                                       data = luma_data_surv, 
                                       family = binomial) 
      summary(glm_surv_2yr_luma_adjst)    # model summary (log odds scale)
      confint(glm_surv_2yr_luma_adjst)    # 95% CIs (log odds scale)
      
      # exponentiate estimates to get onto odds scale
      exp(cbind (O.R. = coef(glm_surv_2yr_luma_adjst), 
                 confint (glm_surv_2yr_luma_adjst)))
      
      
      
###############################################################################
##############                9. Export data files               ##############
###############################################################################
      
  ### 9.1 Export data to csv overview     
      # Save and export analysis ready tables as a .cvs spreadsheet.
      # Files are saved in the 'data' folder in the working directory.
     
       
  ### 9.2 Generate File Names
      # For each table that will be saved as a .csv file, first generate a 
      # file name to save each table
      # here, to paste the folder path, followed by the file name 
    ## a) File name for luma data used in first parturition analysess
      csv.file.name.luma.1part.data <- paste0(here("data", 
                                             "luma_data_1st_part.csv")) 
      
    ## b) File name for luma data used in survival analyses
      csv.file.name.luma.surv.data <- paste0(here("data", 
                                                    "luma_data_surv.csv"))       
    
    ## c) File name for intermediate luma data that includes cubs and subs
      csv.file.name.luma.cubsub.data <- paste0(here("data", 
                                            "luma_data_cubsub.csv")) 
      
    ## d) File name for intermediate luma data that includes cub and sub females
      csv.file.name.luma.lf.hist <- paste0(here("data", 
                                                 "luma_data_lf_hist.csv")) 
      
    
  ### 9.3 Export explanatory/predictor variables to txt      
      # Save data frame as a .csv file (a spreadsheet/table) into the 
      # output data folder in the working directory.
    ## a) Save luma data used in first parturition analysess
      write.csv (luma_data_1st_part, file = csv.file.name.luma.1part.data)
      
    ## b) Save luma data used in survival analyses
      write.csv (luma_data_surv, file = csv.file.name.luma.surv.data)
      
    ## c) Save intermediate luma data that includes cubs and subs
      write.csv (luma_data_cubsub, file = csv.file.name.luma.cubsub.data)
      
    ## d) Save intermediate luma data that includes cub and sub females
      write.csv (luma_data_lf_hist, file = csv.file.name.luma.lf.hist)
      
      
   
        
      
   
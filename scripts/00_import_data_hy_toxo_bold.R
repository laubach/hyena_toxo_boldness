################################################################################
#############   Toxoplasma gondii infections are associated with   #############
#############     costly boldness toward felids in a wild host     #############
#############                                                      #############
#############                   0. Data Import                     #############
#############                                                      #############
#############          By: Zach Laubach and Eben Gerring           #############
#############               created: 12 July 2018                  #############
#############            last modified: 18 April 2021              #############
################################################################################

### PURPOSE: Load T. gondii diagnosis data, hyena lion interaction data, and
           # Access Fisi backend from MHP R package.


  # Code Blocks
    # 1: Configure workspace
    # 2: Import data
    # 3: Export data files
  

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

      
  ### 1.2 Install and load Mara Hyena Project package 
    ## a) Load the Mara Hyena Project packages, which install from a private 
      # github repo. To use a static version rather than living version, 
      # specify a version number in the update tables command. 

      # hyena data tables from Access backend
        library('hyenadata')
      
      # e.g., hyenadata::update_tables('1.2.83')
      #hyenadata::update_tables()
      
      # hyena package tools / functions
        library('hyenatools')
      

  ### 1.3 Install and load CRAN packages
    ## a) Data Manipulation and Descriptive Stats Packages
      
      # load tidyverse packages
        library ('tidyverse')
        
      # load here packages
        library ('here')
      
           
  ### 1.4 Get Version and Session Info
    ## a) R and Package versions
      R.Version()
      sessionInfo()
    
      # Developed in:   
      # R version 4.0.2 (2020-06-22)
      # Platform: x86_64-apple-darwin17.0 (64-bit)
      # Running under: macOS Catalina 10.15.7
      
      # Packages and versions attached in this and subsequent R scripts
      # Packages used included:
      # car_3.0-10, aod_1.3.1, effects_4.2-0car, Data_3.0-4, emmeans_1.5.4,     
      # bbmle_1.0.23.1, lme4_1.1-26, Matrix_1.3-2, broom.mixed_0.2.6, 
      # broom_0.7.4, dotwhisker_0.5.0, gridExtra_2.3, here_1.0.1, forcats_0.5.1     
      # stringr_1.4.0, dplyr_1.0.4, purrr_0.3.4, readr_1.4.0, tidyr_1.1.2, 
      # tibble_3.0.6, ggplot2_3.3.3, tidyverse_1.3.0  
  
    ## b) Note the final version of the hyenadata package
      packageVersion('hyenadata')
    
      # ‘1.2.88’
    
    
 
###############################################################################
##############                  2. Import data                   ##############
###############################################################################    

  ### 2.1 Import toxo and hyena lion intx. data files
    ## a) Import toxo_diagnostics data, which has undergone QAQC 
      toxo_data <- read_csv(here('data/toxo_diagnostics.csv'))
  
      
    ## b) Import hyena distance from lions data
      lion_hy_dist_raw <- read_csv(here('data/LHDistances.csv'))
      
    ## c) Import cub distance from lions sensitivity data
      cub_dist_sens <- read_csv(here('data/lion_hy_dist_cub_sens.csv'))
      

  ### 2.2 Import Access Fisi data files
    ## a) Import Access data backend from Mara Hyena Project data package
      # Use hyenadata package 'load_all_tables' function to load all tables
      #hyenadata::load_all_tables()
      
    ## b) load specific Access tables
      data(tblHyenas)
      # rename data frame
      hyenas <- tblHyenas
      
      data(tblLifeHistory.wide)
      # rename data frame
      life_hist <- tblLifeHistory.wide  
      
      data(tblFemaleRanks)
      # rename data frame
      female_ranks <- tblFemaleRanks
      
      # load(here('data/male_ranks.RData'))
      # # rename data frame
      # male_ranks <- ranks
      
      data(tblSessions)
      # rename data frame
      sessions <- tblSessions
      

      
##############################################################################
##############                3. Export data files               ##############
###############################################################################
      
  ### 3.1 Export data to an RData file     
    ## a) Save and export raw data tables 
      # Files are saved in the 'data' folder in the working directory as an
      # RData file.
      save(file = here('data/01_raw_data_hy_toxo_bold.RData'), 
           list = c('toxo_data','lion_hy_dist_raw', 'hyenas', 
                    'life_hist', 'female_ranks', 'cub_dist_sens',
                    'sessions'))
      
    ## b) Unload hyenadata package
      detach('package:hyenadata', unload= TRUE)    
      
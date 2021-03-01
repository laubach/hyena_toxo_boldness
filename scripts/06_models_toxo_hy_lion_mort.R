################################################################################
#############   Toxoplasma gondii infections are associated with   #############
#############     costly boldness toward felids in a wild host     #############
#############                                                      #############
#############      6. Models: Toxo. and mortality from lions       #############
#############                                                      #############
#############           By: Zach Laubach and Eben Gerring          #############
#############                 created: 12 July 2018                #############
#############             last modified: 1 March 2021              #############
################################################################################

### PURPOSE: Models of lion caused hyena mortality by  
           # T. gondii infection status 


  # Code Blocks
    # 1: Configure workspace
    # 2: Import data
    # 3: Lion caused mortality models

  

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
      
    ## b) Graph Plotting and Visualization Packages
      # load ggplot2 packages
        library ('ggplot2')
        
    ## c) Modeling Packages
      # load broom packages
        library ('broom')   

      # load bbmle packages
        library ('bbmle')

      # load emmeans packages
        library ('emmeans')

      # load effects packages
        library ('effects')

      # load aod packages (used to for Wald test)
       library ('aod')
      
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

    
  ### 1.3 Get Version and Session Info
    ## a) R and Package versions
    R.Version()
    sessionInfo()
    
    # Developed in:   
    # R version 4.0.2 (2020-06-22)
    # Platform: x86_64-apple-darwin17.0 (64-bit)
    # Running under: macOS Catalina 10.15.7
    
    
    
###############################################################################
##############                  2. Import data                   ##############
###############################################################################    
    
  ### 2.1 Load RData
    ## a) Load RData (diognotistics, hy lion intx., and hyena data base)
      load(here('data/06_toxo_hy_mort_data.RData'))
    
    
    
###############################################################################
##############          3. Lion caused mortality models          ##############
###############################################################################

  ### 3.1 Mortality lions vs other: toxo_status
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
      mortality_table <- matrix(c(2, 14, 6, 13), 
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
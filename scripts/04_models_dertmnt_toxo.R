################################################################################
#############   Toxoplasma gondii infections are associated with   #############
#############     costly boldness toward felids in a wild host     #############
#############                                                      #############
#############            4. Models: Determinants of Toxo.          #############
#############                                                      #############
#############           By: Zach Laubach and Eben Gerring          #############
#############                 created: 12 July 2018                #############
#############              last modified: 11 Feb. 2021             #############
################################################################################

### PURPOSE: Model the relationship between demographic and ecological and  
           # T. gondii infection in hyenas 


  # Code Blocks
    # 1: Configure workspace
    # 2: Import data
    # 3: Determinants of T. gondii infection models  

  

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

      # load gridExtra packages
        library ('gridExtra')

      # load dotwhisker packages
        library ('dotwhisker')
        
    ## c) Modeling Packages
      # load broom packages
        library ('broom')   

      # load broom.mixed packages
        library ('broom.mixed')   

      # load lme4 packages
        library ('lme4')

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
      load(here('data/02_toxo_detrmnt_data.RData'))
    
    
    
###############################################################################
##############   3. Determinants of T. gondii infection models   ##############
###############################################################################
    
  ### 3.1 Toxo by sex     
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
      sex.log.adj <- glm(toxo.status ~ sex + age.cat.dart + hum.pop.den 
                         , 
                         data = toxo_data,
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
    

  ### 3.2 Toxo by age 
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
      # Wald Chi-square test of significance using 'aod' overal p-diff
      wald.test(b = coef(age.cat.dart.log.adj), 
                Sigma = vcov(age.cat.dart.log.adj), 
                Terms = 2:3) 

    
  ### 3.3 Toxo by standardized rank 
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
      exp(cbind (O.R. = coef(sub.adult.stanrank.log),
                 confint (sub.adult.stanrank.log)))
      
      # Wald Chi-square test of significance using 'aod'
      wald.test(b = coef(sub.adult.stanrank.log),
                Sigma = vcov(sub.adult.stanrank.log), Terms = 2)
    
    ## c) Unadjusted logistic regression toxo_status by stan.rank.dart among cubs  
      cub.stanrank.log <- glm(toxo.status ~ stan.rank.dart,
                              subset(toxo_data_cub,
                                     !is.na(x = stan.rank.dart)),
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
                                age.cat.dart + hum.pop.den + sex  
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
    
    
  ### 3.4 Toxo by human disturbance   
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
    
    
    
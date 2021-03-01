################################################################################
#############   Toxoplasma gondii infections are associated with   #############
#############     costly boldness toward felids in a wild host     #############
#############                                                      #############
#############       5. Models: Toxo. and distance from lions       #############
#############                                                      #############
#############           By: Zach Laubach and Eben Gerring          #############
#############                 created: 12 July 2018                #############
#############             last modified: 26 Feb. 2021              #############
################################################################################

### PURPOSE: Models assessing effect of T. gondii infection status on
           # hyena approach distance from lions 


  # Code Blocks
    # 1: Configure workspace
    # 2: Import data
    # 3: Cub distance from lions models
    # 4: Sub. and Adult distance from lions models

  

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
      load(here('data/05_toxo_hy_lion_dist_data.RData'))
    
    
    
###############################################################################
##############         3. Cub distance from lions models         ##############
###############################################################################

  ### 3.1 Boldness towards lions: CUB approach lion distance by toxo status
    
    #** NOTE: all animals darted as cubs are from the low.hum.disturb period
    #** hyena, JAB had multiple approach distances from lions, so approach
    # distance and the age in months are averaged
    
    ## a) Unadjusted model cub data: min. approach dist. from lions 
      # by toxo status
      min.dist.unadj.cub <- glm(sqrt.dist ~ toxo.status,
                                data = lion_hy_dist_toxo_cub,
                                family = gaussian)
      
    ## b) Parameter estimates
      summary(min.dist.unadj.cub)  # model parameter estimates
      confint(min.dist.unadj.cub)  # 95% CIs
      plot(min.dist.unadj.cub) # view fitted vs residuals
      
      
    ## c) Adjusted model cub data: lifestage min. approach dist. from lions 
      # by toxo status
      min.dist.adj.cub <- glm(sqrt.dist ~ toxo.status + sex
                              + age.mon.lion
                              #+ mode.fd.pres    # sensitivity analyses
                              #* toxo.status     # sensitivity analyses
                              #+ male.lion.pres  # sensitivity analyses
                              #* toxo.status     # sensitivity analyses
                              #+ avg.stan.rank   # sensitivity analyses
                              ,
                              data = lion_hy_dist_toxo_cub, 
                              family = gaussian)
      
    ## d) Parameter estimates
      summary(min.dist.adj.cub)  # model parameter estimates
      confint(min.dist.adj.cub)  # 95% CIs
      plot(min.dist.adj.cub) # view fitted vs residuals  
      
      # Use emmeans to estimate marginal means
      min.dist.adj.cub.mmean.m <- emmeans(min.dist.adj.cub, 
                                            'toxo.status')
      summary(min.dist.adj.cub.mmean.m)
   
    
    
    
###############################################################################
##############   4. Sub. and Adult distance from lions models    ##############
###############################################################################  

  ### 4.1 Boldness towards lions: SUB and ADULT 
    
    #** NOTE: Data for a sensitivity analysis were restricted such that   
    # average approach distancesare based on approaches after diagnosis for  
    # seropositive and approaches before diagnosis for seronegative animals
    
    ## a) Unadjusted sub / adult model data: min dist lions
      # by toxo status
      lion.dist.unadj.sub.adult <- lmer(sqrt.dist ~ toxo.status +
                                          (1|hy.id),
                                        data = lion_hy_dist_toxo_sub_adult)
      
    ## b) Parameter estimates
      summary(lion.dist.unadj.sub.adult)  # model parameter estimates
      confint(lion.dist.unadj.sub.adult)  # 95% CIs
      plot(lion.dist.unadj.sub.adult) # view fitted vs residuals
    
   
    
    
    # ## c) Bootstrap parameter estimates   
    #   # bootstrapping number of resampling simulations
    #   boot.lion.dist.unadj.sub.adult <- 
    #     bootMer(x = lion.dist.unadj.sub.adult,
    #             FUN = fixef, nsim = 1000,
    #             use.u = F, type = 'parametric')
    #   
    #   tidy(boot.lion.dist.unadj.sub.adult) # beta estimates and SE
    #   
    #   # use 'boot' package to generate 95% CI  
    #   bt.ci.lion.dist.unadj.sub.adult <- 
    #     boot.ci(boot.lion.dist.unadj.sub.adult, 
    #                                       type = c('perc', 'norm', 'basic'),
    #                                       index = 2) # CI for 1st betas
    #   
    #   print(bt.ci.lion.dist.unadj.sub.adult)
    
    
    ## d) Adjusted model sub and adult data: lifestage
    # min. approach dist. from lions by toxo status
    lion.dist.adj.sub.adult <- lmer(sqrt.dist ~ toxo.status 
      # dist.age.stndrzd  ~ toxo.status +
      + sex  
      #+ age.mon.lion # many NA
      + age.cat.dart
      + age.cat.lion
      #+ hum.pop.den      # sensitivity
      #+ food.present     # sensitivity
      #+ stan.rank.lion   # sensitivity, since only 
                          # female ranks, can't control for sex
      + (1|hy.id),
      data = lion_hy_dist_toxo_sub_adult)
     #data = lion_hy_dist_toxo_restrict_sub_adult)    # sensitivity
     #data = subset(lion_hy_dist_toxo_sub_adult,    
     #               diagnosis != 'doubtful'))       # sensitivity
    
    #** NOTE there is a singular fit when including other variables, 
    # results still null, so we report only adjustment for sex, age.cat.dart,
    # and age.cat.lion

    
    ## e) Parameter estimates
    summary(lion.dist.adj.sub.adult)  # model parameter estimates
    confint(lion.dist.adj.sub.adult)  # 95% CIs
    plot(lion.dist.adj.sub.adult) # view fitted vs residuals
    vif(lion.dist.adj.sub.adult)

    # Use emmeans to estimate marginal means
    lion.dist.adj.sub.adult.mmean.m <- emmeans(lion.dist.adj.sub.adult, 
                                        'toxo.status')
    summary(lion.dist.adj.sub.adult.mmean.m)
    

      

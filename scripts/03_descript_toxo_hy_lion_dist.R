################################################################################
#############   Toxoplasma gondii infections are associated with   #############
#############     costly boldness toward felids in a wild host     #############
#############                                                      #############
#############   3. Descriptive statistics: Distance from lions     #############
#############                                                      #############
#############           By: Zach Laubach and Eben Gerring          #############
#############                 created: 12 July 2018                #############
#############              last modified: 23 Feb. 2021             #############
################################################################################

### PURPOSE: Univariate and bivariate statistics for models in which 
           # hyena approach distance from lions is the outcome


  # Code Blocks
    # 1: Configure workspace
    # 2: Import data
    # 3: Univariate data exploration  
    # 4: Bivariate data exploration
    # 5: Data transformations 
    # 6: Identify confounding and precision variables
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
      load(here('data/03_toxo_hy_lion_dist_data.RData'))
    
    
    
###############################################################################
##############           3. Univariate data exploration          ##############
###############################################################################

#**************************  Toxo Lion Hyena Dist.  ****************************
    
  ### 3.1 Univariate stats avg minimum approach towards lions 
    ## a) Descriptive stats hyena min approach distances towards lions
      # repeat data set
      univar_min_app_dist <- lion_hy_dist_toxo %>%
        summarize(n.app.dist = sum(!is.na(distance)),
                  avg.app.dist = round(mean
                                       (distance,
                                         na.rm = T), 2),
                  median.app.dist =  round(quantile(distance,
                                                    c(.5), na.rm = T), 2),
                  sd.app.dist = round(sd(distance,
                                         na.rm = T), 2))
    
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/univar_min_app_dist.pdf'), height = 3, width = 8)
      grid.table(univar_min_app_dist)
      dev.off()
      
    ## c) Descriptive stats hyena min approach distances towards lions
      # repeat data set
      univar_min_app_dist_cub <- lion_hy_dist_toxo_cub %>%
        summarize(n.app.dist = sum(!is.na(avg.min.dist.lion)),
                  avg.app.dist = round(mean
                                       (avg.min.dist.lion,
                                         na.rm = T), 2),
                  median.app.dist =  round(quantile(avg.min.dist.lion,
                                                    c(.5), na.rm = T), 2),
                  sd.app.dist = round(sd(avg.min.dist.lion,
                                         na.rm = T), 2))
      
    ## d) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/univar_min_app_dist.pdf'), height = 3, width = 8)
      grid.table(univar_min_app_dist)
      dev.off()
      
      
  ### 3.2 Histograms of hyena approach distance from lions       
    ## a) Histogram Outcome (minimum approach distance, includes repeated
      # measures)
      ggplot(data=lion_hy_dist_toxo, aes(x=distance)) + 
        geom_histogram(aes(y = ..count..),
                       breaks=seq(0, 100, by = 1), 
                       col='black',
                       fill = 'dark grey') +
        xlim(c(0,100)) +
        labs(title = 'Histogram of hyena minimum approach distance 
             towards lions (includes repeated measures)') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        labs(x='Avg. Min. Approach Dist. (m)', y='Frequency') 
      
    ## b) Save Plot
      # use ggsave to save the plot
      ggsave('life_min_app_dist_histogram.pdf', plot = last_plot(), 
             device = NULL, 
             path = here('output/'), scale = 1, width = 5, 
             height = 3, 
             units = c('in'), dpi = 300, limitsize = TRUE)   
    
    ## c) Histogram of sqrt transformed distances
      ggplot(data=lion_hy_dist_toxo, aes(x=sqrt(distance))) + # sqrt transform
        geom_histogram(aes(y = ..count..),
                       breaks=seq(0, 10, by = 0.1), 
                       col='black',
                       fill = 'dark grey') +
        xlim(c(0,10)) +
        labs(title = 'Histogram of hyena minimum approach distance 
             towards lions (includes repeated measures)') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        labs(x='Avg. Min. Approach Dist. (m)', y='Frequency') 
      
    ## d) Histogram Outcome (minimum approach distance cubs only)
      ggplot(data=lion_hy_dist_toxo_cub, aes(x=avg.min.dist.lion)) + 
        geom_histogram(aes(y = ..count..),
                       breaks=seq(0, 100, by = 1), 
                       col='black',
                       fill = 'dark grey') +
        xlim(c(0,100)) +
        labs(title = 'Histogram of hyena cub minimum approach distance 
             towards lions (one repeated measure averaged)') +
        theme(plot.title = element_text(hjust = 0.5)) + # center title
        labs(x='Avg. Min. Approach Dist. (m)', y='Frequency') 
      
    ## e) Save Plot
      # use ggsave to save the plot
      ggsave('cub_min_app_dist_histogram.pdf', plot = last_plot(), 
             device = NULL, 
             path = here('output/'), scale = 1, width = 5, 
             height = 3, 
             units = c('in'), dpi = 300, limitsize = TRUE)   
      
      
       
###############################################################################
##############            4. Bivariate data exploration          ##############
###############################################################################      

##******************* Determinants hyena bevavior w/ lions *******************##  
  
  ### 4.1 Bivariate descriptive stats for approach distances
      
      # NOTE: The data lion_hy_dist_toxo_restrict_sub_adult, contains data that 
      # are restricted (before seronegative, after seropositive), the data
      # 'lion_hy_dist_toxo' is the full data set including any approach 
      # lion distance for diagnosed hyenas with repeated samples for the 
      # same inividuals
      
    ## a) Bivariate descriptive stats for approach distance by sex
      # Use filter version if you have missing data for grouping variable; 'sex'
      # calculate n, mean, median and standard deviation grouped by sex
      bivar_dist_by_sex_sum <- lion_hy_dist_toxo %>%
        #filter(sex == 'm' | sex == 'f') %>%
        group_by(sex) %>%
        summarize (n.app.dist = sum(!is.na(distance)),
                   avg.app.dist = round(mean(distance, na.rm = T), 2),
                   median.app.dist =  round(quantile(distance, c(.5),
                                                     na.rm = T), 2),
                   sd.app.dist = round(sd(distance, na.rm = T), 2))

    # ## b) save the data frame of summary stats out as a pdf into output file
    #   pdf(here('output/dist_by_sex_summary.pdf'),
    #       height = 3, width = 7)
    #   grid.table(bivar_dist_by_sex_sum)
    #   dev.off()

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
      # ggsave('app_dist_by_sex_plot.pdf', plot = last_plot(), device = NULL,
      #        path = paste0(here(),'/output'),
      #        scale = 1, width = 11, height = 7,
      #        units = c('in'), dpi = 300, limitsize = TRUE)

    ## e) Bivariate descriptive stats for approach distance by age
      # calculate n, mean, median and standard deviation grouped by age
      bivar_dist_by_age_cat <- lion_hy_dist_toxo %>%
        group_by(age.cat.lion) %>%
        summarize (n.app.dist = sum(!is.na(distance)),
                   avg.app.dist = round(mean(distance, na.rm = T), 2),
                   median.app.dist =  round(quantile(distance, c(.5), 
                                                     na.rm = T), 2),
                   sd.app.dist = round(sd(distance, na.rm = T), 2))
      
    ## f) Plot approach distance by age (categorical)
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
      
    # ## g) Save Plot
    #   # use ggsave to save the plot
    #   ggsave('app_dist_by_age_cat_plot.pdf', plot = last_plot(), device = NULL, 
    #          path = paste0(here(),'/output'), 
    #          scale = 1, width = 11, height = 7,
    #          units = c('in'), dpi = 300, limitsize = TRUE)     
      
      
    ## h) Plot approach distance by age.mon.lion
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
      
    ## i) Save Plot
      # use ggsave to save the plot
      # ggsave('app_dist_by_age_mon_plot.pdf', plot = last_plot(), device = NULL, 
      #        path = paste0(here(),'/output'), 
      #        scale = 1, width = 11, height = 7,
      #        units = c('in'), dpi = 300, limitsize = TRUE) 
      
      
    ## j) Plot approach distance by social rank
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
      
    ## k) Save Plot
      # use ggsave to save the plot
      # ggsave('app_dist_by_stnd_rank_plot.pdf', plot = last_plot(), device = NULL, 
      #        path = paste0(here(),'/output'), 
      #        scale = 1, width = 11, height = 7,
      #        units = c('in'), dpi = 300, limitsize = TRUE) 
      
    ## l) Bivariate descriptive stats for approach distance by food present
      # calculate n, mean, median and standard deviation grouped by food pres.
      bivar_dist_by_mode_fd_pres <- lion_hy_dist_toxo %>%
        group_by(food.present) %>%
        summarize (n.app.dist = sum(!is.na(distance)),
                   avg.app.dist = round(mean(distance, na.rm = T), 2),
                   median.app.dist =  round(quantile(distance, c(.5),
                                                     na.rm = T), 2),
                   sd.app.dist = round(sd(distance, na.rm = T), 2))

      
    ## m) Plot approach distance food present
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
      
    ## n) Save Plot
      # use ggsave to save the plot
      # ggsave('app_dist_by_fd_pres_plot.pdf', plot = last_plot(), device = NULL, 
      #        path = paste0(here(),'/output'), 
      #        scale = 1, width = 11, height = 7,
      #        units = c('in'), dpi = 300, limitsize = TRUE)  

    ## o) Bivariate descriptive stats for approach distance by human disturbance
      # calculate n, mean, median and standard deviation (darting yr)
      # bivar_dist_by_hum_dist_sum <- lion_hy_dist_toxo %>%
      #   group_by(hum.pop.den) %>%
      #   summarize (n.app.dist = sum(!is.na(distance)),
      #              avg.app.dist = round(mean(distance, na.rm = T), 2),
      #              median.app.dist =  round(quantile(distance, c(.5),
      #                                                na.rm = T), 2),
      #              sd.app.dist = round(sd(distance, na.rm = T), 2))

    ## p) save the data frame of summary stats out as a pdf into output file
      # pdf(here('output/dist_by_hum_dist_summary.pdf'),
      #     height = 3, width = 7)
      # grid.table(bivar_dist_by_hum_dist_sum)
      # dev.off()
    
    ## q) Bivariate descriptive stats for approach distance by human disturb.
      # calculate n, mean, median and standard deviation grouped by 
      # hum.dist.lion (the year of intx)
      bivar_dist_by_hum_dist <- lion_hy_dist_toxo %>%
        group_by(hum.dist.lion) %>%
        summarize (n.app.dist = sum(!is.na(distance)),
                   avg.app.dist = round(mean(distance, na.rm = T), 2),
                   median.app.dist =  round(quantile(distance, c(.5), 
                                                     na.rm = T), 2),
                   sd.app.dist = round(sd(distance, na.rm = T), 2))
      
    ## r) save the data frame of summary stats out as a pdf into output file
      # pdf(here('output/dist_by_hum_dist_summary.pdf'), 
      #     height = 3, width = 7)
      # grid.table(bivar_dist_by_hum_dist)
      # dev.off()
      
    ## s) Plot approach distance by hum disturbance
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
             Human disturbance assessed the year lion hyena intx.') +
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
      
    ## t) Save Plot
      # use ggsave to save the plot
      # ggsave('app_dist_by_hum_dist_plot.pdf', plot = last_plot(), device = NULL, 
      #        path = paste0(here(),'/output'), 
      #        scale = 1, width = 11, height = 7,
      #        units = c('in'), dpi = 300, limitsize = TRUE)     
    
      
      
  ### 4.2 Descriptive stats min approach distance towards lions
    ## a) min approach distance towards lions summary 
      # when data set is full
      app_dist_by_toxo_full_summary <- lion_hy_dist_toxo %>%
        group_by(toxo.status) %>%
        summarise (n.avg.min.dist =  sum(!is.na
                                         (distance)),
                   #n_rank = n(), # n including na
                   avg_life_avg_min_dist = round(mean
                                                 (distance,
                                                   na.rm = T), 2),
                   stdev_life_avg_min_dist = round(sd
                                                   (distance, 
                                                     na.rm = T), 2)) 
      
    ## b) save the data frame of summary stats out as a pdf into output file
      # pdf(here('output/app_dist_by_toxo_full_summary.pdf'), 
      #     height = 3, width = 10)
      # grid.table(app_dist_by_toxo_full_summary)
      # dev.off()
      
      
  ### 4.3 Bivariate descriptive stats for AGE STRATIFIED approach distances
      
    # NOTE: The data 'lion_hy_dist_toxo_cub' is the full data set  
    # including any hyena cub approach lion distance for diagnosed hyenas,
    # while 'lion_hy_dist_toxo_sub_adult_sum' includes distances when hyeans,
    # were subadults or adults. Repeated samples within age strata are 
    # averaged to summarize into a single value.
      
    ## a) Bivariate descriptive stats for cub age category summarized 
      # approach distance by toxo status
      bivar_cub_dist_by_toxo_sum <- lion_hy_dist_toxo_cub %>%
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
      
      
    ## c) Calculate sample sizes (number of hyenas)
      bivar_sub_adult_dist_by_toxo_sum <- lion_hy_dist_toxo_sub_adult %>%
        group_by(hy.id) %>%
        summarize (toxo.status = first(toxo.status),
                   distance = round(mean(distance, na.rm = T), 2))
      
      bivar_sub_adult_dist_by_toxo_sum <- bivar_sub_adult_dist_by_toxo_sum %>%
        group_by(toxo.status) %>%
        summarize (n.app.dist = sum(!is.na(distance)),
                   avg.app.dist = round(mean(distance, na.rm = T), 2),
                   median.app.dist =  round(quantile(distance, c(.5), 
                                                     na.rm = T), 2),
                   sd.app.dist = round(sd(distance, na.rm = T), 2))  
      
    ## d) Unadjusted model cub data: cub approach dist. from lions 
      # by toxo status
      min.dist.unadj.cub <- glm(avg.min.dist.lion ~ toxo.status,
                                data = lion_hy_dist_toxo_cub,
                                #data = subset(lion_hy_dist_toxo_cub_sum, age.cat.dart =='cub'),
                                family = gaussian)
      
    ## d) Parameter estimates
      summary(min.dist.unadj.cub)  # model parameter estimates
      confint(min.dist.unadj.cub)  # 95% CIs
      plot(min.dist.unadj.cub) # view fitted vs residuals
      
    # Use emmeans to estimate marginal means
      min.dist.unadj.cub.desc.mmean <- emmeans(min.dist.unadj.cub, 
                                               'toxo.status')
      summary(min.dist.unadj.cub.desc.mmean)
      
    ## e) Sub adult and adult repeated measures minimum approach 
      # (min dist lions data by toxo.status)
      min.dist.by.toxo.desc <- lmer(distance ~ toxo.status + 
                                     (1|hy.id),
                                   data = lion_hy_dist_toxo)
      
      summary(min.dist.by.toxo.desc) # model parameter estimates
      confint(min.dist.by.toxo.desc) # 95% CIs
      plot(min.dist.by.toxo.desc) 
      
      # check qq-plot
      qqnorm(resid(min.dist.by.toxo.desc))
      qqline(resid(min.dist.by.toxo.desc))
      
      # Use emmeans to estimate marginal means
      min.dist.by.toxo.desc.mmean <- emmeans(min.dist.by.toxo.desc, 
                                                 'toxo.status')
      summary(min.dist.by.toxo.desc.mmean)
      
      
  ### 4.4 Graphs of cub average approach distance from lions by toxo status    
    ## a) create a new dataframe of means and SD for graphing
      cub.lion.dist.mean.sd <- data.frame(term = c('Seronegative',
                                                    'Seropositive'),
                                           estimate = c(-91.43, -43.50),
                 # Note forcing dwplot to graph SD by specificy the upper and
                 # lower bounds instead of automatically calculating actual
                 # 95% CI from SE
                                           conf.low = c((-91.43 + 0), 
                                                        (-43.50 + 0)),
                                           conf.high = c((-91.43 + 0), 
                                                         (-43.50 + 0)))
      
    ## b) Graph results using dotwhisker, broom, dplyr, and ggplot2 packages
      dwplot(cub.lion.dist.mean.sd, 
             vline = geom_vline(xintercept = 0, colour = 'gray20', 
                                linetype = 2), # line at zero behind coefs
             dot_args = list(size = 3),
             whisker_args = list(size = 1),
             dodge_size = 1) + 
        #coord_flip() + # flip x and y axes
        xlim (-100,0) +
        labs(title = 'Comparison of hyena cub approach 
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
        xlab(expression(atop(bold('Mean ± SD'), 
                             paste(italic('Minimum approach distance from lions (m)'))))) +
        #scale_y_discrete(labels = c('Seropostive hyenas')) +
        ylab('')
      
   ## c) Save Plot
      # use ggsave to save the linearization plot
      ggsave('cub_app_lion_dist_plot.pdf', plot = last_plot(), 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 11,
             height = 6,
             units = c('in'), dpi = 300, limitsize = TRUE)

      
  ### 4.5 Graphs of sub. and adult approach distance from lions by toxo status       
    ## a) create a new dataframe of LS means for graphing
      sub.adult.lion.dist.ls.means <- data.frame(term = c('Seronegative',
                                                     'Seropositive'),
                                      estimate = c(-27.0, -24.8),
                # Note forcing dwplot to graph SE by specificy the upper and
                # lower bounds instead of automatically calculating actual
                # 95% CI from SE
                                    conf.low = c((-27.0 + 0), 
                                                (-24.8 + 0)),
                                    conf.high = c((-27.0 + 0), 
                                                 (-24.8 + 0)))
      
    ## b) Graph results using dotwhisker, broom, dplyr, and ggplot2 packages
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
      
    ## c) Save Plot
      # use ggsave to save the linearization plot
      ggsave('sub_adult_app_lion_dist_plot.pdf', plot = last_plot(), 
             device = NULL,
             path = paste0(here(),'/output'), 
             scale = 1, width = 11,
             height = 6,
             units = c('in'), dpi = 300, limitsize = TRUE)
      


###############################################################################
##############              5. Data transformations              ##############
###############################################################################
  
  ### 5.1 Square root transformation of hyena approach lion distances to
      # improve assumptions of normalilty based on residual and qq plots
    ## a) SQRT transform cub distances  
      lion_hy_dist_toxo_cub <- lion_hy_dist_toxo_cub %>%
        mutate(sqrt.dist = sqrt(avg.min.dist.lion))
      
    ## b) SQRT transform full data distances    
      lion_hy_dist_toxo <- lion_hy_dist_toxo %>%
        mutate(sqrt.dist = sqrt(distance))
      
    ## c) SQRT transform sub adult data distances    
      lion_hy_dist_toxo_sub_adult <- lion_hy_dist_toxo_sub_adult %>%
        mutate(sqrt.dist = sqrt(distance))
      
    ## d) SQRT transform restricted sub adult data distances    
      lion_hy_dist_toxo_restrict_sub_adult <- 
        lion_hy_dist_toxo_restrict_sub_adult %>%
        mutate(sqrt.dist = sqrt(distance))
      
      
  ### 5.2 Bin hyena approach lion distances into a binary variable
      # Based on freq distribution of distances from lion_hy_dist_toxo
      # data set; should help deal with inaccuracy in distance estiimates
      
    ## a) Cub data set 
      median(lion_hy_dist_toxo_cub$avg.min.dist.lion)
    
      lion_hy_dist_toxo_cub <- lion_hy_dist_toxo_cub %>%
        mutate(distance.binary = as.factor(ifelse(avg.min.dist.lion >= 100, 
                                                  'far',
                                                  'close')))
      
    ## b) Full data set (with pseudoreplication)
      median(lion_hy_dist_toxo$distance)
      
      lion_hy_dist_toxo <- lion_hy_dist_toxo %>%
        mutate(distance.binary = as.factor(ifelse(distance >= 15, 'far',
                                                 'close')))
      
    ## c) Subadult and adult data set (with pseudoreplication)
      lion_hy_dist_toxo_sub_adult <- lion_hy_dist_toxo_sub_adult %>%
        mutate(distance.binary = as.factor(ifelse(distance >= 15, 'far',
                                                  'close')))

    ## d) Restricted sub and adult data set (with pseudoreplication)
      lion_hy_dist_toxo_restrict_sub_adult <- 
        lion_hy_dist_toxo_restrict_sub_adult %>%
        mutate(distance.binary = as.factor(ifelse(distance < 15, 'close',
                                                  'far')))

# ##****** NOTE: After identifiying age structure in hyena approach distance
#       # we considered age standardized distances
#     
#   ### 5.x Age (at hyena lion interaction) z-score standardization of
#       # approach distances
# 
#     ## a) Age standardized distance for lion_hy_dist
#       lion_hy_dist <- lion_hy_dist  %>%
#         group_by(age.cat.lion) %>%
#         mutate(dist.age.stndrzd = scale(distance))%>%
#         ungroup() # ungroup the data frame.
# 
#   #     #NOTE: check that scaling occurs within category of lion age  
#       lion_hy_dist_cub <- filter(lion_hy_dist, age.cat.lion == 'cub')
#       lion_hy_dist_cub <- lion_hy_dist_cub  %>%
#         mutate(age.stndrdz.lion = scale(lion_hy_dist_cub[ ,'distance']))
# 
#   #   ## b) Age standardized distance for lion_hy_dist_toxo
#       lion_hy_dist_toxo <- lion_hy_dist_toxo  %>%
#         group_by(age.cat.lion) %>%
#         mutate(dist.age.stndrzd = scale(distance))%>%
#         ungroup() # ungroup the data frame.
# 
#   #   ## c) Age standardized distance for lion_hy_dist_toxo_restrict
#       lion_hy_dist_toxo_restrict <- lion_hy_dist_toxo_restrict  %>%
#         group_by(age.cat.lion) %>%
#         mutate(dist.age.stndrzd = scale(distance))%>%
#         ungroup() # ungroup the data frame.
#    
#           
#   ### 5.x Center and Transform Predictive Variables
#     ## a) center function based on column means    
#      center_fxn <- function(x) {
#        xcenter = colMeans(x, na.rm = T)
#        x - rep(xcenter, rep.int(nrow(x), ncol(x)))
#      }
#       
#     ## b) make a list of variable names to center, value = T is necessary
#       # or column positions will be returned
#      vars_to_center <- c('age_mon','mass')
#    
#     ## c) Center continous predictors  
#      toxo_data[ ,c(vars_to_center)] <-
#        center_fxn(toxo_data[ ,c(vars_to_center)])
#       
#     ## d) Z-score standardize
#       # Use 'scale' function to z-score standardization function based on
#       # subtracting mean from each x and dividing
#       # by 1 sd in the select columns; raw data are replaced with z-scores
#       # Here Prey densities have been z-score transformed
#       # Z-score standardizetoxo_data
#       standardizetoxo_data
#        toxo_data[ ,c(vars_to_center)] <-
#          scale(toxo_data[ ,c(vars_to_center)])

       
      
###############################################################################
##############  6. Identify confounding and precision variables  ##############
###############################################################################     
    
  ### 6.1 Boldness towards lions: Approach lion distance by sex
    ## a) Repeated measures minimum approach distance FULL data
      # min dist lions data by sex
      min.dist.by.sex.full <- lmer(
                                    sqrt.dist ~ 
                                    #distance ~ # check resid and qq plots
                                    sex + 
                                      (1|hy.id),
                                    data = lion_hy_dist_toxo)
      
      summary(min.dist.by.sex.full) # model parameter estimates
      confint(min.dist.by.sex.full) # 95% CIs
      plot(min.dist.by.sex.full) 
      
      qqnorm(resid(min.dist.by.sex.full))
      qqline(resid(min.dist.by.sex.full))
      
      
  ### 6.2 Boldness towards lions: Approach lion distance by age
    ## a) Repeated measures minimum approach distance FULL data
      # min dist lions data by age
      min.dist.by.age.full <- lmer(sqrt.dist ~ age.cat.lion + 
                                      (1|hy.id),
                                    data = lion_hy_dist_toxo)
      
      summary(min.dist.by.age.full) # model parameter estimates
      confint(min.dist.by.age.full) # 95% CIs
      plot(min.dist.by.age.full)


  ### 6.3 Boldness towards lions: Approach lion distance by stan.rank
    ## a) Repeated measures minimum approach distance FULL data
      # min dist lions data by standardized rank (rank is for year of intx)
      life.dist.by.rank <- lmer(sqrt.dist ~
                                  stan.rank.lion
                                + (1|hy.id),
                                data = lion_hy_dist_toxo)

      summary(life.dist.by.rank) # model parameter estimates
      confint(life.dist.by.rank) # 95% CIs
      plot(life.dist.by.rank)
      
    ## c) Subadult, adult Repeated measures minimum approach distance FULL data
      # min dist lions data by standardized rank (rank is for year of intx)
      sub.adult.dist.by.rank <- lmer(sqrt.dist ~
                                 stan.rank.lion
                                + (1|hy.id),
                                data = lion_hy_dist_toxo_sub_adult)
      
      summary(sub.adult.dist.by.rank) # model parameter estimates
      confint(sub.adult.dist.by.rank) # 95% CIs
      plot(sub.adult.dist.by.rank)
      
      qqnorm(resid(sub.adult.dist.by.rank))
      qqline(resid(sub.adult.dist.by.rank))
      
      # Cub minimum approach distance FULL data
      # min dist lions data by standardized rank (rank is for year of intx)
      cub.dist.by.rank <- glm(sqrt.dist ~
                                avg.stan.rank,
                                     data = lion_hy_dist_toxo_cub)
      
      summary(cub.dist.by.rank) # model parameter estimates
      confint(cub.dist.by.rank) # 95% CIs
      #plot(cub.dist.by.rank)

      
  ### 6.4 Boldness towards lions: Approach lion distance by food present
    ## a) Repeated measures minimum approach distance FULL data
      # min dist lions data by whether or not food was present during 
      #those interactions
      min.dist.by.fd.pres <- lmer(sqrt.dist ~ food.present
                                  + (1|hy.id),
                                  data = lion_hy_dist_toxo) 
      
      summary(min.dist.by.fd.pres) # model parameter estimates
      confint(min.dist.by.fd.pres) # 95% CIs
      #plot(min.dist.by.mode.fd.pres.restrict)
      

  ### 6.5 Boldness towards lions: Approach lion distance by hum.dist.lion
    ## a) # Repeated measures minimum approach distance FULL data
      # min dist lions data by human disturbance (the year each hy-lion intx) 
      min.dist.by.hum.pop.den <- lmer(sqrt.dist ~ 
                                        hum.dist.lion
                                      + (1|hy.id),
                                      data = lion_hy_dist_toxo) 
      
      summary(min.dist.by.hum.pop.den) # model parameter estimates
      confint(min.dist.by.hum.pop.den) # 95% CIs
      plot(min.dist.by.hum.pop.den) 
    
        
      
###############################################################################
##############               7. Export data files                ##############
###############################################################################
      
  ### 7.1 Export data to an RData file
    # Files are saved in the 'data' folder in the working directory as an
    # RData file.
      
    ## a) Save and export tidy data tables for hyena toxo. and approach 
      # distance from lions analyses 
      save(file = here('data/05_toxo_hy_lion_dist_data.RData'),
           list = c('lion_hy_dist_toxo',
                    'lion_hy_dist_toxo_cub',
                    #'lion_hy_dist_toxo_cub_rpt',
                    'lion_hy_dist_toxo_sub_adult',
                    'lion_hy_dist_toxo_restrict_sub_adult'))
      
   

      

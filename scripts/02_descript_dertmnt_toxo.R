################################################################################
#############   Toxoplasma gondii infections are associated with   #############
#############     costly boldness toward felids in a wild host     #############
#############                                                      #############
#############  2. Descriptive statistics: Determinants of Toxo.    #############
#############                                                      #############
#############           By: Zach Laubach and Eben Gerring          #############
#############                 created: 12 July 2018                #############
#############              last modified: 9 Feb. 2021              #############
################################################################################

### PURPOSE: Univariate and bivariate statistics for models in which 
           # T. gondii infection in hyenas is outcome


  # Code Blocks
    # 1: Configure workspace
    # 2: Import data
    # 3: Univariate data exploration  
    # 4: Bivariate data exploration

  

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
##############           3. Univariate data exploration          ##############
###############################################################################

#***************************  Determinants of Toxo  ****************************
  ### 3.1 Univariate stats toxo 
    
    ## a) Descriptive stats toxo status
      univar_toxo_stat <- toxo_data %>%
        group_by(toxo.status) %>%
        summarise(n.status = sum(!is.na(toxo.status))) %>%
        mutate(freq = n.status / sum(n.status, na.rm = T))
        
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/univar_toxo_status.pdf'), height = 4, width = 8)
      grid.table(univar_toxo_stat)
      dev.off()
      
    ## b) Histogram Outcome (SP ratio)
        ggplot(data=toxo_data, aes(x=spratio)) +
          geom_histogram(aes(y = ..count..),
                         breaks=seq(0, 1, by = 0.05),
                         col='black',
                         fill = 'dark grey') +
          xlim(c(0,1)) +
          labs(title= 'Histogram for SP ratio') +
          theme(plot.title = element_text(hjust = 0.5)) + # center title
          labs(x='SP ratio', y='Frequency') +
          geom_vline(aes(xintercept = 0.4),
                     color='red', linetype='dashed', size=1) +
          geom_vline(aes(xintercept = 0.5),
                     color='red', linetype='solid', size=1)

      ## c) Save Plot
        # use ggsave to save the plot
        ggsave('spratio_histogram.pdf', plot = last_plot(), device = NULL,
               path = here('output/'), scale = 1, width = 5,
               height = 3,
               units = c('in'), dpi = 300, limitsize = TRUE)

    
       
###############################################################################
##############            4. Bivariate data exploration          ##############
###############################################################################      

#***************************  Determinants of Toxo  ****************************  
  
  ### 4.1 Descriptive bivariate stats toxo status by sex
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
      
      
  ### 4.2 Descriptive bivariate stats toxo by age (at time of diagnosis)  
    ## a) Age summary  
      age_var_summary <- toxo_data %>%
        group_by (age.cat.dart) %>%
        summarise (n.age.dart = sum(!is.na(age.mon.dart)),
                   avg.age.dart = round (mean(age.mon.dart, na.rm = T),2),
                   stdev.age.dart = round (sd(age.mon.dart, na.rm = T), 2)) %>%
        mutate (freq_age =  (n.age.dart / sum(n.age.dart)))
      
    ## b) save the data frame of summary stats out as a pdf into output file
      pdf(here('output/age_var_summary.pdf'), 
          height = 4, width = 7)
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
          height = 4, width = 7)
      grid.table(age_cat_summary)
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
      
      
    ## g) Plot spratio by categorical age at darting 
      # graph of raw data for spratio by age
      ggplot(data = toxo_data,
             aes (x = age.cat.dart, y = spratio,
                  color = age.cat.dart)) +
        geom_boxplot() +
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs (title = 'SP ratio by age at diagnosis') +
        ylab ('SP ratio') +
        xlab ('Age at diagnosis')

    ## i) Save Plot
      # use ggsave to save the boxplot
      ggsave('spratio_by_age_plot.pdf', plot = last_plot(), device = NULL,
             path = here('output/'), scale = 1, width = 7,
             height = 5,
             units = c('in'), dpi = 300, limitsize = TRUE)

    ## j) Plot spratio by age at darting (months)
      ggplot(data = toxo_data,
             aes(x = age.mon.dart, y = spratio, color = toxo.status)) +
        geom_point(shape = 1) +
        geom_smooth(method = loess, se = F) + # Add smooth curve best fit lines
        geom_hline(aes(yintercept = 0.5), colour = 'red', linetype = 'dashed') +
        theme(text = element_text(size=20))+
        scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        labs(title = 'SP ratio by age at diagnosis',
             subtitle = '(Age in months)') +
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
        ylab('SPratio') +
        xlab('Age at diagnosis (mon)')

    ## k) Save Plot
      # use ggsave to save the plot
      ggsave('sp_by_age_mon_plot.pdf', plot = last_plot(), device = NULL,
             path = paste0(here(),'/output'),
             scale = 1, width = 11, height = 7,
             units = c('in'), dpi = 300, limitsize = TRUE)
      
      
 ### 4.3 Descriptive bivariate stats toxo by rank
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
    
    
  ### 4.4 Descriptive bivariate stats toxo by livestock density
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

      

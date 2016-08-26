#-----------------------------------------------------------------------------------*
# ---- SET-UP ----
#===================================================================================*

library(dplyr) ; library(tidyr) ; library(ggplot2) 
library(grid) ; library(gridExtra) ; library(stringr)

# STACKED BAR:

# Theme:

plot_theme <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 7),
          axis.title.x = element_text(size = 12, vjust = -0.4),
          axis.title.y = element_text(size = 12, vjust = 1.1),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 7),
          panel.border = element_blank())
}

# Colors for bars:

colorBlindPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                       "#0072B2", "#D55E00", "gray60")

#-----------------------------------------------------------------------------------*
# ---- DATA ----
#===================================================================================*
# Data are derived in script: rubl_12_15.R
# My working directory: C:/Users/Brian/Desktop/gits/RUBL

dataStackedBar <- bind_rows(
  #---------------------------------------------------------------------------------*
  # Bind the three contribution frames for each flock size class:
  
  lambdaContributionFrame_allIndOut %>%
    mutate(flockClass = 'ind'),
  lambdaContributionFrame_allSfOut %>%
    mutate(flockClass = 'sf'), 
  lambdaContributionFrame_allLfOut %>%
    mutate(flockClass = 'lf')
  ) %>%
  #---------------------------------------------------------------------------------*
  # Set some variables to "other":
  
  mutate(variable = as.character(variable),
         variable = ifelse(
    !(variable %in% c('tmin', 'flood', 'rowcrop', 'ppt', 
                      'flood', 'pasture', 'dev_hi')),
    'other', variable)
  ) %>%
  #---------------------------------------------------------------------------------*
  # Sum the other percentages for each flock size class:
  
  group_by(flockClass, variable) %>%
  summarize(varContribution = sum(meanContribution)) %>%
  ungroup %>%
  #---------------------------------------------------------------------------------*
  # Reset the factor levels and labels for plotting:
  
  mutate(variable = variable %>%
           factor(levels = c('other', 'dev_hi', 'pasture', 'ppt', 
                             'rowcrop', 'flood', 'tmin'),
                  labels = c('Other   ', 'Highly developed   ', 'Pasture    ', 
                             'Precipitation   ', 
                             'Row crop   ', 'Floodplain   ', 'Minimum temperature    '),
                  ordered = TRUE),
         flockClass = flockClass %>%
           factor(levels = c('ind', 'sf', 'lf'),
                  labels = c('Individual', 'Small', 'Large'))
         )

#-----------------------------------------------------------------------------------*
# ---- PLOTTING ----
#===================================================================================*

plotWithTemp <- ggplot(dataStackedBar, 
       aes(x = flockClass, y  = varContribution, 
           fill = variable, order = -as.numeric(variable))) + 
  geom_bar(stat = 'identity') + 
  geom_bar(stat = 'identity',color = 'black', show_guide = FALSE, size = .6) +
  plot_theme() +
  labs(x = 'Flock size class', y = 'Variable contribution (%)') +
  scale_fill_manual(name = 'Variable', values = colorBlindPalette) +
  theme(legend.title = element_blank(),
        legend.key.height = unit(1,"line"),
        legend.key = element_rect(size=2, color = 'white'),
        # legend.margin = unit(0, 'line'),
        # legend.key.size = unit(2, 'lines'),
        legend.position = 'top')

plotNoTemp <- ggplot(dataStackedBar %>% 
         filter(!str_detect(variable, 'temperature')) %>%
         group_by(flockClass) %>% 
         mutate(TotalContribution = sum(varContribution)) %>% 
         ungroup %>% 
         mutate(relativeContribution = varContribution/TotalContribution), 
       aes(x = flockClass, y  = relativeContribution, 
           fill = variable, order = -as.numeric(variable))) + 
  geom_bar(stat = 'identity') + 
  geom_bar(stat = 'identity',color = 'black', show_guide = FALSE, size = .6) +
  plot_theme() +
  labs(x = 'Flock size class', y = 'Relative variable contribution') +
  scale_fill_manual(name = 'Variable', values = colorBlindPalette) +
  theme(legend.key.height = unit(2,"line"))

plots <- list(plotWithTemp, plotNoTemp)

g <- ggplotGrob(plots[[1]] + theme(legend.position="top"))$grobs

legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

# Plot output:

png(filename = "outPlots/VariableContributionMultiPlot.png", 
    width = 7.5, height = 4, units = 'in', res = 300)
grid.arrange(legend, arrangeGrob(plotWithTemp + theme(legend.position = 'none'),
                                 plotNoTemp + theme(legend.position = 'none'),
                                 nrow = 1),
             nrow = 2, heights = c(1, 6))
dev.off()



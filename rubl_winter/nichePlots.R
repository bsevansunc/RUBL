dat <-  data.frame(cond = factor(rep(c('A',"B", "C", "D"), each=200)), 
                   rating = c(rnorm(200),rnorm(200, mean=.8),
                              rnorm(200, mean = 1),
                              rnorm(200, mean = 1.5)))

ggplot(dat %>%
         filter(cond == 'A'),
       aes(x=rating, color = cond)) + 
  geom_density(alpha=.2, aes(fill = cond))+
  scale_fill_discrete(guide = FALSE)+
  xlab('Environmental variable') +
  ylab('Performance')+
  ylim(0, .5) +
  theme(legend.position = 'none',
        axis.title = element_text(size = rel(2)),
        axis.text.x = element_text(size = rel(2)),
        # legend.margin = unit(0, 'line'),
        # legend.key.size = unit(2, 'lines'),
        legend.position = 'top') + theme_bw()


ggsave(paste0('outPlots/niche', 1, '.png'),
       width = 6.5, height = 4.5, units = 'in')

ggplot(dat %>%
         filter(cond == c('A', 'B')),
       aes(x=rating, color = cond)) + 
  geom_density(alpha=.2, aes(fill = cond))+
  scale_fill_discrete(guide = FALSE)+
  xlab('Environmental variable') +
  ylab('Performance')+
  ylim(0, .5) +
  theme(legend.position = 'none',
        axis.title = element_text(size = rel(2)),
        axis.text.x = element_text(size = rel(2)),
        # legend.margin = unit(0, 'line'),
        # legend.key.size = unit(2, 'lines'),
        legend.position = 'top') + theme_bw()


ggsave(paste0('outPlots/niche', 2, '.png'),
       width = 6.5, height = 4.5, units = 'in')



ggplot(dat %>%
         filter(cond == c('A', 'B','C','D')),
                aes(x=rating, color = cond)) + 
         geom_density(alpha=.2, aes(fill = cond))+
         scale_fill_discrete(guide = FALSE)+
         xlab('Environmental variable') +
         ylab('Performance')+
         ylim(0, .5) +
         theme(legend.position = 'none',
               axis.title = element_text(size = rel(2)),
               axis.text.x = element_text(size = rel(2)),
               # legend.margin = unit(0, 'line'),
               # legend.key.size = unit(2, 'lines'),
               legend.position = 'top') + theme_bw()
       
       
       ggsave(paste0('outPlots/niche', 3, '.png'),
              width = 6.5, height = 4.5, units = 'in')
       
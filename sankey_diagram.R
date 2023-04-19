# demo code with toy data to generate sankey diagrams
rm(list=ls(all=T))
library(dplyr)
library(ggsankey)
library(ggplot2)
# generate toy data
set.seed(111)
t1 <- sample(x = c("Hosp A", "Hosp B", "Hosp C","Hosp D"), size = 100, replace = T)
t2 <- sample(x = c("Male", "Female"), size = 100, replace = T)
t3 <- sample(x = c("Survived", "Died"), size = 100, replace = T)
d <- data.frame(cbind(t1,t2,t3))
names(d) <- c('Hospital', 'Gender', 'Outcome')
# reshape toy data
df <- d %>% 
  make_long(Hospital, Gender, Outcome)
dagg <- df %>%
  dplyr::group_by(node) %>%
  tally() # count n per node
TotalCount = nrow(d)
dagg <- dagg %>%
  dplyr::group_by(node) %>%
  dplyr::mutate(pct = n/TotalCount) # calculate percent
df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = T)
# generate and display sankey diagram
pl <- ggplot(df2, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node),
                      label = paste0(node,", n = ", n, ', (',  round(pct* 100,1), '%)')))
pl <- pl + geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = T)
pl <- pl + geom_sankey_label(size = 3, color = "white", fill= "gray40", hjust = -0.2)
pl <- pl + theme_bw()
pl <- pl + theme(legend.position = "none")
pl <- pl + theme(axis.title = element_blank(), 
                  axis.text.y = element_blank(), 
                  axis.ticks = element_blank(), 
                  panel.grid = element_blank())
pl <- pl + scale_fill_viridis_d(option = "inferno")
pl <- pl + labs(fill = 'Nodes')
pl

# alluvial demo with Titanic toy data
library(ggalluvial)
titanic_wide <- data.frame(Titanic)
head(titanic_wide)
ggplot(data = titanic_wide, aes(axis1 = Class, axis2 = Sex, axis3 = Age, y = Freq)) +
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.2, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal()
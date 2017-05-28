# --------------------------- #
# Author: Odei Barredo Diaz   #
# Date:   28/05/2017          #
# --------------------------- #

#####  RIVER CONTINUUM CONCEPT  #####

# Library needed:
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(scales)
library(psych)
library(Deducer)

### A) IMPORT AND PREPARE DATA:

# Table indicating macroinvertebrates functional feeding groups: 
feeding_g <- read.csv("Macroinvertebrate_feeding_groups.csv", header = T, sep = ";")

# For the next data we also need to convert the variables "Section" and "Season"
# to factors. Also, we sort the Season months in the right order (since it 
# automatically orders them in alphabetical order).

# Macroinvertebrates abundance in each section, season and sample:
macro_abun <- read.csv("Macroinvertebrate_abundance.csv", header = T, sep = ";")
macro_abun$Section <- factor(macro_abun$Section)
macro_abun$Season <- factor(macro_abun$Season, levels= c("January", "April", "July", "October"))

# Macroinvertebrates biomass in each section, season and sample:
macro_biom <- read.csv("Macroinvertebrate_biomass.csv", header = T, sep = ";")
macro_biom$Section <- factor(macro_biom$Section)
macro_biom$Season <- factor(macro_biom$Season, levels= c("January", "April", "July", "October"))

# Organic matter resource in each section and season:
om <- read.csv("Organic_matter.csv", header = T, sep = ";")
om$Section <- factor(om$Section)
om$Season <- factor(om$Season, levels= c("January", "April", "July", "October"))


### B) VISUALIZATION OF DATA:

# b.1) Changes of organic matter resource type and amount through 
#      sections and seasons:

om_r <- melt(om) 

ggplot(om_r, aes(x=Season, y =value, fill=variable)) +
  geom_bar(stat="identity", show.legend=T, position = "dodge") + 
  ylim(0,300)  + 
  labs(title = "Organic matter resource type and amount changes through sections and seasons", 
       fill="Resource") + 
  scale_fill_manual(values = c("springgreen3", "orange", "purple", "turquoise3" ),
                    labels = c(expression(paste("CPOM (g AFDW/", m^2,")", sep="")), 
                               expression(paste("FPOM_benthos (g AFDW/", m^2,")", sep="")),
                               "FPOM_seston (mg AFDW/l)",
                               expression(paste("Epilithon (g AFDW/", m^2, ")", sep = "")))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, vjust = 5), 
        strip.text.x = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12), axis.title = element_blank(),
        legend.position = "bottom", 
        legend.text = element_text(size = 12, vjust = 5),
        legend.title = element_text(size = 12, face = "bold"),
        plot.margin = unit(c(1,2,1,1), "cm")) +
  facet_wrap(~ Section)


# b.2) Changes of macroinvertebrates abundance and biomass through sections 
#      and seasons:

# If we take a look at the data we see that for each case the sampling took
# 5 replica (A, B, C, D, E), so we have to estimate the means and standard errors
# to make a plot out of them.

# Sum the columns referring to macroinvertebrates abundance and biomass
# in one vector each
abun_sum <- rowSums(macro_abun[4:61])
biom_sum <- rowSums(macro_biom[4:61])

# and add it to the rest of the data
macro_full <- cbind(macro_abun[1:3], "s_abun"= abun_sum, "s_biom"= biom_sum)

# add two more columns containing the means and se by section and season 
# for both variables

macro_full <- macro_full %>% group_by(Section, Season) %>% 
  mutate(m_abun=mean(s_abun)) %>% 
  mutate(se_abun=sd(s_abun)/sqrt(length(s_abun))) %>%
  mutate(m_biom=mean(s_biom)) %>% 
  mutate(se_biom=sd(s_biom)/sqrt(length(s_biom)))

# get just the data needed for the plot
macro_full <- unique(macro_full[-c(2,4,5)]) 

abun_plot <- ggplot(data=macro_full, aes(x=Season, y=m_abun, fill=Section)) +
  ylim(0,30000) + labs(title= "Macroinvertebrates abundance", 
                       y = expression(paste("individuals/ ", m^2, sep = "")),
                       fill="Section") +
  geom_bar(stat="identity", show.legend=T, position = "dodge" ) +
  geom_errorbar(aes(ymin=m_abun-se_abun, ymax=m_abun+se_abun),
                width=0.3,
                position=position_dodge(0.9), size = 0.8) +
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        axis.text = element_text(size = 12), axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = c(0.1,0.9),
        legend.background = element_blank(),
        legend.text = element_text(size = 12, vjust = 5),
        legend.title = element_text(size = 12, face = "bold"))

biom_plot <- ggplot(data=macro_full, aes(x=Season, y=m_biom, fill=Section)) +
  ylim(0,2500) + labs(title= "Macroinvertebrates biomass",
                      y = expression(paste("mg/ ", m^2, sep = ""))) +
  geom_bar(stat="identity", show.legend=F, position = "dodge" ) +
  geom_errorbar(aes(ymin=m_biom-se_biom, ymax=m_biom+se_biom),
                width=0.3,
                position=position_dodge(0.9), size = 0.8) +
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        axis.text = element_text(size = 12), axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        plot.margin = unit(c(1,1,1,1), "cm"))

grid.arrange(abun_plot, biom_plot, ncol=2)


# b.3) Changes of functional feeding groups through sections and seasons:
# Add the corresponding FFG to each taxa
macro_abun_r <- melt(macro_abun)
f_group <- vector()

for(i in 1:nrow(macro_abun_r)){
  if(macro_abun_r$variable[i] %in% feeding_g$Taxa){
    f_group[i] <- as.character(feeding_g$FFG[match(macro_abun_r$variable[i], feeding_g$Taxa)])
  }
}

macro_abun_r$feeding_group <- f_group

# For the next plot (percent barplot) we wont use the se, just the means:

abun_ffg <- macro_abun_r %>% group_by(Section, Season, feeding_group) %>% 
  mutate(m_abun=mean(value))

abun_ffg_summ <- unique(abun_ffg[-c(2, 4,5)])

abun_ffg_plot <- ggplot(abun_ffg_summ, aes(x=Season, y = m_abun, fill=feeding_group)) +
  geom_bar(stat="identity", position = "fill", show.legend = F) +
  labs(title= "Macroinvertebrates feeding groups structure changes through sections and seasons",
       y = expression(paste("Abundance (individuals/  ", m^2, ")", sep = "")),
       fill="Functional Feeeding Group") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        strip.text.x = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12), axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12, vjust = 5),
        legend.title = element_text(size = 12, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = percent) +
  facet_wrap(~Section) 

# Same again with biomass
macro_biom_r <- melt(macro_biom)
macro_biom_r$feeding_group <- f_group

biom_ffg <- macro_biom_r %>% group_by(Section, Season, feeding_group) %>% 
  mutate(m_biom=mean(value))

biom_ffg_summ <- unique(biom_ffg[-c(2, 4,5)])

biom_ffg_plot <- ggplot(biom_ffg_summ, aes(x=Season, y=m_biom, fill=feeding_group)) +
  geom_bar(stat="identity", position = "fill", show.legend = F) +
  labs(y = expression(paste("Biomass (mg/", m^2, ")",sep = ""))) +
  theme(axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = percent) +
  facet_wrap(~Section)

# In this case we will get the shared legend between the two plots, and plot it
# as another object

# Shared legend:
s_legend <- function(a.plot){
  tmp <- ggplot_gtable(ggplot_build(a.plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
# *NOTE: in order to make the function "s_legend" work, first you need to
# activate the option "show.legend = T" in the plot that you will insert 
# as argument in the function. After you get the legend, deactivate this 
# option again.

shared_legend <- s_legend(abun_ffg_plot) 

grid.arrange(arrangeGrob(abun_ffg_plot, biom_ffg_plot),
             arrangeGrob(shared_legend),
             ncol=2, layout_matrix= rbind(c(1,1,1,1,2), c(1,1,1,1,2)))


### C) CORRELATION BETWEEN ALL THE VAIABLES

# Create the necessary data.frame, adding to the organic matter data the means
# of macroinvertebrates abundance and biomass by their feeding group

# For that, we need to reverse the "melt" step taken before
abun_ffg_means <- dcast(abun_ffg_summ, Section + Season ~feeding_group,
                        value.var = "m_abun")
corr_df_abun <- merge(om, abun_ffg_means)

# Same with biomass
biom_ffg_means <- dcast(biom_ffg_summ, Section + Season ~feeding_group,
                        value.var = "m_biom")
corr_df_biom <- merge(om, biom_ffg_means)

# Correlations:
# Abundance:
pairs.panels(corr_df_abun, hist.col = "#00AFBB", ellipses = F, lm=T)
              
# Biomass:
pairs.panels(corr_df_biom, hist.col = "#00AFBB", ellipses = F, lm=T)


# The plotting might be too big to clearly see the stablished correlations
# You can always change the data you import to the "pairs.panels" function
# to reduce it to exactly what you want.
# For example, let's see the relashionship between Section, CPOM,
# Epilithon, Shredders and Scrapers:

pairs.panels(corr_df_abun[c("Section", "CPOM", "Epilithon", "Shredder", "Scraper")],
             hist.col = "#00AFBB", ellipses = F, lm=T)

# The library "Deducer" includes a function that also creates a correlation
# plot, which I find to be better to the eye.

# First create the correlation matrix
cm <- cor.matrix(corr_df_abun[c("Section","CPOM", "Epilithon", "Shredder", "Scraper")])

# and then insert it in the function along whit the original data that the 
# correlation matrix was build on
ggcorplot(cm,
  data = corr_df_abun,
  var_text_size = 6,
  cor_text_limits = c(5,10)) +
  labs(title= "Pearson's correlation") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"))

####   THE END!   ####
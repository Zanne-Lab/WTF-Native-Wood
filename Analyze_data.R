###################################################################
# read in required packages
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(car)
library(lme4)
library(lmerTest)
library(MASSExtra)
library(emmeans)
library(gridExtra)
library(glmmTMB)
library(rcompanion)
library(betareg)
library(ggeffects)
library(viridis)
library(sdamr)
library(shades)
library(broom)
library(corrplot)

# functions to transform and back transform proportions to remove 0 and 1

transform01<-function(x){
  (x * (length (x)-1)+ 0.5) / (length(x))
}

backtransform.est<-function(x,n){
  y<-(x*n-0.5)/(n-1)
  return(y)
}

# calculate logit of proportional mass loss (add 1 to account for negative values), for models on mass loss later
# function gives warning that values over 1 are treated as percents, which makes the transform incorrect
# converted negative mass losses to zero
# function automatically rescales between 0.025 and 0.975 to avoid undefined values at pro.mass.loss = 0

###################################################################
# Read in processed csv data files and check dataset properties

df <- read.csv("Natives_processed.csv")
wood_traits <- read.csv("Wood_traits.csv")

df <- df %>%
  mutate(pro.mass.loss.tr = transform01(pro.mass.loss))

nrow(df) # n = 629, 11 blocks removed

# how many TE bags had evidence of termite.attack? n = 6
TE.dam<-df%>%
  filter(termite_treatment_abbreviation == "TE")%>%
  filter(termite.attack == 1)
TE.dam

# set species order
sp.order <- c("CAAU", "NONO", "ROAN", "ALSC", "ARPE", "CASU", "CLOB",
              "DYPA", "EUCU", "EULE", "MEST","MEVI", "MYGL","PEBA","SYSA", "TEAR")
df$Species.Code<-factor(df$Species.Code, levels = sp.order)


###################################################################
## hyp 1: Discovery rate of deadwood by termites is greater in savanna compared with rainforest
## looking at effects of site, time (months since deployment) and species on discovery (termite attack)

## binomial model testing site, time (months), and species on termite discovery
## can't include interaction because of nesting of species within site

disc.mod1<-glm(termite.attack~site+months, 
                 data=df[df$termite_treatment_abbreviation == "TI",], family=binomial)
disc.mod2<-glm(termite.attack~months+Species.Code, 
                 data=df[df$termite_treatment_abbreviation == "TI",], family=binomial)
summary(disc.mod1)
summary(disc.mod2)
car::Anova(disc.mod1) # use this for site and time effects
car::Anova(disc.mod2) # use this for species effect


###################################################################
# Plot using model discover ~ months + site binomial data

mth.sp_pred <- ggpredict(disc.mod1, c("months [12:42]", "site"))

disc.plot<-df%>%
  filter(termite_treatment_abbreviation=="TI")%>%
  ggplot(aes(x=months, y=termite.attack, colour = site))+
  scale_color_manual(values=c("red", "blue"), labels=c("Rainforest", "Savanna"))+
  geom_jitter(size=1.5,position=position_jitter(height=0.02, width=1.5))+
  geom_line(aes(x=x, y=predicted), data=filter(mth.sp_pred, group=="DRO"), 
            inherit.aes=FALSE, colour="red")+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), data=filter(mth.sp_pred, group=="DRO"), 
              inherit.aes=FALSE, alpha=0.15, linetype='dashed', colour="red", fill="red") +
  geom_line(aes(x=x, y=predicted), data=filter(mth.sp_pred, group=="PNW"), 
            inherit.aes=FALSE, colour="blue")+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), data=filter(mth.sp_pred, group=="PNW"), 
              inherit.aes=FALSE, alpha=0.25, linetype='dashed', colour="blue", fill="blue")+
  xlab("Months")+ 
  ylab("Discovery") + 
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.justification = c(0,1), legend.position=c(0.02,0.90), 
        legend.key.height= unit(0.1, 'cm'), legend.key.width= unit(0.5, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=14))+
  ggtitle ("B)")

disc.plot


###################################################################
## Calculate and plot discovery rate across all harvests for each species 

disc.rate<-df%>%
  filter(termite_treatment_abbreviation == "TI")%>%
  group_by(site, months, Species.Code)%>%
  dplyr::summarise(no.rows= length(Species.Code),
                   no.TM = length(which(termite.attack == 1)),
                   disc.rate = no.TM / no.rows) %>%
  mutate(sum = case_when(Species.Code %in% c("ALSC", "CASU", "MYGL", "EULE", "MEVI") ~ 18,
                         Species.Code %in% c("CAAU", "PEBA") ~ 19, 
                         Species.Code %in% c("NONO", "ROAN", "ARPE", "CLOB", "DYPA", "SYSA", "EUCU", "MEST" , "TEAR") ~ 20))

# Discovery rates by site to report in the paper
disc.rate.site <- ungroup(disc.rate)%>%
  group_by(site)%>%
  dplyr::summarise(no.rows = sum(no.rows), no.TM = sum(no.TM))%>%
  mutate(disc.rate = no.TM/no.rows)
disc.rate.site

# Discovery rates by species
disc.rate.species <- ungroup(disc.rate)%>%
  group_by(Species.Code)%>%
  dplyr::summarise(no.rows = sum(no.rows), no.TM = sum(no.TM))%>%
  mutate(disc.rate = no.TM/no.rows)%>%
  select(Species.Code,disc.rate)
disc.rate.species

disc.rate$months <- factor(disc.rate$months, levels = c("42", "36", "30", "24", "18", "12"))
m.lab<-c("42 (dry)", "36 (wet)", "30 (dry)", "24 (wet)", "18 (dry)", "12 (wet)")
site.labs <- c("Rainforest", "Savanna")
names(site.labs) <- c("DRO", "PNW")
sp.order <- c( "ALSC", "ARPE", "CAAU", "CASU", "CLOB","DYPA", "EUCU", "EULE", 
               "MEST","MEVI", "MYGL","NONO", "PEBA", "ROAN", "SYSA", "TEAR")
disc.rate$Species.Code<-factor(disc.rate$Species.Code, levels = sp.order)

disc.rate.plot<-disc.rate%>%
  ggplot(aes(x=Species.Code, y=(no.TM/sum)*100, group = as.factor(months), col = as.factor(months), fill = as.factor(months))) + 
  geom_bar(position="stack", stat="identity")+
  scale_color_viridis(discrete = TRUE, option = "D", labels = m.lab)+
  scale_fill_viridis(discrete = TRUE, option = "D", labels = m.lab)+
  facet_grid(~site, scale= "free", space = "free", 
             labeller = labeller(site = site.labs))+
  ylab("% of TI stems discovered \nby termites") + xlab("Species") + 
  ylim(0,100) +ggtitle ("A)")+
  labs(colour = "Months (season)", fill = "Months (season)")+
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14, angle = 45, vjust = 1, hjust=1),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position="right", 
        legend.title = element_text(size=16),
        legend.text = element_text(size=16))
disc.rate.plot

# Figure 1
g<-arrangeGrob(disc.rate.plot, disc.plot, ncol=2, nrow=1)
ggsave("Graphics/Discovery.png", g, width = 15, height = 7)


###################################################################
## hyp 2: termites accelerate decomposition more in the savanna compared with the rainforest
## beta regression models on pro mass loss
## looking at effects of discovery and site on mass loss (included station as a random factor)

beta.ran<-glmmTMB(pro.mass.loss.tr ~ termite.attack+site+months + (1|station), 
                  data=df, family=beta_family(link="logit"))
summary(beta.ran)

beta.ran2<-glmmTMB(pro.mass.loss.tr ~ termite.attack*site+months + (1|station), 
                   data=df, family=beta_family(link="logit"))
summary(beta.ran2)

beta.ran3<-glmmTMB(pro.mass.loss.tr ~ termite.attack*site+termite.attack*months + (1|station), 
                   data=df, family=beta_family(link="logit"))
summary(beta.ran3)

beta.ran4<-glmmTMB(pro.mass.loss.tr ~ termite.attack*Species.Code + months, 
                   data=df, family=beta_family(link="logit"))
summary(beta.ran4)

lmtest::lrtest(beta.ran, beta.ran2, beta.ran3)
AIC(beta.ran, beta.ran2, beta.ran3)

# Model 3 offers no improvement because there is no interaction between termite attack and time
# Using model 2
glmmTMB:::Anova.glmmTMB(beta.ran2)
performance::r2(beta.ran2)

# pairwise comparisons by site, proportional to the frequencies of discovered stems
sitecomp <- emmeans(beta.ran2,~ site, type = "response", weights = "proportional")
sitecomp
pairs(sitecomp, adjust="tukey")

# comparisons for discovered, undiscovered within each site
marginal <- emmeans(beta.ran2,~ termite.attack:site, type = "response", weights = "proportional")
pairs(marginal, adjust="tukey")
marginal1<-as.data.frame(marginal)
marginal1

# marginal mean damage effect by species
marginal.species <- as.data.frame(emmeans(beta.ran4,~ termite.attack:Species.Code, type = "response"))
damage.by.species <- marginal.species%>%
  pivot_wider(id_cols = Species.Code, names_from = termite.attack, values_from = response, names_prefix = "D")%>%
  mutate(damage.index = 100*(D1-D0)/D1)%>%
  select(Species.Code,damage.index)
damage.by.species

# range in mass loss for discovered stems
a<-df%>%
  filter(termite.attack == 0)%>%
  filter(site=="DRO")%>%
  filter(months==42)

range(a$pro.mass.loss)

# back transform estimates as response was scaled so there were no 0 or 1 values
# However, we don't see any difference in the values after back transformation
n.DRO.0 <- dim(df%>%filter(site  == "DRO" & termite.attack == 0))[1] # 356
n.DRO.1 <- dim(df%>%filter(site  == "DRO" & termite.attack == 1))[1] # 39
n.PNW.0 <- dim(df%>%filter(site  == "PNW" & termite.attack == 0))[1] # 207
n.PNW.1 <- dim(df%>%filter(site  == "PNW" & termite.attack == 1))[1] # 27

marginal1<-marginal1%>%
  mutate(emmean.tr = case_when(site  == "DRO" & termite.attack == 0 ~ backtransform.est(response, n.DRO.0),
                               site  == "DRO" & termite.attack == 1 ~ backtransform.est(response, n.DRO.1),
                               site  == "PNW" & termite.attack == 0 ~ backtransform.est(response, n.PNW.0),
                               site  == "PNW" & termite.attack == 1 ~ backtransform.est(response, n.PNW.1)))%>%
  mutate(lower.SE.tr = case_when(site  == "DRO" & termite.attack == 0 ~ backtransform.est(response-SE, n.DRO.0),
                                 site  == "DRO" & termite.attack == 1 ~ backtransform.est(response-SE, n.DRO.1),
                                 site  == "PNW" & termite.attack == 0 ~ backtransform.est(response-SE, n.PNW.0),
                                 site  == "PNW" & termite.attack == 1 ~ backtransform.est(response-SE, n.PNW.1)))%>%
  mutate(upper.SE.tr = case_when(site  == "DRO" & termite.attack == 0 ~ backtransform.est(response+SE, n.DRO.0),
                                 site  == "DRO" & termite.attack == 1 ~ backtransform.est(response+SE, n.DRO.1),
                                 site  == "PNW" & termite.attack == 0 ~ backtransform.est(response+SE, n.PNW.0),
                                 site  == "PNW" & termite.attack == 1 ~ backtransform.est(response+SE, n.PNW.1)))%>%
  mutate(counts = case_when(site  == "DRO" & termite.attack == 0 ~ n.DRO.0,
                            site  == "DRO" & termite.attack == 1 ~ n.DRO.1,
                            site  == "PNW" & termite.attack == 0 ~ n.PNW.0,
                            site  == "PNW" & termite.attack == 1 ~ n.PNW.1))
marginal1

termite<-ggplot(marginal1,aes(x = site, y = (emmean.tr*100), group = factor(termite.attack),
                              colour = factor(termite.attack), shape = factor(termite.attack)))+
  geom_errorbar(aes(ymin = (lower.SE.tr*100),
                    ymax = (upper.SE.tr*100)),
                width = 0.2,
                size  = 0.5,
                position = position_dodge(width = 0.9)) +
  geom_point(size = 3, position = position_dodge(width=0.9), stat = "identity") +
  theme_bw(base_size=16) +
  scale_colour_manual(values = c("black", "orange"))+
  ylab("Mean mass loss (%)") +
  xlab("Site")+
  scale_x_discrete(labels=c("DRO" = "Rainforest", "PNW" = "Savanna"))+
  labs(colour = "Termite discovery", shape = "Termite discovery")+
  geom_text(
    aes(label = counts, group = factor(termite.attack),
        y=100), 
    position = position_dodge(0.8),
    size = 4, show.legend = F)+
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position = "top")+
  ylim(0, 100)

# Figure 2
termite
ggsave("Graphics/termite.massloss.png", termite, width = 5, height = 5)


###################################################################
## plot mean mass remaining at each time point for each species
## use only undiscovered blocks as sample size of discovered is too small to plot the same
## and microbial decay is primarily responsible for decay

mean.undisc<-df%>%
  filter(termite.attack == 0)%>%
  add_row(months = rep(0, times = 16), 
          Species.Code = c("CAAU", "NONO", "ROAN", "ALSC", "ARPE", "CASU", "CLOB", "DYPA",
                           "MYGL", "SYSA", "EUCU", "EULE", "MEST", "MEVI", "PEBA", "TEAR"),
          site = rep(c("DRO", "PNW"), times = c(10, 6)),
          pct.mass.rem = rep(100, times = 16))%>% # add a point for time months == 0
  group_by(months, Species.Code, site) %>%
  dplyr::summarise(mean.mass.rem = mean(pct.mass.rem, na.rm = TRUE),
                   sd.mass.rem = sd(pct.mass.rem, na.rm=TRUE),
                   se.mass.rem = sd.mass.rem / sqrt(length(pct.mass.rem)),
                   upperCI = mean.mass.rem + (1.96*se.mass.rem),
                   lowerCI = mean.mass.rem - (1.96*se.mass.rem))%>%
  mutate(sd.mass.rem = ifelse(is.na(sd.mass.rem), 0, sd.mass.rem))%>%
  mutate(se.mass.rem = ifelse(is.na(se.mass.rem), 0, se.mass.rem))%>%
  print(n=Inf)

sp.order <- c( "ALSC", "ARPE", "CAAU", "CASU", "CLOB","DYPA", "EUCU", "EULE", 
               "MEST","MEVI", "MYGL","NONO", "PEBA", "ROAN", "SYSA", "TEAR")
mean.undisc$Species.Code<-factor(mean.undisc$Species.Code, levels = sp.order)

labels.minor <- c("0\nWet","6\nDry", "12\nWet", "18\nDry", "24\nWet", "30\nDry", "36\nWet", "42\nDry")

# Plotting SE instead of 95% CI
# Make sure the axis maximum allows for jittering below
ggList <- lapply(split(mean.undisc, mean.undisc$site), function(i) {
  ggplot(i, aes(x=months, y=(mean.mass.rem), colour=Species.Code, 
                group = Species.Code, linetype = Species.Code, shape = Species.Code)) + 
    geom_errorbar(aes(ymin=(mean.mass.rem-se.mass.rem), ymax=(mean.mass.rem+se.mass.rem)), width=.1, lty=1, show.legend = F) +
    geom_line() +
    geom_point(size = 2)+
    scale_shape_manual(values=1:nlevels(mean.undisc$Species.Code))+
    lightness(scale_color_viridis(discrete = T, option = "D"), scalefac(0.70)) +
    labs(colour = "Species", linetype = "Species", shape = "Species")+
    scale_x_continuous("Months since deployment", limits=c(0, 43), breaks=seq(0, 42, 6), labels = labels.minor)+
    scale_y_continuous("Mass remaining (%)", limits=c(0, 100), breaks=seq(0, 100, 10))+  
    theme_bw(base_size=16) +
    theme(plot.title = element_text(hjust=0, size=18),
          axis.text.y=element_text(size=14),
          axis.text.x=element_text(size=14),
          axis.title.y=element_text(size=18),
          axis.title.x=element_text(size=18),
          legend.position="right", 
          legend.title = element_text(size=14),
          legend.key.width= unit(1.5, 'cm'),
          legend.text = element_text(size=12))})

DRO.plot<-ggList$DRO
PNW.plot<-ggList$PNW

# plot discovered blocks in the background for visual comparison 

DRO<-df%>%
  filter(termite.attack == 1)%>%
  filter(site=="DRO")

PNW<-df%>%
  filter(termite.attack == 1)%>%
  filter(site=="PNW")

DRO$Species.Code<-as.factor(DRO$Species.Code)
PNW$Species.Code<-as.factor(PNW$Species.Code)

DRO.plot2<-DRO.plot +
  geom_point(data = DRO, 
             aes(x=months, y=pct.mass.rem, shape=Species.Code), 
             colour= "#333333", position = position_jitternudge(nudge.x=0, jitter.width = 1), size = 1, show.legend= F) +
  ggtitle("A)")

PNW.plot2<-PNW.plot +
  geom_point(data = PNW, 
             aes(x=months, y=pct.mass.rem, shape=Species.Code), 
             colour= "#333333", position = position_jitternudge(nudge.x=0, jitter.width = 1), 
             size = 1, show.legend= F) +
  theme(axis.title.y=element_blank()) +
  ggtitle("B)")


DRO.plot2
PNW.plot2

# Figure 3
g<-arrangeGrob(DRO.plot2, PNW.plot2, ncol=2, nrow=1)
ggsave("Graphics/Species.massloss.png", g, width = 15, height = 6)


###################################################################
# Calculating k-values

# Create initial values at mass remaining = 100%
init100 <- df%>%
  group_by(site,Species.Code,block)%>%
  summarise(pct.mass.rem = mean(pct.mass.rem))%>%
  mutate(pct.mass.rem = 100)%>%
  mutate(date_diff = 0)%>%
  mutate(station = interaction(site,Species.Code,block))%>%
  mutate(termite.attack = 0)

# Estimate k-values with negative exponential model
k.vals <- df%>%
  full_join(init100)%>%
  filter(termite.attack == 0)%>%
  nest_by(station)%>%
  mutate(model = list(nls(pct.mass.rem/100 ~ exp(-k*(date_diff/365)),data=data,start=list(k=1))))%>%
  summarise(tidy(model))%>%
  mutate(site = unlist(strsplit(as.character(station),"[.]"))[1])%>%
  mutate(Species.Code = unlist(strsplit(as.character(station),"[.]"))[2])%>%
  mutate(block = unlist(strsplit(as.character(station),"[.]"))[3])%>%
  rename(k=estimate)

# Fit linear models
k.model.site <- lm(log(k)~site,data=k.vals)
k.model.DRO <- lm(log(k)~Species.Code,data=k.vals[k.vals$site=="DRO",])
k.model.PNW <- lm(log(k)~Species.Code,data=k.vals[k.vals$site=="PNW",])

# All are normal with log transform
rs <- resid(k.model.DRO)
print(shapiro.test(rs))

# All highly significant
car::Anova(k.model.site)
car::Anova(k.model.DRO)
car::Anova(k.model.PNW)

# Calculate mean k-values
std.error <- function(x) sd(x)/sqrt(length(x))
k.means <- k.vals%>%
  group_by(site,Species.Code)%>%
  summarise(k.mean = mean(k), k.se = std.error(k))
k.means

k.means.site <- k.vals%>%
  group_by(site)%>%
  summarise(k.mean = mean(k), k.se = std.error(k))
k.means.site


###################################################################
# Wood trait relationships with decay and termites
df.species <- k.means%>%
  left_join(disc.rate.species)%>%
  left_join(damage.by.species)%>%
  left_join(wood_traits)%>%
  select(-k.se,-init_drywt_fraction,-mean.K.perc)

vars1 <- c("wood_density", "mean.C.perc", "mean.N.perc", "mean.S.G", "mean.pH", "mean.Ca.perc", "mean.P.perc")
corrplot(cor(df.species[vars1]),order = 'AOE',diag = F,type = 'upper',method = 'number')
# wood density negatively correlated with P and pH

pca_values <- 
  prcomp(df.species[vars1], center = TRUE, scale = TRUE)
summary(pca_values)

pca_points <- as_tibble(pca_values$x) %>% 
  bind_cols(df.species)%>%
  select(Species.Code, site, PC1, PC2)

df.species <- left_join(df.species,pca_points)

pca_load <- 
  as_tibble(pca_values$rotation, rownames = 'variable') %>% 
  mutate(variable = dplyr::recode(variable,
                                  'wood_density' = 'Density',
                                  'mean.C.perc' = '% C',
                                  'mean.N.perc' = '% N',
                                  'mean.S.G' = 'S:G ratio',
                                  'mean.pH' = 'pH',
                                  'mean.Ca.perc' = '% Ca',
                                  'mean.P.perc' = '% P'))

pca_load2<-pca_load%>%
  select(c(variable, PC1, PC2))%>%
  gather(PC1, PC2, key = PC, value = loadings)

pca_plot <- 
  ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = PC1*3.7,
                   yend = PC2*3.7),
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$PC1*4.4), y = (pca_load$PC2*4.1),
           label = pca_load$variable,
           size = 5)+
  geom_point(aes(colour = site, shape =site), size = 2.5) +
  geom_text(aes(label = Species.Code, colour = site), nudge_x = -0.3, show.legend = F)+
  scale_color_manual(values=c("blue", "red"), labels=c("Rainforest", "Savanna"))+
  scale_shape_manual(values=c(19, 17), labels=c("Rainforest", "Savanna"))+
  scale_x_continuous("PC1 (38.9%)", limits=c(-4, 4), breaks=seq(-4, 4, 2))+
  scale_y_continuous("PC2 (26.3%)", limits=c(-3, 3), breaks=seq(-2, 2, 2))+
  labs(colour = "Site", shape = "Site")+
  theme_light()+
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.justification = c(0,1), legend.position=c(0.01,0.99), 
        legend.key.height= unit(0.2, 'cm'), legend.key.width= unit(1.5, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=16)) +
  guides(colour = guide_legend(override.aes = list(size=2.5)))+
  ggtitle("A)")

pca_plot

loadings_plot <- pca_load2%>%
  ggplot(aes(x = variable, y = loadings, colour=variable, fill = variable)) +
  geom_bar(stat="identity")+
  lightness(scale_color_viridis(discrete = T, option = "D"), scalefac(0.70)) +
  lightness(scale_fill_viridis(discrete = T, option = "D"), scalefac(0.70)) +
  xlab("Wood traits")+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ylab("Factor loadings")+
  facet_wrap(~PC)+
  theme_bw()+
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        strip.text = element_text(size=18),
        legend.text = element_text(size=14),
        legend.position = "none")+
  ggtitle("B)")

loadings_plot

# Figure 4
g <- grid.arrange(arrangeGrob(pca_plot, loadings_plot, ncol=2, nrow=1, widths = c(1.5,1)))
ggsave("Graphics/pca.plot.png", g, width = 15, height = 8)

# Linear regression of log(k) versus PC1 and PC2
# Both significant negative relationships
# Model explains 85% of variance, 50% from PC2 and 35% from PC1
model.k <- lm(log(k.mean) ~ PC1 + PC2, data = df.species)
model.k1 <- lm(log(k.mean) ~ PC1, data = df.species)
model.k2 <- lm(log(k.mean) ~ PC2, data = df.species)
summary(model.k)
summary(model.k1)
summary(model.k2)
plot(log(k.mean)~PC1,data=df.species)
plot(log(k.mean)~PC2,data=df.species)

# Beta or linear regression of discovery rate vs PCs
# No relationship in either case
# Most attacked species are polar opposites on PC1 (EUCU and ROAN)
model.disc1 <- glmmTMB(disc.rate ~ PC1 + PC2, data=df.species, family=beta_family(link="logit"))
model.disc2 <- lm(log(disc.rate) ~ PC1 + PC2, data=df.species)
summary(model.disc1)
summary(model.disc2)

# Linear regression of damage index versus PCs
# Nothing significant
model.damage <- lm(damage.index ~ PC1 + PC2, data = df.species)
summary(model.damage)

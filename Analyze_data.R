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


###################################################################
# Read in processed csv data files and check dataset properties

df <- read.csv("Natives_processed.csv")
wood_traits <- read.csv("Wood_traits.csv")
pine.df <- read.csv("Pines_processed.csv")

# Add pine data to natives data
all.df <- df %>%
  full_join(pine.df)

nrow(df) # n = 629, 11 blocks removed
# n = 118; 1 TI block lost from each of DRO and PNW at harvest 7
table(pine.df$site,pine.df$months,pine.df$termite_treatment_abbreviation)
table(all.df$site,all.df$months,all.df$termite_treatment_abbreviation,all.df$Species.Code)

# how many native TE stems had evidence of termite.attack? n = 6
df%>%
  filter(termite_treatment_abbreviation == "TE")%>%
  filter(termite.attack == 1)

# how many pine TE stems had evidence of termite.attack? n = 1
pine.df%>%
  filter(termite_treatment_abbreviation == "TE")%>%
  filter(termite.attack == 1)

# set species order
sp.order <- c("CAAU", "NONO", "ROAN", "ALSC", "ARPE", "CASU", "CLOB",
              "DYPA", "EUCU", "EULE", "MEST","MEVI", "MYGL","PEBA","SYSA", "TEAR")
df$Species.Code<-factor(df$Species.Code, levels = sp.order)


###################################################################
# testing if discovery rate of deadwood by termites is greater in savanna compared with rainforest
# looking at effects of site, time (months since deployment) and species on discovery (termite attack)
# binomial model testing site, time (months), and species on termite discovery
# can't include interaction because of nesting of species within site
# checked AIC with months as categorical, but no improvement, so sticking with continuous time

disc.mod1<-glm(termite.attack~site+months, 
                 data=df[df$termite_treatment_abbreviation == "TI",], family=binomial)
disc.mod2<-glm(termite.attack~months+Species.Code, 
                 data=df[df$termite_treatment_abbreviation == "TI",], family=binomial)
summary(disc.mod1)
summary(disc.mod2)
car::Anova(disc.mod1) # use this for site and time effects
car::Anova(disc.mod2) # use this for species effect

# For pine data; both site and time are significant
disc.mod3<-glm(termite.attack~site+months, 
               data=pine.df[pine.df$termite_treatment_abbreviation == "TI",], family=binomial)
summary(disc.mod3)
car::Anova(disc.mod3) # use this for site and time effects


###################################################################
# Plot using model: discovery ~ months + site binomial data

mth.sp_pred <- ggpredict(disc.mod1, c("months [12:42]", "site"))

disc.plot<-df%>%
  filter(termite_treatment_abbreviation=="TI")%>%
  ggplot(aes(x=months, y=termite.attack, colour = site))+
  scale_color_manual(values=c("blue", "red"), labels=c("Rainforest", "Savanna"))+
  geom_jitter(size=2,position=position_jitter(height=0.0, width=1.5), alpha = 0.25)+
  geom_line(aes(x=x, y=predicted), data=filter(mth.sp_pred, group=="DRO"), 
            inherit.aes=FALSE, colour="blue")+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), data=filter(mth.sp_pred, group=="DRO"), 
              inherit.aes=FALSE, alpha=0.15, linetype='dashed', colour="blue", fill="blue") +
  geom_line(aes(x=x, y=predicted), data=filter(mth.sp_pred, group=="PNW"), 
            inherit.aes=FALSE, colour="red")+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), data=filter(mth.sp_pred, group=="PNW"), 
              inherit.aes=FALSE, alpha=0.25, linetype='dashed', colour="red", fill="red")+
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
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle ("B)")

disc.plot


mth.sp_pred.pine <- ggpredict(disc.mod3, c("months [12:42]", "site"))

disc.plot.pine<-pine.df%>%
  filter(termite_treatment_abbreviation=="TI")%>%
  ggplot(aes(x=months, y=termite.attack, colour = site))+
  scale_color_manual(values=c("blue", "red"), labels=c("Rainforest", "Savanna"))+
  geom_jitter(size=2,position=position_jitter(height=0.0, width=1.5), alpha = 0.25)+
  geom_line(aes(x=x, y=predicted), data=filter(mth.sp_pred.pine, group=="DRO"), 
            inherit.aes=FALSE, colour="blue")+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), data=filter(mth.sp_pred.pine, group=="DRO"), 
              inherit.aes=FALSE, alpha=0.15, linetype='dashed', colour="blue", fill="blue") +
  geom_line(aes(x=x, y=predicted), data=filter(mth.sp_pred.pine, group=="PNW"), 
            inherit.aes=FALSE, colour="red")+
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high), data=filter(mth.sp_pred.pine, group=="PNW"), 
              inherit.aes=FALSE, alpha=0.25, linetype='dashed', colour="red", fill="red")+
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
        legend.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle ("Pine block data")

disc.plot.pine
ggsave("Graphics/DiscoveryPine.png", disc.plot.pine)


###################################################################
# Calculate and plot discovery rate across all harvests for each species 

disc.rate<-all.df%>%
  filter(termite_treatment_abbreviation == "TI")%>%
  group_by(site, months, Species.Code)%>%
  dplyr::summarise(no.rows= length(Species.Code),
                   no.TM = length(which(termite.attack == 1)),
                   disc.rate = no.TM / no.rows)

sum.species <- disc.rate %>%
  group_by(site,Species.Code) %>%
  dplyr::summarise(sum = sum(no.rows))

disc.rate <- disc.rate %>%
  left_join(sum.species)

# Natives discovery rates by site to report in the paper
disc.rate.site.native <- disc.rate %>%
  filter(Species.Code != "PIRA") %>%
  group_by(site)%>%
  dplyr::summarise(no.rows = sum(no.rows), no.TM = sum(no.TM))%>%
  mutate(disc.rate = no.TM/no.rows)
disc.rate.site.native

# Pine discovery rates by site to report in the paper
disc.rate.site.pine <- disc.rate %>%
  filter(Species.Code == "PIRA") %>%
  group_by(site)%>%
  dplyr::summarise(no.rows = sum(no.rows), no.TM = sum(no.TM))%>%
  mutate(disc.rate = no.TM/no.rows)
disc.rate.site.pine

# Discovery rates by species
disc.rate.species <- disc.rate %>%
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
               "MEST","MEVI", "MYGL","NONO", "PEBA", "ROAN", "SYSA", "TEAR", "PIRA")
disc.rate$Species.Code<-factor(disc.rate$Species.Code, levels = sp.order)

disc.rate.plot<-disc.rate%>%
  ggplot(aes(x=Species.Code, y=(no.TM/sum)*100, group = as.factor(months), col = as.factor(months), fill = as.factor(months))) + 
  geom_bar(position="stack", stat="identity")+
  scale_color_viridis(discrete = TRUE, option = "D", labels = m.lab)+
  scale_fill_viridis(discrete = TRUE, option = "D", labels = m.lab)+
  facet_grid(~site, scale= "free", space = "free", 
             labeller = labeller(site = site.labs))+
  ylab("% of TI stems discovered \nby termites") + xlab("Species") + 
  ylim(0,100) + ggtitle ("A)")+
  labs(colour = "Months (season)", fill = "Months (season)")+
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14, angle = 45, vjust = 1, hjust=1),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position=c(0.2,0.8), 
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
disc.rate.plot

# Figure 1
g<-arrangeGrob(disc.rate.plot, disc.plot, ncol=2, nrow=1)
ggsave("Graphics/Discovery.png", g, width = 15, height = 7)


###################################################################
# testing if termites accelerate decomposition more in the savanna compared with the rainforest
# beta regression models on prop. mass loss
# looking at effects of discovery and site on mass loss (included station as a random factor)

# Generate a list object analogous to make.link() for custom transformation to remove 0 and 1
# Transformation: y = (x*(n-1)+0.5)/n; y*n-0.5 = x*(n-1); x = (y*n-0.5)/(n-1)
# Inverse: x = (y*n-0.5)/(n-1)
# Derivative: n/(n-1)
scale.tran <- list(
  linkfun = function(mu) (mu*(629-1)+0.5)/629,
  linkinv = function(eta) (eta*629-0.5)/(629-1), 
  mu.eta = function(eta) 629/(629-1),
  name = "scale.proportion"
)

scale.tran.pine <- list(
  linkfun = function(mu) (mu*(118-1)+0.5)/118,
  linkinv = function(eta) (eta*118-0.5)/(118-1), 
  mu.eta = function(eta) 118/(118-1),
  name = "scale.proportion"
)

beta.ran<-with(scale.tran,glmmTMB(linkfun(pro.mass.loss) ~ termite.attack+site+as.factor(months) + (1|station), 
                  data=df, family=beta_family(link="logit")))
summary(beta.ran)

beta.ran0<-with(scale.tran,glmmTMB(linkfun(pro.mass.loss) ~ termite.attack+site+as.factor(months), 
                  data=df, family=beta_family(link="logit")))
summary(beta.ran0)

beta.ran1<-with(scale.tran,glmmTMB(linkfun(pro.mass.loss) ~ termite.attack+site+months, 
                  data=df, family=beta_family(link="logit")))
summary(beta.ran1)

beta.ran2<-with(scale.tran,glmmTMB(linkfun(pro.mass.loss) ~ termite.attack*site+as.factor(months) + (1|station), 
                   data=df, family=beta_family(link="logit")))
summary(beta.ran2)

beta.ran3<-with(scale.tran,glmmTMB(linkfun(pro.mass.loss) ~ termite.attack*site+termite.attack*as.factor(months) + (1|station), 
                   data=df, family=beta_family(link="logit")))
summary(beta.ran3)

beta.ran4<-with(scale.tran,glmmTMB(linkfun(pro.mass.loss) ~ termite.attack*Species.Code + as.factor(months), 
                   data=df, family=beta_family(link="logit")))
summary(beta.ran4)

# With pine dataset; model does not converge with months as categorical
beta.ran5<-with(scale.tran.pine,glmmTMB(linkfun(pro.mass.loss) ~ termite.attack*site+termite.attack*months,
                                   data=pine.df, family=beta_family(link="logit")))
summary(beta.ran5)

beta.ran6<-with(scale.tran.pine,glmmTMB(linkfun(pro.mass.loss) ~ termite.attack*site+termite.attack*months + (1|station), 
                                        data=pine.df, family=beta_family(link="logit")))
summary(beta.ran6)

# Clearly better to include station as a random factor and months as categorical for natives data
lmtest::lrtest(beta.ran, beta.ran0, beta.ran1, beta.ran2, beta.ran3)
AIC(beta.ran, beta.ran0, beta.ran1, beta.ran2, beta.ran3)

# Including the random effect is better for pine data
lmtest::lrtest(beta.ran5, beta.ran6)
AIC(beta.ran5, beta.ran6)

# Interactions are significant so model 3 is best
glmmTMB:::Anova.glmmTMB(beta.ran3)

# Testing the species effect
glmmTMB:::Anova.glmmTMB(beta.ran4)

# Tests for pine data; interactions are not significant
glmmTMB:::Anova.glmmTMB(beta.ran6)

# marginal mean damage effect by species
# emmeans back-transforms automatically using the scale.tran functions
marginal.species <- summary(emmeans(beta.ran4,~ termite.attack:Species.Code, type = "response", weights = "proportional"))
damage.by.species <- as.data.frame(marginal.species)%>%
  pivot_wider(id_cols = Species.Code, names_from = termite.attack, values_from = response, names_prefix = "D")%>%
  mutate(damage.index = 100*(D1-D0)/D1)%>%
  select(Species.Code,damage.index)
damage.by.species

# pairwise comparisons by site, proportional to the frequencies of discovered stems
sitecomp <- emmeans(beta.ran3,~ site, type = "response", weights = "proportional")
pairs(sitecomp, adjust="tukey")
sitecomp

# pairwise comparisons by site, proportional to the frequencies of discovered stems (for pine)
sitecomp.pine <- emmeans(beta.ran6,~ site, type = "response", weights = "proportional")
pairs(sitecomp.pine, adjust="tukey")
sitecomp.pine

# comparisons for discovered, undiscovered within each site
# counts for plotting
term.by.site <- emmeans(beta.ran3,~ termite.attack:site, type = "response", weights = "proportional")
pairs(term.by.site, adjust="tukey")
counts <- as.data.frame(table("site"=df$site,"termite.attack"=df$termite.attack),stringsAsFactors=F) %>%
  mutate(termite.attack=as.integer(termite.attack))
term.by.site.df <- as.data.frame(summary(term.by.site)) %>%
  left_join(counts)
term.by.site.df

# comparisons for discovered, undiscovered within each site
# counts for plotting
term.by.site.pine <- emmeans(beta.ran6,~ termite.attack:site, type = "response", weights = "proportional")
pairs(term.by.site.pine, adjust="tukey")
counts <- as.data.frame(table("site"=pine.df$site,"termite.attack"=pine.df$termite.attack),stringsAsFactors=F) %>%
  mutate(termite.attack=as.integer(termite.attack))
term.by.site.pine.df <- as.data.frame(summary(term.by.site.pine)) %>%
  left_join(counts)
term.by.site.pine.df

# significant termite effect at 12, 30, 36, and 42 months but not 18 or 24 months
term.by.time <- emmeans(beta.ran3,~ termite.attack:as.factor(months), type = "response", weights = "proportional")
contrast(term.by.time, simple="termite.attack", adjust="sidak")
term.by.time

termite<-ggplot(term.by.site.df,aes(x = site, y = (response*100), group = factor(termite.attack),
                              colour = factor(termite.attack), shape = factor(termite.attack)))+
  geom_errorbar(aes(ymin = ((response-SE)*100),
                    ymax = ((response+SE)*100)),
                width = 0.2,
                size  = 0.5,
                position = position_dodge(width = 0.9)) +
  geom_point(size = 3, position = position_dodge(width=0.9), stat = "identity") +
  theme_bw(base_size=16) +
  scale_colour_manual(values = c("black", "darkorange"), labels = c("No", "Yes")) +
  scale_shape_discrete(labels = c("No", "Yes")) +
  labs(colour = "Termite discovery", shape = "Termite discovery")+
  ylab("Mean mass loss (%)") +
  xlab("Site")+
  scale_x_discrete(labels=c("DRO" = "Rainforest", "PNW" = "Savanna"))+
  geom_text(
    aes(label = Freq, group = factor(termite.attack),
        y=100), 
    position = position_dodge(0.8),
    size = 4, show.legend = F)+
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylim(0, 100)

# Figure 2
termite
ggsave("Graphics/termite.massloss.png", termite, width = 5, height = 5)


termite.pine<-ggplot(term.by.site.pine.df,aes(x = site, y = (response*100), group = factor(termite.attack),
                                    colour = factor(termite.attack), shape = factor(termite.attack)))+
  geom_errorbar(aes(ymin = ((response-SE)*100),
                    ymax = ((response+SE)*100)),
                width = 0.2,
                size  = 0.5,
                position = position_dodge(width = 0.9)) +
  geom_point(size = 3, position = position_dodge(width=0.9), stat = "identity") +
  theme_bw(base_size=16) +
  scale_colour_manual(values = c("black", "darkorange"), labels = c("No", "Yes")) +
  scale_shape_discrete(labels = c("No", "Yes")) +
  labs(colour = "Termite discovery", shape = "Termite discovery")+
  ylab("Mean mass loss (%)") +
  xlab("Site")+
  scale_x_discrete(labels=c("DRO" = "Rainforest", "PNW" = "Savanna"))+
  geom_text(
    aes(label = Freq, group = factor(termite.attack),
        y=100), 
    position = position_dodge(0.8),
    size = 4, show.legend = F)+
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylim(0, 100) +
  ggtitle ("Pine block data")

# SI figure
termite.pine
ggsave("Graphics/termite.massloss.pine.png", termite.pine, width = 5, height = 5)

###################################################################
## plot mean mass remaining at each time point for each species
## use only undiscovered blocks as sample size of discovered is small
## and microbial decay is dominant mechanism

# range in mass loss for stems
a<-df%>%
  filter(termite.attack == 0)%>%
  filter(site=="DRO")%>%
  filter(months==42)

range(a$pro.mass.loss)

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

# Make sure the axis maximum allows for jittering below
ggList <- lapply(split(mean.undisc, mean.undisc$site), function(i) {
  ggplot(i, aes(x=months, y=(mean.mass.rem), colour=Species.Code, 
                group = Species.Code, linetype = Species.Code, shape = Species.Code)) + 
    geom_errorbar(aes(ymin=(mean.mass.rem-se.mass.rem), ymax=(mean.mass.rem+se.mass.rem)), width=.1, lty=1, show.legend = F) +
    geom_line() +
    geom_point(size = 2)+
    scale_shape_manual(values=1:nlevels(mean.undisc$Species.Code))+
    lightness(scale_color_viridis(discrete = T, option = "H"), scalefac(0.75)) +
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
          legend.text = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())})

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
             colour= "black", position = position_jitternudge(nudge.x=0, jitter.width = 1),
             size = 2, show.legend= F) +
  ggtitle("A)")

PNW.plot2<-PNW.plot +
  geom_point(data = PNW, 
             aes(x=months, y=pct.mass.rem, shape=Species.Code), 
             colour= "black", position = position_jitternudge(nudge.x=0, jitter.width = 1), 
             size = 2, show.legend= F) +
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
  theme_bw()+
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.justification = c(0,1), legend.position=c(0.01,0.99), 
        legend.key.height= unit(0.2, 'cm'), legend.key.width= unit(1.5, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
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
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
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

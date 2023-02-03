##############################################################################
# Analysis of Fandom Data. Code written by Karl Lamothe 29-10-2022
# StatMando 
##############################################################################
# load packages
library(pacman)
p_load(ggplot2)     # for plotting
p_load(patchwork)   # for plotting multiple ggplots
p_load(vegan)       # for multivariate analyses
p_load(ggcorrplot)  # for correlations between variables
p_load(ggrepel)     # for nudging labels in ggplots

# read data 
Fandom.Data<-read.csv('Ultiworld_StatMando_Fandom_Data.csv', header=T, check.names = F)
colnames(Fandom.Data)

# personal ggplot theme
theme_me <- theme_bw() + 
  theme(axis.title.  = element_text(size=12, family="sans", colour="black"),
        axis.text.x  = element_text(size=11, family="sans", colour="black"),
        axis.text.y  = element_text(size=11, family="sans", colour="black"),
        legend.title = element_text(size=10, family="sans", colour="black"),
        legend.text. = element_text(size=10, family="sans", colour="black"),
        plot.title   = element_text(size=12, family="sans", colour="black" ),
        axis.ticks   = element_line(colour="black"))
theme_set(theme_me)

#~~~~~~~~~~~~~~~~~~~~~~~~~#
# Look at fandom score distributions
#~~~~~~~~~~~~~~~~~~~~~~~~~#
# For players being dropped from subsequent analyses
MPO.drops<-c(Fandom.Data$Kyle_Klein,     Fandom.Data$Corey_Ellis,
             Fandom.Data$Niklas_Anttila, Fandom.Data$Chris_Clemons,
             Fandom.Data$Cale_Leiviska,  Fandom.Data$Albert_Tamm,
             # note that you must change vainos name to include umlauts
             Fandom.Data$Vaino_Makela,   Fandom.Data$Bradley_Williams,
             Fandom.Data$Andrew_Fish)

#Create data frame for plotting
MPO.drops.df<-cbind.data.frame(MPO.drops, 
                               Player = c(rep("Kyle Klein",        length(Fandom.Data[,1])),
                                          rep("Corey Ellis",      length(Fandom.Data[,1])),
                                          rep("Niklas Anttila",   length(Fandom.Data[,1])),
                                          rep("Chris Clemons",    length(Fandom.Data[,1])),
                                          rep("Cale Leiviska",    length(Fandom.Data[,1])),
                                          rep("Albert Tamm",      length(Fandom.Data[,1])),
                                          rep("Vaino Makela",     length(Fandom.Data[,1])),
                                          rep("Bradley Williams", length(Fandom.Data[,1])),
                                          rep("Andrew Fish",      length(Fandom.Data[,1]))))

MPO.drops.df<-as.data.frame(na.omit(MPO.drops.df)) 

# Scores should be factors and ordered
MPO.drops.df$MPO.drops<-factor(MPO.drops.df$MPO.drops, levels=c(1,2,3,4,5,6,7,8,9,10))

#plot
Fandom.drops.MPO<-ggplot(MPO.drops.df, aes(y=MPO.drops))+
  geom_histogram(stat='count')+
  coord_flip()+
  xlim(0,350)+
  facet_wrap(~Player)+
  labs(x="Count",y='Fandom score')+
  theme_me

# export the figure
#tiff("MPO.drops.tiff", height=6, width=6, res=800, units='in')
Fandom.drops.MPO
#dev.off()

# For FPO players being dropped from subsequent analyses
FPO.drops<-c(Fandom.Data$Macie_Velediaz, Fandom.Data$Rebecca_Cox,
             Fandom.Data$Juliana_Korver, Fandom.Data$Emily_Beach,
             Fandom.Data$Holyn_Handley,  Fandom.Data$Maria_Oliva)

#Create data frame for plotting
FPO.drops.df<-cbind.data.frame(FPO.drops, 
                               Player = c(rep("Macie Velediaz", length(Fandom.Data[,1])),
                                          rep("Rebecca Cox",    length(Fandom.Data[,1])),
                                          rep("Juliana Korver", length(Fandom.Data[,1])),
                                          rep("Emily Beach",    length(Fandom.Data[,1])),
                                          rep("Holyn Handley",  length(Fandom.Data[,1])),
                                          rep("Maria Oliva",    length(Fandom.Data[,1]))))

FPO.drops.df<-as.data.frame(na.omit(FPO.drops.df)) 

# Scores should be factors and ordered
FPO.drops.df$FPO.drops<-factor(FPO.drops.df$FPO.drops, levels=c(1,2,3,4,5,6,7,8,9,10))

#plot
Fandom.drops.FPO<-ggplot(FPO.drops.df, aes(y=FPO.drops))+
  geom_histogram(stat='count')+
  coord_flip()+
  xlim(0,450)+
  facet_wrap(~Player)+
  labs(x="Count",y='Fandom score')+
  theme_me

# export the figure
#tiff("FPO.drops.tiff", height=4, width=6, res=800, units='in')
Fandom.drops.FPO
#dev.off()

#############################################################3
# Make data frame of people
People <- Fandom_copy[c(3:55)]

# division specific
MPO <- People[c(1:9, 20:41, 53)]
FPO <- People[c(10:19, 42:52)]

# remove Clemons, Fish, Ellis, Makela, Antilla, Klein, Tamm, Jones
colnames(MPO)
MPO.players.rm <- MPO[c(1:7,9,10,13:15,18:24,26:27,30)]
MPO.complete   <- MPO.players.rm[complete.cases(MPO.players.rm),]

# remove Cox, Velediaz, Scoggins, Corver, Handley, Oliva
colnames(FPO)
FPO.players.rm <- FPO[c(1:13,15,20)]
FPO.complete   <- FPO.players.rm[complete.cases(FPO.players.rm),]

# transform data
MPO.t <- decostand(MPO.complete, method='total')
FPO.t <- decostand(FPO.complete, method='total')

#reproducible results
set.seed(2336) 

# PCA
mod2<-rda(MPO.t ~ 1)
mod3<-rda(FPO.t ~ 1)

#summary of models
summary(mod2)
barplot(as.vector(mod2$CA$eig)/sum(mod2$CA$eig)) 
sum((as.vector(mod2$CA$eig)/sum(mod2$CA$eig))[1]) # 14.00%
sum((as.vector(mod2$CA$eig)/sum(mod2$CA$eig))[2]) # 9.90%
sum((as.vector(mod2$CA$eig)/sum(mod2$CA$eig))[1:2]) # 23.9%

summary(mod3)
barplot(as.vector(mod3$CA$eig)/sum(mod3$CA$eig)) 
sum((as.vector(mod3$CA$eig)/sum(mod3$CA$eig))[1]) # 18.37%
sum((as.vector(mod3$CA$eig)/sum(mod3$CA$eig))[2]) # 12.26% 
sum((as.vector(mod3$CA$eig)/sum(mod3$CA$eig))[1:2]) # 30.6%

##########################################
## MPO plotting
##########################################
# Observation scores (site scores)
scores <- data.frame(mod2$CA$u)
colnames(scores)

# Species scores
vscores <- data.frame(mod2$CA$v)
rownames(vscores)
rownames(vscores)<-c("McMahon", "Wysocki", "Dickerson",
                     "Heimburg", "McBeth", "Lizotte", "Buhr",
                     "Locastro", "Orum", "Hammes", "Robinson",
                     "Conrad", "Gibson", "Freeman", "Gurthie",
                     "Sexton", "Aderhold", "Smith", "Barsby",
                     "Koling", "Gilbert", "Harris")

# Create plot of model
MPO.PCA.gg <- ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept=0, linetype="dashed", col="black")+
  geom_vline(xintercept=0, linetype="dashed", col="black")+
  annotate('text', label='MPO', x=-0.33, y = 0.45)+
  geom_segment(data = vscores, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow=arrow(length = unit(0.2, "cm")), 
               color = "black", inherit.aes = FALSE, lwd=0.25) +
  geom_text(data = vscores, 
            aes(x = PC1, y = PC2, label = rownames(vscores)), 
            col = 'black', inherit.aes = FALSE, 
            nudge_y = ifelse(vscores$PC2 > 0, 0.02, -0.02),
            nudge_x = ifelse(vscores$PC1 > 0, 0.02, -0.02),
            size=3)+
  labs(x = "PCA Axis 1 (14.00%)", y = "PCA Axis 2 (9.90%)") + 
  coord_fixed(xlim=c(-0.4,0.5))+
  theme_me
MPO.PCA.gg

##########################################
# FPO plotting
##########################################
# Observation scores (site scores)
scores2 <- data.frame(mod3$CA$u)
colnames(scores2)

# Species scores
vscores2 <- data.frame(mod3$CA$v)
rownames(vscores2)
rownames(vscores2)<-c("Tattar","Pierce","C.Allen",
                      "V.Mandujano", "Salonen", "Scoggins",
                      "H.King", "Gannon", "Hokom", "Hansen",
                      'Mertsch', "Blomrooos", "Ryan", 'Panis', 'J.Allen')

# Create plot of model
PCA.FPO.gg <- ggplot(scores2, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept=0, linetype="dashed", col="black")+
  geom_vline(xintercept=0, linetype="dashed", col="black")+
  annotate('text',label='FPO', x=-0.54, y = 0.54)+
  geom_segment(data = vscores2, 
               aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow=arrow(length=unit(0.2,"cm")), 
               color = "black",inherit.aes = FALSE,lwd=0.25) +
  geom_text(data = vscores2, 
            aes(x = PC1, y = PC2, label = rownames(vscores2)), 
            col = 'black', inherit.aes = FALSE, 
            nudge_y = ifelse(vscores2$PC2 > 0, 0.02, -0.02),
            nudge_x = ifelse(vscores2$PC1 > 0, 0.02, -0.02), size=3)+
  labs(x = "PCA Axis 1 (18.37%)", y = "PCA Axis 2 (12.26%)") + 
  coord_fixed(xlim = c(-.62,0.45),
              ylim = c(-0.5,0.55))+
  theme_me
PCA.FPO.gg

#tiff('MPO_FPO_PCA.tiff',height=4, width=7, res=800, units='in')
MPO.PCA.gg + PCA.FPO.gg
#dev.off()
rm(list = ls())
## Notes: -----

# This script uses LMM model outputs and home range metrics to generate plots for the paper entitled "Differential effects of environmental predictability on ungulate movement behavior in disparate ecosystems"


## Loading packages -----
list.of.packages <- c("ggpubr", "ggeffects", "jtools","interactions","Hmisc","lattice","rms","akima","rgl","plotly","merTools","insight","dplyr","gridExtra","dplyr", "ggplot2", "tidyverse","purrr","sf","sp","viridis","hrbrthemes","GGally","RColorBrewer","car","gridExtra","scales","plm","gplots","lmtest","lme4","MuMIn","nlme","lmerTest","broom",
                      "plotly","rgl","sjPlot","sjmisc","sjlabelled","utilities","ggpmisc","effects","splines","ggpubr", "ggeffects", "jtools","interactions","Hmisc","lattice","rms","akima","rgl","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Loading in the model objects and the data -----
mod.list <- readRDS("./mod_list.RDS")
data <- readRDS("./Homerange_finaldat.RDS")

# Creating lists by season, place, and combinations of season x place
se.LIST <- setNames(split(data,data$Season), paste0(sort(unique(data$Season))))
pl.LIST <- setNames(split(data,data$CaptureUnit), paste0(sort(unique(data$CaptureUnit))))
sepl.LIST <- setNames(split(data,data$sePL), paste0(sort(unique(data$sePL))))

# Create a folder to store final plots

if(dir.exists("./figures_final") == FALSE){dir.create("./figures_final")}


## Creating plots for area and spatial and temporal constancy separated by season and capture unit (Supporting info Figure 1 and Figure 2) -----------------------------------------

seasontitles <- c("Summer","Winter")
names(seasontitles) <- c("1","2")

placetitles <- c("North Slope", "Pine Valley")
names(placetitles)<- c("1","2")

# Calculate R^2 for each facet

data_r2 <- data %>%
  group_by(CaptureUnit, Season) %>%
  nest() %>%
  # Fit linear model
  mutate(Mod = map(data, ~lm(spatial.C ~ logarea, data = .x))) %>%
  # Get the R2
  mutate(R2 = map_dbl(Mod, ~round(summary(.x)$r.squared, 4))) 



ggplot(data, aes(x = logarea, y = spatial.C,alpha=0.1)) +
  theme_bw() + #Remove grey background
  theme(legend.position = "none",text = element_text(family = "Times New Roman"))+
  geom_point(aes(color = sePL)) +
  xlab('HR area (ha)') + #x-axis
  ylab('Spatial constancy') + #y-axis
  geom_label(data = data_r2, 
             aes(x = Inf, y = Inf, 
                 label = paste("R² = ", round(R2,3), sep = "")),
             hjust = 1.2, vjust = 1.7) + 
  facet_grid(Season~ CaptureUnit, labeller = labeller(Season = seasontitles, CaptureUnit = placetitles)) + scale_color_manual(values = c("#2E8B57","#1E90FF","#9ACD32","#00CDCD"))+
  theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),strip.text = element_text(face = "bold"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks = c(2.995732,  3.912023,  5.010635,  5.991465,  6.907755,  8.006368,  8.987197, 9.998798, 11.002100),labels = c("20","50","150","400","1,000","3,000","8,000","22,000","60,000")) 

ggsave(filename = "./figures_final/area_spat.jpeg", height = 5, width = 7)


data_r2 <- data %>%
  group_by(CaptureUnit, Season) %>%
  nest() %>%
  # Fit linear model
  mutate(Mod = map(data, ~lm(temporal.C ~ logarea, data = .x))) %>%
  # Get the R2
  mutate(R2 = map_dbl(Mod, ~round(summary(.x)$r.squared, 4))) 


ggplot(data, aes(x = logarea, y = temporal.C,alpha=0.1)) +
  theme_bw() + #Remove grey background
  theme(legend.position = "none")+
  geom_point(aes(color = sePL)) +
  xlab('HR area (ha)') + #x-axis
  ylab('Temporal constancy') + #y-axis
  geom_label(data = data_r2, 
             aes(x = Inf, y = Inf, 
                 label = paste("R² = ", round(R2,3), sep = "")),
             hjust = 1.2, vjust = 1.7) +
  facet_grid(Season~ CaptureUnit, labeller = labeller(Season = seasontitles, CaptureUnit = placetitles)) + scale_color_manual(values = c("#2E8B57","#1E90FF","#9ACD32","#00CDCD"))+
  theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),strip.text = element_text(face = "bold"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks = c(2.995732,  3.912023,  5.010635,  5.991465,  6.907755,  8.006368,  8.987197, 9.998798, 11.002100),labels = c("20","50","150","400","1,000","3,000","8,000","22,000","60,000")) 


ggsave(filename = "./figures_final/area_temp.jpeg", height = 5, width = 7)

## Violin plots for capture units (Figure 3a) --------------------------------------------------------------

p.violin.area <- ggplot(data, aes(x=sePL, y=logarea, fill=sePL)) + 
  geom_hline(yintercept=c(2.5,5,7.5,10,12.5),linetype=2, color = "#CDC9C9", linewidth = .3) +
  geom_violin(trim=FALSE, alpha = 0.5)+ labs(title="",x="", y = "Hectares")+scale_fill_manual(values=c("#A9A9A9","white","#A9A9A9","white"))+ 
  geom_boxplot(width=0.1, color = "black") +  theme_classic() +
  theme(legend.position="none", legend.title = element_blank(), axis.text.x = element_blank(),text = element_text(family = "Times New Roman",color = "black", size = 22),axis.ticks = element_blank())

p.violin.area
ggsave(filename = "./figures_final/area_violin_Figure_3a.jpeg", height = 5, width = 5)

p.violin.NDVI <- ggplot(data, aes(x=sePL, y=x_NDVI, fill=sePL)) + 
  geom_hline(yintercept=c(.2,.4,.6,.8),linetype=2, color = "#CDC9C9", linewidth = .3) +
  geom_violin(trim=FALSE, alpha = 0.5)+ labs(title="",x="", y = "NDVI")+scale_fill_manual(values=c("#A9A9A9","white","#A9A9A9","white"))+ 
  geom_boxplot(width=0.1, color = "black") +  theme_classic() +
  theme(legend.position="none", legend.title = element_blank(), axis.text.x = element_blank(),text = element_text(family = "Times New Roman",color = "black", size = 22),axis.ticks = element_blank())

p.violin.NDVI
ggsave(filename = "./figures_final/NDVI_violin_Figure_3a.jpeg", height = 5, width = 5)

p.violin.dist <- ggplot(data, aes(x=sePL, y=logdist, fill=sePL)) + 
  geom_hline(yintercept=c(5,6,7,8),linetype=2, color = "#CDC9C9", linewidth = .3) +
  geom_violin(trim=FALSE,alpha = 0.5)+ labs(title="",x="", y = "Meters")+scale_fill_manual(values=c("#A9A9A9","white","#A9A9A9","white"))+ 
  geom_boxplot(width=0.1, color = "black") + theme_classic() +
  theme(legend.position="none", legend.title = element_blank(), axis.text.x = element_blank(),text = element_text(family = "Times New Roman", color = "black", size = 22),axis.ticks = element_blank())+
  scale_y_continuous(breaks = c(5.010635,5.991465,6.907755,8.006368),labels = c("150", "400","1,000","3,000")) 

## element_text(family = "Times New Roman", colour = "black", size = 11,angle = 45, hjust=.5, vjust = .5)
p.violin.dist
ggsave(filename = "./figures_final/dist_violin_Figure_3a.jpeg", height = 5, width = 5)

grid.arrange(p.violin.NDVI,p.violin.area,p.violin.dist, nrow = 2, ncol = 2, widths=c(4, 4))


## Interaction plots (Supporting info Figures 3-7) -----


## Used to determine axis values for plots with non-logged value for area
areaaxis2 <- c(3,4,5,6,7,8,9,10,11)
for(i in 1:length(areaaxis2)){
  areaaxis2[i] <- exp(areaaxis2[i])
}
areaaxis2
# 20.08554    54.59815   148.41316   403.42879  1096.63316  2980.95799 8103.08393 22026.46579 59874.14172
# Round to create nice axes
areaaxis2 <- c(20,50,150,400,1000,3000,8000,22000,60000)
for(i in 1:length(areaaxis2)){
  areaaxis2[i] <- log(areaaxis2[i])
}
areaaxis2
# 2.995732  3.912023  5.010635  5.991465  6.907755  8.006368  8.987197 9.998798 11.002100


## Used to define quantiles for spatial constancy
quantile(sepl.LIST[[1]]$spat_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[2]]$spat_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[3]]$spat_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[4]]$spat_scale, probs = c(.05, .5, .95))

spat.area.pred <- vector("list",4)
spat.area.pred[[1]] <- data.frame(ggpredict(mod.list[[1]], term = c("area_scale[all]", "spat_scale [-1.45453267, -0.03909351,  1.59665075]"), type = "re"))
spat.area.pred[[2]] <- data.frame(ggpredict(mod.list[[2]], term = c("area_scale[all]", "spat_scale [-1.2382794, -0.2642326,  1.8005955]"), type = "re"))
spat.area.pred[[3]] <- data.frame(ggpredict(mod.list[[3]], term = c("area_scale[all]", "spat_scale [-1.1696858, -0.1212212,  1.8428034]"), type = "re"))
spat.area.pred[[4]] <- data.frame(ggpredict(mod.list[[4]], term = c("area_scale[all]", "spat_scale [-1.41495616, -0.07681884,  1.95758767]"), type = "re"))

## Interaction between spatial predictability and HR area
spat.area.PLOTS <- vector("list",4)
sig.spat.area <- c("solid","dashed", "dashed","dashed")

for (i in 1:4){
  predicted.data <- spat.area.pred[[i]]
  predicted.data$area_unscaled <- predicted.data$x*sd(data$logarea) + mean(data$logarea)
  spat.area.PLOTS[[i]] <- ggplot(data=predicted.data, aes(x=area_unscaled, y=predicted)) +
  ##geom_point(data = sepl.LIST[[i]], aes(x=logarea, y = logdist,alpha=0.1))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), linetype="solid", alpha=0.2) +
  scale_fill_manual(values= c("darkorchid1","darkorchid","darkorchid4"), name = "Spatial Predictability") +
  geom_line(aes(y = predicted, color = group), size = .75) +
  scale_color_manual(values= c("darkorchid1","darkorchid","darkorchid4"), name = "Spatial Predictability") +
  xlab("") +  
    ggtitle("")+ 
  ylab("") + 
    theme_apa() + theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1,size = 10), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),legend.position="none") + scale_x_continuous(breaks = c(2.995732,  3.912023,  5.010635,  5.991465,  6.907755,  8.006368,  8.987197, 9.998798, 11.002100),labels = c("20","50","150","400","1,000","3,000","8,000","22,000","60,000")) +
  # scale_y_continuous(breaks = c(5.010635, 5.991465,6.684612,7.090077,7.377759,7.600902,7.937375,8.188689),labels = c(150,400,800,1200,1600,2000,2800,3600), limits = c(5.010635,8.188689)) 
    scale_y_continuous(breaks = c(5.010635, 5.991465,6.684612,7.377759,7.937375),labels = c("150","400","800","1,600","2,800"), limits = c(5.010635,8.188689)) 
  
}

spat_area_all <- grid.arrange(spat.area.PLOTS[[1]],spat.area.PLOTS[[3]],spat.area.PLOTS[[2]],spat.area.PLOTS[[4]],ncol = 2, nrow = 2)
ggsave(spat_area_all, filename = "./figures_final/spat_area_all.jpeg", height = 5, width = 7)



## Interaction between temporal predictability and HR area
quantile(sepl.LIST[[1]]$temp_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[2]]$temp_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[3]]$temp_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[4]]$temp_scale, probs = c(.05, .5, .95))

temp.area.pred <- vector("list",4)
temp.area.pred[[1]] <- data.frame(ggpredict(mod.list[[1]], term = c("area_scale[all]", "temp_scale [-1.17295412, -0.07220974,  1.60944597]"), type = "re"))
temp.area.pred[[2]] <- data.frame(ggpredict(mod.list[[2]], term = c("area_scale[all]", "temp_scale [-1.57843993, -0.09215715,  1.73722237]"), type = "re"))
temp.area.pred[[3]] <- data.frame(ggpredict(mod.list[[3]], term = c("area_scale[all]", "temp_scale [-1.8048825, -0.1012047,  2.0731020]"), type = "re"))
temp.area.pred[[4]] <- data.frame(ggpredict(mod.list[[4]], term = c("area_scale[all]", "temp_scale [-1.18693904, -0.09310741,  1.47263151]"), type = "re"))


temp.area.PLOTS <- vector("list",4)
sig.temp.area <- c("solid","dashed","dashed","dashed")
for (i in 1:4){
    predicted.data <- temp.area.pred[[i]]
    predicted.data$area_unscaled <- predicted.data$x*sd(data$logarea) + mean(data$logarea)
    temp.area.PLOTS[[i]] <- ggplot(data=predicted.data, aes(x=area_unscaled, y=predicted)) +
    ##geom_point(data = sepl.LIST[[i]], aes(x=logarea, y = logdist,alpha=0.1))+
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), linetype="solid", alpha=0.2) +
    scale_fill_manual(values= c("brown1","brown","brown4"), name = "Temporal Predictability") +
    geom_line(aes(y = predicted, color = group), size = .75) +
    scale_color_manual(values= c("brown1","brown","brown4"), name = "Temporal Predictability") +
    xlab("") +  ggtitle("")+
    ylab("") + theme_apa() + theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1,size = 10), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),legend.position="none") + scale_x_continuous(breaks = c(2.995732,  3.912023,  5.010635,  5.991465,  6.907755,  8.006368,  8.987197, 9.998798, 11.002100),labels = c("20","50","150","400","1,000","3,000","8,000","22,000","60,000")) +
    # scale_y_continuous(breaks = c(5.010635, 5.991465,6.684612,7.090077,7.377759,7.600902,7.937375,8.188689),labels = c(150,400,800,1200,1600,2000,2800,3600), limits = c(5.010635,8.188689))
      scale_y_continuous(breaks = c(5.010635, 5.991465,6.684612,7.377759,7.937375),labels = c("150","400","800","1,600","2,800"), limits = c(5.010635,8.188689))
    
}


temp_area_all <- grid.arrange(temp.area.PLOTS[[1]],temp.area.PLOTS[[3]],temp.area.PLOTS[[2]],temp.area.PLOTS[[4]],ncol = 2, nrow = 2)
ggsave(temp_area_all, filename = "./figures_final/temp_area_all.jpeg", height = 5, width = 7)

## Interaction between NDVI and spatial predictability
quantile(sepl.LIST[[1]]$spat_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[2]]$spat_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[3]]$spat_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[4]]$spat_scale, probs = c(.05, .5, .95))

spat.NDVI.pred <- vector("list",4)
spat.NDVI.pred[[1]] <- data.frame(ggpredict(mod.list[[1]], term = c("NDVI_scale[all]", "spat_scale [-1.45453267, -0.03909351,  1.59665075]"), type = "re"))
spat.NDVI.pred[[2]] <- data.frame(ggpredict(mod.list[[2]], term = c("NDVI_scale[all]", "spat_scale [-1.2382794, -0.2642326,  1.8005955]"), type = "re"))
spat.NDVI.pred[[3]] <- data.frame(ggpredict(mod.list[[3]], term = c("NDVI_scale[all]", "spat_scale [-1.1696858, -0.1212212,  1.8428034]"), type = "re"))
spat.NDVI.pred[[4]] <- data.frame(ggpredict(mod.list[[4]], term = c("NDVI_scale[all]", "spat_scale [-1.41495616, -0.07681884,  1.95758767]"), type = "re"))

spat.NDVI.PLOTS <- vector("list",4)
sig.spat.NDVI <- c("solid","dashed","solid","dashed")
for (i in 1:4){
    predicted.data <- spat.NDVI.pred[[i]]
    predicted.data$NDVI_unscaled <- predicted.data$x*sd(data$x_NDVI) + mean(data$x_NDVI)
    spat.NDVI.PLOTS[[i]] <- ggplot(data=predicted.data, aes(x=NDVI_unscaled, y=predicted)) +
    ##geom_point(data = sepl.LIST[[i]], aes(x=x_NDVI, y = logdist,alpha=0.1))+
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), linetype="solid", alpha=0.2) +
    scale_fill_manual(values= c("lightgreen","#00CD66","darkgreen"), name = "Spatial Predictability") +
    geom_line(aes(y = predicted, color = group), size = .75) +
    scale_color_manual(values= c("lightgreen","#00CD66","darkgreen"), name = "Spatial Predictability") +
    xlab("") +  ggtitle("")+
    ylab("") + theme_apa() + theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1,size = 10), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),legend.position="none") +
    # scale_y_continuous(breaks = c(5.991465,6.684612,7.090077,7.377759,7.600902,7.937375,8.188689),labels = c(400,800,1200,1600,2000,2800,3600), limits = c(5.7,8.188689))
    scale_y_continuous(breaks = c(5.991465,6.684612,7.377759,7.937375),labels = c("400","800","1,600","2,800"), limits = c(5.7,8.188689))
    
}

spat_NDVI_all <- grid.arrange(spat.NDVI.PLOTS[[1]],spat.NDVI.PLOTS[[3]],spat.NDVI.PLOTS[[2]],spat.NDVI.PLOTS[[4]],ncol = 2, nrow = 2)
ggsave(spat_NDVI_all, filename = "./figures_final/spat_NDVI_all.jpeg", height = 5, width = 7)


## Interaction between NDVI and temporal predictability
quantile(sepl.LIST[[1]]$temp_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[2]]$temp_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[3]]$temp_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[4]]$temp_scale, probs = c(.05, .5, .95))

temp.NDVI.pred <- vector("list",4)
temp.NDVI.pred[[1]] <- data.frame(ggpredict(mod.list[[1]], term = c("NDVI_scale[all]", "temp_scale [-1.17295412, -0.07220974,  1.60944597]"), type = "re"))
temp.NDVI.pred[[2]] <- data.frame(ggpredict(mod.list[[2]], term = c("NDVI_scale[all]", "temp_scale [-1.57843993, -0.09215715,  1.73722237]"), type = "re"))
temp.NDVI.pred[[3]] <- data.frame(ggpredict(mod.list[[3]], term = c("NDVI_scale[all]", "temp_scale [-1.8048825, -0.1012047,  2.0731020]"), type = "re"))
temp.NDVI.pred[[4]] <- data.frame(ggpredict(mod.list[[4]], term = c("NDVI_scale[all]", "temp_scale [-1.18693904, -0.09310741,  1.47263151]"), type = "re"))

temp.NDVI.PLOTS <- vector("list",4)
sig.temp.NDVI <- c("dashed","dashed","dashed","dashed")
for (i in 1:4){
    predicted.data <- temp.NDVI.pred[[i]]
    predicted.data$NDVI_unscaled <- predicted.data$x*sd(data$x_NDVI) + mean(data$x_NDVI)
    temp.NDVI.PLOTS[[i]] <- ggplot(data=predicted.data, aes(x=NDVI_unscaled, y=predicted)) +
    ##geom_point(data = sepl.LIST[[i]], aes(x=x_NDVI, y = logdist,alpha=0.1))+
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), linetype=paste0(sig.temp.NDVI[i]), alpha=0.2) +
    scale_fill_manual(values= c("darkorange","darkorange3","darkorange4"), name = "Temporal Predictability") +
    geom_line(aes(y = predicted, color = group), size = .75,linetype="solid") +
    scale_color_manual(values= c("darkorange","darkorange3","darkorange4"), name = "Temporal Predictability") +
    xlab("") +  ggtitle("")+
    ylab("") + theme_apa() + theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1,size = 10), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),legend.position="none") + ##scale_x_continuous(breaks = c(0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55, 0.6,0.65,0.7,0.75),limits = c(0.15,0.75) ) + 
    # scale_y_continuous(breaks = c(5.991465,6.684612,7.090077,7.377759,7.600902),labels = c(400,800,1200,1600,2000), limits = c(5.8,7.8))
      scale_y_continuous(breaks = c(5.991465,6.684612,7.090077,7.600902),labels = c(400,800,1200,2000), limits = c(5.8,7.8))
    
}


temp_NDVI_all <- grid.arrange(temp.NDVI.PLOTS[[1]],temp.NDVI.PLOTS[[3]],temp.NDVI.PLOTS[[2]],temp.NDVI.PLOTS[[4]],ncol = 2, nrow = 2)
ggsave(temp_NDVI_all, filename = "./figures_final/temp_NDVI_all.jpeg", height = 5, width = 7)


## Interaction between spatial and temporal predictability
quantile(sepl.LIST[[1]]$temp_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[2]]$temp_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[3]]$temp_scale, probs = c(.05, .5, .95))
quantile(sepl.LIST[[4]]$temp_scale, probs = c(.05, .5, .95))

temp.spat.pred <- vector("list",4)
temp.spat.pred[[1]] <- data.frame(ggpredict(mod.list[[1]], term = c("spat_scale[all]", "temp_scale [-1.17295412, -0.07220974,  1.60944597]"), type = "re"))
temp.spat.pred[[2]] <- data.frame(ggpredict(mod.list[[2]], term = c("spat_scale[all]", "temp_scale [-1.57843993, -0.09215715,  1.73722237]"), type = "re"))
temp.spat.pred[[3]] <- data.frame(ggpredict(mod.list[[3]], term = c("spat_scale[all]", "temp_scale [-1.8048825, -0.1012047,  2.0731020]"), type = "re"))
temp.spat.pred[[4]] <- data.frame(ggpredict(mod.list[[4]], term = c("spat_scale[all]", "temp_scale [-1.18693904, -0.09310741,  1.47263151]"), type = "re"))

temp.spat.PLOTS <- vector("list",4)
sig.temp.NDVI <- c("dashed","dashed","dashed","dashed")
for (i in 1:4){
    predicted.data <- temp.spat.pred[[i]]
    predicted.data$spat_unscaled <- predicted.data$x*sd(data$spat_resid) + mean(data$spat_resid)
    temp.spat.PLOTS[[i]] <- ggplot(data=predicted.data, aes(x=spat_unscaled, y=predicted)) +
    ##geom_point(data = sepl.LIST[[i]], aes(x=x_NDVI, y = logdist,alpha=0.1))+
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), linetype=paste0(sig.temp.NDVI[i]), alpha=0.2) +
    scale_fill_manual(values= c("darkgoldenrod1","darkgoldenrod3","darkgoldenrod4"), name = "Temporal Predictability") +
    geom_line(aes(y = predicted, color = group), size = .75,linetype="solid") +
    scale_color_manual(values= c("darkgoldenrod1","darkgoldenrod3","darkgoldenrod4"), name = "Temporal Predictability") +
    xlab("") +  ggtitle("")+
    ylab("") + theme_apa() + theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1,size = 10), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),legend.position="none") +
    # scale_y_continuous(breaks = c(5.991465,6.684612,7.090077,7.377759,7.600902,7.937375),labels = c(400,800,1200,1600,2000,2800), limits = c(5.6,7.937375))
    scale_y_continuous(breaks = c(5.991465,6.684612,7.377759,7.937375),labels = c("400","800","1,600","2,800"), limits = c(5.6,7.937375))
    
}

temp_spat_all <- grid.arrange(temp.spat.PLOTS[[1]],temp.spat.PLOTS[[3]],temp.spat.PLOTS[[2]],temp.spat.PLOTS[[4]],ncol = 2, nrow = 2)
ggsave(temp_spat_all, filename = "./figures_final/temp_spat_all.jpeg", height = 5, width = 7)


## Interaction plots for significant effects (Figure 4) -----

predicted.data <- spat.area.pred[[1]]
predicted.data$area_unscaled <- predicted.data$x*sd(data$logarea) + mean(data$logarea)
NSspatarea <- ggplot(data=predicted.data, aes(x=area_unscaled, y=predicted)) +
  ##geom_point(data = sepl.LIST[[i]], aes(x=logarea, y = logdist,alpha=0.1))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), linetype="solid", alpha=0.2) +
  scale_fill_manual(values= c("darkorchid1","darkorchid","darkorchid4"), name = "Spatial Predictability") +
  geom_line(aes(y = predicted, color = group), size = .75) +
  scale_color_manual(values= c("darkorchid1","darkorchid","darkorchid4"), name = "Spatial Predictability") +
  xlab("HR area (ha)") +  ggtitle("")+ 
  ylab("") + theme_apa() + theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1,size = 10), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),legend.position="none") + scale_x_continuous(breaks = c(2.995732,  3.912023,  5.010635,  5.991465,  6.907755,  8.006368,  8.987197, 9.998798, 11.002100),labels = c("20","50","150","400","1,000","3,000","8,000","22,000","60,000")) +
  scale_y_continuous(breaks = c(5.991465,6.684612,7.090077,7.377759,7.600902,7.937375,8.188689),labels = c("400","800","1,200","1,600","2,000","2,800","3,600"), limits = c(5.9,8.188689)) 

predicted.data <- temp.area.pred[[1]]
predicted.data$area_unscaled <- predicted.data$x*sd(data$logarea) + mean(data$logarea)
NStemparea <- ggplot(data=predicted.data, aes(x=area_unscaled, y=predicted)) +
  ##geom_point(data = sepl.LIST[[i]], aes(x=logarea, y = logdist,alpha=0.1))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), linetype="solid", alpha=0.2) +
  scale_fill_manual(values= c("brown1","brown","brown4"), name = "Temporal Predictability") +
  geom_line(aes(y = predicted, color = group), size = .75) +
  scale_color_manual(values= c("brown1","brown","brown4"), name = "Temporal Predictability") +
  xlab("HR area (ha)") +  ggtitle("")+
  ylab("") + theme_apa() + theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1,size = 10), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),legend.position="none") + scale_x_continuous(breaks = c(2.995732,  3.912023,  5.010635,  5.991465,  6.907755,  8.006368,  8.987197, 9.998798, 11.002100),labels = c("20","50","150","400","1,000","3,000","8,000","22,000","60,000")) +
  scale_y_continuous(breaks = c(5.991465,6.684612,7.090077,7.377759,7.600902,7.937375,8.188689),labels = c("400","800","1,200","1,600","2,000","2,800","3,600"), limits = c(5.9,8.188689))


top_row <- grid.arrange(NSspatarea,NStemparea, ncol = 2, nrow = 1)
ggsave(top_row, filename = "./figures_final/top_figure_4.jpeg", height = 3, width = 7)




predicted.data <- spat.NDVI.pred[[1]]
predicted.data$ndvi_unscaled <- predicted.data$x*sd(data$NDVI_scale) + mean(data$NDVI_scale)
NSspatNDVI <- ggplot(data=predicted.data, aes(x=ndvi_unscaled, y=predicted)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), linetype="solid", alpha=0.2) +
  scale_fill_manual(values= c("lightgreen","#00CD66","darkgreen"), name = "Spatial Predictability") +
  geom_line(aes(y = predicted, color = group), size = .75) +
  scale_color_manual(values= c("lightgreen","#00CD66","darkgreen"), name = "Spatial Predictability") +
  xlab("NDVI") +  ggtitle("")+ 
  ylab("") + theme_apa() + theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1,size = 10), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),legend.position="none") + scale_x_continuous(breaks = c(-0.3213043, 0.5069903, 1.3352848, 2.1635793),labels = c(.4, .5, .6, .7)) +
  scale_y_continuous(breaks = c(5.991465,6.684612,7.090077,7.377759,7.600902,7.937375,8.188689),labels = c("400","800","1,200","1,600","2,000","2,800","3,600"), limits = c(5.7,8.188689)) 

predicted.data <- spat.NDVI.pred[[3]]
predicted.data$ndvi_unscaled <- predicted.data$x*sd(data$NDVI_scale) + mean(data$NDVI_scale)
PVspatNDVI <- ggplot(data=predicted.data, aes(x=ndvi_unscaled, y=predicted)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), linetype="solid", alpha=0.2) +
  scale_fill_manual(values= c("lightgreen","#00CD66","darkgreen"), name = "Temporal Predictability") +
  geom_line(aes(y = predicted, color = group), size = .75) +
  scale_color_manual(values= c("lightgreen","#00CD66","darkgreen"), name = "Temporal Predictability") +
  xlab("NDVI") +  ggtitle("")+
  ylab("") + theme_apa() + theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1,size = 10), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),legend.position="none") + scale_x_continuous(breaks = c(-0.3213043, 0.5069903, 1.3352848, 2.1635793),labels = c(.4, .5, .6, .7)) +
  scale_y_continuous(breaks = c(5.991465,6.684612,7.090077,7.377759,7.600902,7.937375,8.188689),labels = c("400","800","1,200","1,600","2,000","2,800","3,600"), limits = c(5.7,8.188689))


middle_row <- grid.arrange(NSspatNDVI,PVspatNDVI, ncol = 2, nrow = 1)
ggsave(middle_row, filename = "./figures_final/middle_figure_4.jpeg", height = 3, width = 7)




predicted.data <- temp.spat.pred[[3]]
predicted.data$spat_unscaled <- predicted.data$x*sd(data$spat_resid) + mean(data$spat_resid)
PVspattemp <- ggplot(data=predicted.data, aes(x=spat_unscaled, y=predicted)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), linetype="solid", alpha=0.2) +
  scale_fill_manual(values= c("darkgoldenrod1","darkgoldenrod3","darkgoldenrod4"), name = "Temporal Predictability") +
  geom_line(aes(y = predicted, color = group), size = .75) +
  scale_color_manual(values= c("darkgoldenrod1","darkgoldenrod3","darkgoldenrod4"), name = "Temporal Predictability") +
  xlab("Spatial constancy") +  ggtitle("")+
  ylab("") + theme_apa() + theme(text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1,size = 10), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),legend.position="none")  +
  scale_y_continuous(breaks = c(5.991465,6.684612,7.090077,7.377759,7.600902,7.937375,8.188689),labels = c("400","800","1,200","1,600","2,000","2,800","3,600"), limits = c(5.7,8.188689))


bottom <- grid.arrange(PVspattemp,PVspattemp, ncol = 2, nrow = 1)
ggsave(bottom, filename = "./figures_final/bottom_figure_4.jpeg", height = 3, width = 7)

all <- grid.arrange(NSspatarea,NStemparea,NSspatNDVI,PVspatNDVI,PVspattemp, ncol = 2, nrow = 3)
ggsave(all, filename = "./figures_final/Figure_4.jpeg", height = 9, width = 7)



## Main Effect plots for NDVI and predictability (Figure 3b) -----
Effects <- c("Spatial", "Temporal", "NDVI")

# Extracting information from model objects about fixed effects of spatial, temporal and NDVI
effects_list <- list()
for(i in 1:4){
  summary<- as.data.frame(summary(mod.list[[i]])$coefficients[5:7,]) # pulling the information
  sig <- ifelse(summary$`Pr(>|t|)` < 0.05, "Y", "N") # determining if effect significant
  CONF <- as.data.frame(confint(mod.list[[i]]),method="Wald") # extracting the 95% CI
  effects_list[[i]] <- cbind(summary,sig,Effects,CONF[8:10,]) # creating a dataframe
}

# Combining all model inforation together
EFF <- bind_rows(effects_list)

# Creating info for plotting
CaptureUnit <- c(rep("North Slope", times = 6),rep("Pine Valley", times = 6))
Season <- c(rep("Summer", times = 3),rep("Winter", times = 3),rep("Summer", times = 3),rep("Winter",times = 3))
EFF <- cbind(EFF,CaptureUnit,Season)
names(EFF) <- c("Estimate","Error","df","t-value","p-value","sig","Effects","lower","upper","CaptureUnit","Season")
EFF$sig_shape <- paste(EFF$sig,EFF$CaptureUnit)

# Splitting info a list based on season for plotting
EFF.LIST <- setNames(split(EFF,EFF$Season), paste0(sort(unique(EFF$Season))))

param.col.S <- c(rep(c("black","black"),times = 3))
param.col.W <- c(rep(c("black","black"),times = 3))

pd <- position_dodge(0.4)

## 1) Creating Summer plot
sort(unique(EFF.LIST[[1]]$sig_shape))
plot.summer <- ggplot(EFF.LIST[[1]], aes(x=Effects, y=(Estimate),group=CaptureUnit, colour =CaptureUnit, shape = sig_shape)) + 
  geom_hline(yintercept = 0, col = "#CDC9C9") +
  geom_hline(yintercept = c(0.2,-0.2), linetype=2, color = "#CDC9C9", linewidth = .3) +
  geom_errorbar(aes (ymin=(lower), ymax=(upper)), width=.2, position=pd) +
  geom_point(position=pd, size = 4)+ 
  ylim(min(EFF.LIST[[1]]$lower),max(EFF.LIST[[2]]$upper))+
  scale_color_manual(values= param.col.S, name = "Capture Unit")+ ggtitle("") + ylab("") + xlab("") +
   theme_apa() + theme(legend.position="none", legend.title = element_blank(),text = element_text(family = "Times New Roman",color = "black", size = 16)) + scale_shape_manual(values = c(2,1,17,19),name = "Significance")

## 2) Creating winter plot
EFF.LIST[[2]]$sig_shape
plot.winter <- ggplot(EFF.LIST[[2]], aes(x=Effects, y=(Estimate),group=CaptureUnit, colour =CaptureUnit, shape = sig_shape)) + 
  geom_hline(yintercept = 0, col = "#CDC9C9")  +
  geom_hline(yintercept = c(0.2,-0.2), linetype=2, color = "#CDC9C9", linewidth = .3) +
  geom_errorbar(aes (ymin=(lower), ymax=(upper)), width=.2, position=pd) +
  geom_point(position=pd, size = 4)+ 
  ylim(min(EFF.LIST[[1]]$lower),max(EFF.LIST[[2]]$upper))+
  scale_color_manual(values= param.col.W, name = "Capture Unit")+ ggtitle("") + ylab("") + xlab("") +
   theme_apa() + theme(legend.title = element_blank(),text = element_text(family = "Times New Roman",color = "black", size = 16)) + scale_shape_manual(values = c(2,1,17,19),name = "Significance") 


# Using the legend to create final ms plot
legend.eff <- get_legend(plot.winter)
as_ggplot(legend.eff)

## Re-creating winter plot
EFF.LIST[[2]]$sig_shape
plot.winter <- ggplot(EFF.LIST[[2]], aes(x=Effects, y=(Estimate),group=CaptureUnit, colour =CaptureUnit, shape = sig_shape)) + 
  geom_hline(yintercept = 0, col = "#CDC9C9")  +
  geom_hline(yintercept = c(0.2,-0.2), linetype=2, color = "#CDC9C9", linewidth = .3) +
  geom_errorbar(aes (ymin=(lower), ymax=(upper)), width=.2, position=pd) +
  geom_point(position=pd, size = 4)+ 
  ylim(min(EFF.LIST[[1]]$lower),max(EFF.LIST[[2]]$upper))+
  scale_color_manual(values= param.col.W, name = "Capture Unit")+ ggtitle("") + ylab("") + xlab("") +
  theme_apa() + theme(legend.position="none", legend.title = element_blank(),text = element_text(family = "Times New Roman",color = "black", size = 16)) + scale_shape_manual(values = c(2,1,17,19),name = "Significance") 


eff_plot <- grid.arrange(plot.summer,plot.winter, nrow = 1, ncol = 2)
ggsave(eff_plot, filename = "./figures_final/eff_plot_figure_3b.jpeg", height = 4, width = 7)



## Random effects information and plots (Supporting info Figure 8 and Figure 9) -----

# Extracting random effect information from model objects
NSRandom <- broom.mixed::tidy(mod.list[[1]], effects = "ran_vals", conf.int = TRUE)
NSRandom <- as.data.frame(NSRandom)
NSRandomAN <- NSRandom[c(1:55),]
NSRandomAN$sePL <- "North Slope Summer"
NSRandomAN$Season <- "Summer"
NSRandomAN$CaptureUnit <- "North Slope"
NSRandomYR <- NSRandom[c(56:59),]
NSRandomYR$sePL <- "North Slope Summer"
NSRandomYR$Season <- "Summer"
NSRandomYR$CaptureUnit <- "North Slope"

NWRandom <- broom.mixed::tidy(mod.list[[2]], effects = "ran_vals", conf.int = TRUE)
NWRandom <- as.data.frame(NWRandom)
NWRandomAN <- NWRandom[c(1:64),]
NWRandomAN$sePL <- "North Slope Winter"
NWRandomAN$Season <- "Winter"
NWRandomAN$CaptureUnit <- "North Slope"
NWRandomYR <- NWRandom[c(65:68),]
NWRandomYR$sePL <- "North Slope Winter"
NWRandomYR$Season <- "Winter"
NWRandomYR$CaptureUnit <- "North Slope"

PSRandom <- broom.mixed::tidy(mod.list[[3]], effects = "ran_vals", conf.int = TRUE)
PSRandom <- as.data.frame(PSRandom)
PSRandomAN <- PSRandom[c(1:112),]
PSRandomAN$sePL <- "Pine Valley Summer"
PSRandomAN$Season <- "Summer"
PSRandomAN$CaptureUnit <- "Pine Valley"
PSRandomYR <- PSRandom[c(113:118),]
PSRandomYR$sePL <- "Pine Valley Summer"
PSRandomYR$Season <- "Summer"
PSRandomYR$CaptureUnit <- "Pine Valley"

PWRandom <- broom.mixed::tidy(mod.list[[4]], effects = "ran_vals", conf.int = TRUE)
PWRandom <- as.data.frame(PWRandom)
PWRandomAN <- PWRandom[c(1:158),]
PWRandomAN$sePL <- "Pine Valley Winter"
PWRandomAN$Season <- "Winter"
PWRandomAN$CaptureUnit <- "Pine Valley"
PWRandomYR <- PWRandom[c(159:165),]
PWRandomYR$sePL <- "Pine Valley Winter"
PWRandomYR$Season <- "Winter"
PWRandomYR$CaptureUnit <- "Pine Valley"

# Creating info for plotting
seasontitles <- c("Summer","Winter")
names(seasontitles) <- c("Summer","Winter")

placetitles <- c("North Slope", "Pine Valley")
names(placetitles)<- c("North Slope","Pine Valley")

place.pallette <- c("#2E8B57","#1E90FF","#9ACD32","#00CDCD")

## 1) Year random effect
YEARrandom <- rbind(NSRandomYR,NWRandomYR,PSRandomYR,PWRandomYR)
REyearLIST <- setNames(split(YEARrandom,YEARrandom$sePL), paste0(sort(unique(YEARrandom$sePL))))

for (i in 1:4){ 
  
REyearLIST[[i]] <- ggplot(REyearLIST[[i]], aes(x = estimate, y = level)) +
  theme_bw() + #Remove grey background
  geom_vline(xintercept = 0, color = "lightgray")+
  theme(legend.position = "none")+
  geom_errorbar(aes (xmin=(conf.low), xmax=(conf.high)), width=.2, position=pd) +
  geom_point(color = place.pallette[i], size = 3) +
  xlab('Estimate') + #x-axis
  ylab('Year(s)') + #y-axis
  theme(text = element_text(family = "Times New Roman"), axis.text.y = element_text(family = "Times New Roman",angle = 45,vjust = 0.5, hjust=1),axis.text.x = element_text(family = "Times New Roman"), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),strip.text = element_text(face = "bold"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
}

RE_year <- grid.arrange(REyearLIST[[1]],REyearLIST[[3]],REyearLIST[[2]],REyearLIST[[4]], ncol = 2, nrow = 2)

ggsave(RE_year, filename = "./figures_final/RE_year.jpeg", height = 7, width = 7)


## 2) Individual animal random effect
ANIMrandom <- rbind(NSRandomAN,NWRandomAN,PSRandomAN,PWRandomAN)
REanimalLIST <- setNames(split(ANIMrandom,ANIMrandom$sePL), paste0(sort(unique(ANIMrandom$sePL))))

for (i in 1:4){
  REanimalLIST[[i]] <- REanimalLIST[[i]][order(REanimalLIST[[i]]$estimate,decreasing=TRUE),]
}

for (i in 1:4){
REanimalLIST[[i]] <- ggplot(REanimalLIST[[i]], aes(x = estimate, y = fct_inorder(level))) +
  theme_bw() + #Remove grey background
  geom_vline(xintercept = 0, color = "lightgray")+
  theme(legend.position = "none")+
  geom_errorbar(aes (xmin=(conf.low), xmax=(conf.high)), width=.1, position=pd) +
  geom_point(color = place.pallette[i]) +
  xlab('Estimate') + #x-axis
  ylab('Animal') + #y-axis
  theme(text = element_text(family = "Times New Roman"), axis.text.x = element_text(family = "Times New Roman"), axis.text.y = element_blank(), title = element_text(family = "Times New Roman", hjust = 0.5), axis.title = element_text(family = "Times New Roman"),strip.text = element_text(face = "bold"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.ticks.y=element_blank())

}

RE_animal <- grid.arrange(REanimalLIST[[1]],REanimalLIST[[3]],REanimalLIST[[2]],REanimalLIST[[4]], ncol = 2, nrow = 2)
ggsave(RE_animal, filename = "./figures_final/RE_animal.jpeg", height = 7, width = 7)



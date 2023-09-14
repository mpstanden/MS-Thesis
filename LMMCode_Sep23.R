### Code for LMM 
### Madeline P. Standen, 2023

#### Loading in Packages ####
list.of.packages <- c("dplyr", "ggplot2", "tidyverse","purrr","sf","sp","viridis","hrbrthemes","GGally","RColorBrewer","car","gridExtra","scales","plm","gplots","lmtest","lme4","MuMIn","nlme","lmerTest","broom",
                      "plotly","rgl","sjPlot","sjmisc","sjlabelled","utilities","ggpmisc","effects","splines")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(c("dplyr", "ggplot2", "tidyverse","purrr","sf","sp","viridis","hrbrthemes","GGally","RColorBrewer","car","gridExtra","scales","plm","gplots","lmtest","lme4","MuMIn","nlme","lmerTest","broom",
         "plotly","rgl","sjPlot","sjmisc","sjlabelled","utilities","ggpmisc","effects","splines"), require, character.only = TRUE)

## For later plotting/creation of tables
theme_set(theme_sjplot())
COL <- c("#2E8B57","#1E90FF","#9ACD32","#00CDCD")
seasons <- c("North Slope Summer","North Slope Winter","Pine Valley Summer","Pine Valley Winter")
Effects <- c("Spatial", "Temporal", "NDVI")

## Set working directory
wd <- getwd()

data <- readRDS("~wd/Homerange.RDS")

## Separating out data for model
data.s <- data[,c("spat_resid","spat_bin","spatial.C","temp_resid","temp_bin","temporal.C","logarea","logdist","x_NDVI","NDVI_bin","SD_NDVI","step.NDVI_x","step.NDVI_SD","animal","year","Sex","CaptureUnit","Season","sePL_col","sePL", "c.prop", "n.prop", "d.prop","year","contingency")]


## Scaling data
data.s <- data.s %>% mutate(area_scale = scale(logarea),
                            NDVI_scale = scale(x_NDVI),
                            spat_scale = scale(spat_resid),
                            temp_scale = scale(temp_resid),
                            crop_scale = scale(c.prop),
                            dev_scale = scale(d.prop),
                            nat_scale = scale(n.prop),
                            cont_scale = scale(contingency),.before = "spat_resid")

## Creating lists by season, place, and combinations of season x place
se.LIST <- setNames(split(data.s,data.s$Season), paste0(sort(unique(data.s$Season))))
pl.LIST <- setNames(split(data.s,data.s$CaptureUnit), paste0(sort(unique(data.s$CaptureUnit))))
sepl.LIST <- setNames(split(data.s,data.s$sePL), paste0(sort(unique(data.s$sePL))))


#### Demonstrating trends of area and daily distance

## Loading required packages
list.of.packages <- c("ggpubr", "ggeffects", "jtools","interactions","Hmisc","lattice","rms","akima","rgl","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(c("ggpubr", "ggeffects", "jtools","interactions","Hmisc","lattice","rms","akima","rgl","plotly"), require, character.only = TRUE)

## Running model
AREAmodel <- lm(logdist~ns(logarea, df = 3), data = data.s)
AREApred <- data.frame(ggpredict(AREAmodel, term = c("logarea[all]]"), type = "fe"))

# Plot #1
ggplot(data=AREApred, aes(x=x, y=predicted)) +
  geom_point(data = data.s, aes(x=logarea, y = logdist,alpha=0.1))+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), color = "gray", linetype="blank", alpha=0.2) +
  geom_line(aes(y = predicted), color = "black", size = .75,linetype="solid") +
  labs(x = "Log Home Range Area (ha)", y = "Log Distance Per Day (m)", title = "") + 
  theme(legend.position="none",legend.title = element_blank(), plot.title = element_text(family= "Times New Roman"), axis.title.x = element_text(family= "Times New Roman"), axis.title.y = element_text(family= "Times New Roman"),text = element_text(family = "Times New Roman"), axis.text = element_text(family = "Times New Roman"))

# Plot #2
ggplot(data=data.s, aes(x=logarea, y = logdist,alpha=0.1)) + scale_alpha(guide = 'none') +
  geom_point(data = data.s, aes(x=logarea, y = logdist,alpha=0.1, color = sePL))+ scale_color_manual("Home Range",values=c("#2E8B57","#1E90FF","#9ACD32","#00CDCD"), labels = c("North Slope Summer","North Slope Winter","Pine Valley Summer","Pine Valley Winter"))+ 
  labs(x = "Log HR area (ha)", y = "Log distance (m)", title = "") + 
  theme(legend.position="right", plot.title = element_text(family= "Times New Roman"), legend.text = element_text(family= "Times New Roman"),legend.title = element_text(family= "Times New Roman"),
        axis.title.x = element_text(family= "Times New Roman"), axis.title.y = element_text(family= "Times New Roman"),axis.text.x = element_text(family= "Times New Roman"),
        axis.text.y = element_text(family= "Times New Roman"))

#### Running Linear Mixed Models for each combo of season x capture unit

## North Slope Summer
NS.model.combo.REML <- lmer(logdist ~ ns(area_scale,df = 3) + area_scale:spat_scale + area_scale:temp_scale + NDVI_scale:spat_scale + NDVI_scale:temp_scale + spat_scale*temp_scale + NDVI_scale + (1|animal) + (1|year), data = sepl.LIST[[1]], REML=TRUE)
AIC(NS.model.combo.REML)
summary(NS.model.combo.REML)
plot(NS.model.combo.REML)
hist(summary(NS.model.combo.REML)$residuals)
plot(sepl.LIST[[1]]$logdist,predict(NS.model.combo.REML), col = COL[1], xlab = "Observed", ylab = "Predicted", main = "North Slope Summer")
abline(a = 0, b=1)

## North Slope Winter
NW.model.combo.REML <- lmer(logdist ~ ns(area_scale, df = 3) + area_scale:spat_scale + area_scale:temp_scale + NDVI_scale:spat_scale + NDVI_scale:temp_scale + spat_scale*temp_scale + NDVI_scale + (1|animal) + (1|year), data = sepl.LIST[[2]], REML=TRUE)
AIC(NW.model.combo.REML)
summary(NW.model.combo.REML)
plot(NW.model.combo.REML)
hist(summary(NW.model.combo.REML)$residuals)
plot(sepl.LIST[[2]]$logdist,predict(NW.model.combo.REML), col = COL[2], xlab = "Observed", ylab = "Predicted", main = "North Slope Winter")
abline(a = 0, b=1)

## Pine Valley Summer
PS.model.combo.REML <- lmer(logdist ~ ns(area_scale, df = 3) + area_scale:spat_scale + area_scale:temp_scale + NDVI_scale:spat_scale + NDVI_scale:temp_scale + spat_scale*temp_scale + NDVI_scale + (1|animal) + (1|year), data = sepl.LIST[[3]], REML=TRUE)
AIC(PS.model.combo.REML)
summary(PS.model.combo.REML)
plot(PS.model.combo.REML)
hist(summary(PS.model.combo.REML)$residuals)
plot(sepl.LIST[[3]]$logdist,predict(PS.model.combo.REML), col = COL[3], xlab = "Observed", ylab = "Predicted", main = "Pine Valley Summer")
abline(a = 0, b=1)

## Pine Valley Winter
PW.model.combo.REML <- lmer(logdist ~ ns(area_scale,df = 3) + area_scale:spat_scale + area_scale:temp_scale + NDVI_scale:spat_scale + NDVI_scale:temp_scale + spat_scale*temp_scale + NDVI_scale + (1|animal) + (1|year), data = sepl.LIST[[4]], REML=TRUE)
AIC(PW.model.combo.REML)
summary(PW.model.combo.REML)
plot(PW.model.combo.REML)
hist(summary(PW.model.combo.REML)$residuals)
plot(sepl.LIST[[4]]$logdist,predict(PW.model.combo.REML), col = COL[4], xlab = "Observed", ylab = "Predicted", main = "Pine Valley Winter")
abline(a = 0, b=1)


## Creating list of all models for later use
mod.list <- list(NS.model.combo.REML,NW.model.combo.REML,PS.model.combo.REML,PW.model.combo.REML)

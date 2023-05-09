library(ggplot2)
library(lme4)
library(lmerTest)
library(tidyr)
library(tidyverse)
library(here)

data<-read_csv(here("data_raw", "data_raw.csv"))

## Tidy data

data$tone <- ifelse(data$key_resp_4.keys=="d",1,0)
data <- data %>%
  separate(sound, c("item1", "item2"), sep="/") %>%
  separate(item2, c("item", "factor", "step"), sep = "_")
data<-data[!data$participant %in% c("NF01"),]
data$participant <- gsub("N","",data$participant)
data$step <- gsub(".wav", "", data$step)
data$step <- gsub("step", "", data$step)
data <- data[,-c(1:3)]

glimpse(data)

## Plot
align<-data[data$factor=="peak",]
shape<-data[data$factor=="shape",]

# Save the clean data
write_csv(align, here("data_tidy", "data_align.csv"))
write_csv(shape, here("data_tidy", "data_shape.csv"))

# Font
library('showtext')
font_add_google('Times New Roman', 'Times New Roman')
showtext_auto()

# Peak alignment_across speakers
align %>%
  ggplot() +
  aes(x = as.numeric(step), y = tone) +
  geom_hline(yintercept = 0.5,  color='grey50', size=0.7)+
  stat_summary(fun = mean, geom = "line", color = "brown2", size=2.2)+
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "black", width = .2) +
  #stat_summary(fun = mean_cl_normal,  geom = "ribbon", fill = "brown2", alpha=.1)+
  stat_summary(fun = mean, geom = "point", size = 4, fill='white', color = "brown2",
               stroke = 1.5, pch = 21) +
  coord_cartesian(ylim = c(0, 1))+
  theme_bw()+
  theme(#text=element_text(family="Times"),
    axis.text.x = element_text(size=13,color = "black", family="Times New Roman"), 
    axis.text.y = element_text(size=11,color = "black",family="Times New Roman"),
    plot.title = element_text(size=16, face = "bold", hjust=0.5,family="Times New Roman"), 
    axis.title.x = element_text(size=15,face="bold",family="Times New Roman"), 
    axis.title.y = element_text(size=15,face="bold",family="Times New Roman"),
    strip.text.x = element_text(size = 14, family="Times New Roman",color = "black"))+
  labs(x = "Steps", y = "H response (%)")

## Peak alignment by Item
align %>%
  ggplot() +
  aes(x = as.numeric(step), y = tone) +
  geom_hline(yintercept = 0.5,  color='grey50', size=0.7)+
  stat_summary(fun = mean, geom = "line", color = "brown2", size=2.2)+
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "black", width = .2) +
  #stat_summary(fun = mean_cl_normal,  geom = "ribbon", fill = "brown2", alpha=.1)+
  stat_summary(fun = mean, geom = "point", size = 4, fill='white', color = "brown2",
               stroke = 1.5, pch = 21) +
  coord_cartesian(ylim = c(0, 1))+
  theme_bw()+
  theme(#text=element_text(family="Times"),
    axis.text.x = element_text(size=13,color = "black", family="Times New Roman"), 
    axis.text.y = element_text(size=11,color = "black",family="Times New Roman"),
    plot.title = element_text(size=16, face = "bold", hjust=0.5,family="Times New Roman"), 
    axis.title.x = element_text(size=15,face="bold",family="Times New Roman"), 
    axis.title.y = element_text(size=15,face="bold",family="Times New Roman"),
    strip.text.x = element_text(size = 14, family="Times New Roman",color = "black"))+
  labs(x = "Steps", y = "H response (%)")+
  facet_grid(.~item)

## Peak alignment by Subject

align %>%
  ggplot() +
  aes(x = as.numeric(step), y = tone) +
  geom_hline(yintercept = 0.5,  color='grey50', size=0.7)+
  stat_summary(fun = mean, geom = "line", color = "brown2", size=2.2)+
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "black", width = .2) +
  #stat_summary(fun = mean_cl_normal,  geom = "ribbon", fill = "brown2", alpha=.1)+
  stat_summary(fun = mean, geom = "point", size = 4, fill='white', color = "brown2",
               stroke = 1.5, pch = 21) +
  coord_cartesian(ylim = c(0, 1))+
  theme_bw()+
  theme(#text=element_text(family="Times"),
    axis.text.x = element_text(size=11,color = "black", family="Times New Roman"), 
    axis.text.y = element_text(size=10,color = "black",family="Times New Roman"),
    plot.title = element_text(size=16, face = "bold", hjust=0.5,family="Times New Roman"), 
    axis.title.x = element_text(size=15,face="bold",family="Times New Roman"), 
    axis.title.y = element_text(size=15,face="bold",family="Times New Roman"),
    strip.text.x = element_text(size = 12, family="Times New Roman",color = "black"))+
  labs(x = "Steps", y = "H response (%)") + 
  facet_wrap(.~participant, nrow=3)

##################################

# Shape_across speakers

shape %>%
  ggplot() +
  aes(x = as.numeric(step), y = tone) +
  geom_vline(xintercept = 2, linetype="longdash", color='grey50', size=0.7)+
  geom_hline(yintercept = 0.5,  color='grey50', size=0.7)+
  stat_summary(fun = mean, geom = "line", color = "royalblue3", size=2.2)+
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "black", width = .2) +
  #stat_summary(fun = mean_cl_normal,  geom = "ribbon", fill = "brown2", alpha=.1)+
  stat_summary(fun = mean, geom = "point", size = 4, fill='white', color = "royalblue3",
               stroke = 1.5, pch = 21) +
  coord_cartesian(ylim = c(0, 1))+
  theme_bw()+
  theme(#text=element_text(family="Times"),
    axis.text.x = element_text(size=13,color = "black", family="Times New Roman"), 
    axis.text.y = element_text(size=11,color = "black",family="Times New Roman"),
    plot.title = element_text(size=16, face = "bold", hjust=0.5,family="Times New Roman"), 
    axis.title.x = element_text(size=15,face="bold",family="Times New Roman"), 
    axis.title.y = element_text(size=15,face="bold",family="Times New Roman"),
    strip.text.x = element_text(size = 14, family="Times New Roman",color = "black"))+
  labs(x = "Steps", y = "H response (%)")

# Shape by Item
shape %>%
  ggplot() +
  aes(x = as.numeric(step), y = tone) +
  geom_vline(xintercept = 2, linetype="longdash", color='grey50', size=0.7)+
  geom_hline(yintercept = 0.5,  color='grey50', size=0.7)+
  stat_summary(fun = mean, geom = "line", color = "royalblue3", size=2.2)+
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "black", width = .2) +
  #stat_summary(fun = mean_cl_normal,  geom = "ribbon", fill = "brown2", alpha=.1)+
  stat_summary(fun = mean, geom = "point", size = 4, fill='white', color = "royalblue3",
               stroke = 1.5, pch = 21) +
  coord_cartesian(ylim = c(0, 1))+
  theme_bw()+
  theme(#text=element_text(family="Times"),
    axis.text.x = element_text(size=13,color = "black", family="Times New Roman"), 
    axis.text.y = element_text(size=11,color = "black",family="Times New Roman"),
    plot.title = element_text(size=16, face = "bold", hjust=0.5,family="Times New Roman"), 
    axis.title.x = element_text(size=15,face="bold",family="Times New Roman"), 
    axis.title.y = element_text(size=15,face="bold",family="Times New Roman"),
    strip.text.x = element_text(size = 14, family="Times New Roman",color = "black"))+
  labs(x = "Steps", y = "H response (%)") +
  facet_grid(.~item)

# Shape by Subject

shape %>%
  ggplot() +
  aes(x = as.numeric(step), y = tone) +
  geom_vline(xintercept = 2, linetype="longdash", color='grey50', size=0.7)+
  geom_hline(yintercept = 0.5,  color='grey50', size=0.7)+
  stat_summary(fun = mean, geom = "line", color = "royalblue3", size=2.2)+
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "black", width = .2) +
  #stat_summary(fun = mean_cl_normal,  geom = "ribbon", fill = "brown2", alpha=.1)+
  stat_summary(fun = mean, geom = "point", size = 3.5, fill='white', color = "royalblue3",
               stroke = 1.5, pch = 21) +
  coord_cartesian(ylim = c(0, 1))+
  theme_bw()+
  theme(#text=element_text(family="Times"),
    axis.text.x = element_text(size=11,color = "black", family="Times New Roman"), 
    axis.text.y = element_text(size=10,color = "black",family="Times New Roman"),
    plot.title = element_text(size=16, face = "bold", hjust=0.5,family="Times New Roman"), 
    axis.title.x = element_text(size=15,face="bold",family="Times New Roman"), 
    axis.title.y = element_text(size=15,face="bold",family="Times New Roman"),
    strip.text.x = element_text(size = 12, family="Times New Roman",color = "black"))+
  labs(x = "Steps", y = "H response (%)") + 
  facet_wrap(.~participant, nrow=3)

df <- data[data$factor=="shape"|data$factor=="peak",]
aa <- df %>%
  group_by(factor, item, step) %>%
  summarize(avg = mean(tone),
            sd = sd(tone),
            min = min(tone),
            max = max(tone))

print(aa, n =30)

## Stat_01
df_wide <- df %>%
  pivot_wider(names_from = factor, 
              values_from = step)
df_wide$peak <- as.numeric(df_wide$peak)
df_wide$shape <- as.numeric(df_wide$shape)
align_wide<-data[data$factor=="peak",]

df_wide_align <- align %>%
  pivot_wider(names_from = factor, 
              values_from = step)
df_wide_shape <- shape %>%
  pivot_wider(names_from = factor, 
              values_from = step)

df_wide_align$peak <- as.numeric(df_wide_align$peak)

df_wide_shape$shape <- as.numeric(df_wide_shape$shape)
mod_pk_1 <- glmer(tone ~ peak + (1+peak|participant) + (1+peak|item), family = "binomial", df_wide_align)

pk_stat<-summary(mod_pk_1)
pk_stat_trim<-round(pk_stat$coefficients,3)
pk_stat_add<-rbind(colnames(pk_stat_trim),pk_stat_trim)
pk_stat_com<-rbind(pk_stat$call, pk_stat_add)


mod_sp_1 <- glmer(tone ~ shape + (1+shape|participant)+ (1+shape|item), family = "binomial", df_wide_shape)
mod_sp_2 <- glmer(tone ~ shape + (1+shape|participant)+ (1|item), family = "binomial", df_wide_shape)

sp_stat_2<-summary(mod_sp_2)
sp_stat_2_trim<-round(sp_stat_2$coefficients,3)
sp_stat_2_add<-rbind(colnames(sp_stat_2_trim),sp_stat_2_trim)
sp_stat_2_com<-rbind(sp_stat_2$call, sp_stat_2_add)

stat_all <- rbind(pk_stat_com, sp_stat_2_com)

write.csv(pk_stat_com,"stat_peak.csv",row.names=T)
write.csv(sp_stat_2_com,"stat_shape.csv",row.names=T)

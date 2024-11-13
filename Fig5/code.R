rm(list = ls())

library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(readxl)

colorgroup <- c("#3a5985" ,"#89afe0","#b2caee")

public_themes  <-    
  theme_classic()+
  theme(
    axis.title = element_text(size = 15, face = "bold", color = "black"),
    axis.text = element_text(size = 15, face = "bold", color = "black"),
    axis.ticks.length = unit(.2 ,"cm"),
    axis.ticks = element_line(size = 1),
    axis.line = element_line(size = 1),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    plot.title = element_text(size = 18, face = "bold", color = "black"),
  )

dcf_data <- read_excel("data.xlsx")

head(dcf_data)

dcf_data$Xlabel <- factor(x = c("Precision","False positive rate","False negative rate"), 
                          levels = c("Precision","False positive rate","False negative rate"),
                          labels = c("P","FPR","FNR"))

p_human <- ggplot(dcf_data, aes(x = Xlabel, y = Accuracy_human)) + 
  geom_bar(stat= "identity", fill = colorgroup)+
  geom_errorbar(aes(ymin = Accuracy_human, ymax = ymaxerrorbar_human),
                stat = "identity",
                width = 0,    
                size = 1) +  
  geom_errorbar(aes(ymin = ymaxerrorbar_human, ymax = ymaxerrorbar_human),
                stat = "identity",
                width = 0.28, 
                size = 1)+
  labs(title = "a",x = "Human",y="")+
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(expand = c(0,0),breaks= seq(0, 1,by= 0.25), )+
  public_themes


p_machine_75 <- ggplot(dcf_data, aes(x = Xlabel, y = Accuracy_machine_75)) + 
  geom_bar(stat= "identity", fill = colorgroup)+
  geom_errorbar(aes(ymin = Accuracy_machine_75, ymax = ymaxerrorbar_machine_75),
                stat = "identity",
                width = 0,    
                size = 1) +  
  geom_errorbar(aes(ymin = ymaxerrorbar_machine_75, ymax = ymaxerrorbar_machine_75),
                stat = "identity",
                width = 0.28, 
                size = 1)+
  labs(title = "b",x = "HTPIS 75%",y="")+
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(expand = c(0,0),breaks= seq(0, 1,by= 0.25), )+
  public_themes

p_machine_80 <- ggplot(dcf_data, aes(x = Xlabel, y = Accuracy_machine_80)) + 
  geom_bar(stat= "identity", fill = colorgroup)+
  geom_errorbar(aes(ymin = Accuracy_machine_80, ymax = ymaxerrorbar_machine_80),
                stat = "identity",
                width = 0,    
                size = 1) +  
  geom_errorbar(aes(ymin = ymaxerrorbar_machine_80, ymax = ymaxerrorbar_machine_80),
                stat = "identity",
                width = 0.28, 
                size = 1)+
  labs(title = "c",x = "HTPIS 80%",y="")+
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(expand = c(0,0),breaks= seq(0, 1,by= 0.25), )+
  public_themes

p_machine_85 <- ggplot(dcf_data, aes(x = Xlabel, y = Accuracy_machine_85)) + 
  geom_bar(stat= "identity", fill = colorgroup)+
  geom_errorbar(aes(ymin = Accuracy_machine_85, ymax = ymaxerrorbar_machine_85),
                stat = "identity",
                width = 0,    
                size = 1) +  
  geom_errorbar(aes(ymin = ymaxerrorbar_machine_85, ymax = ymaxerrorbar_machine_85),
                stat = "identity",
                width = 0.28, 
                size = 1)+
  labs(title = "d",x = "HTPIS 85%",y="")+
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(expand = c(0,0),breaks= seq(0, 1,by= 0.25), )+
  public_themes

p_machine_90 <- ggplot(dcf_data, aes(x = Xlabel, y = Accuracy_machine_90)) + 
  geom_bar(stat= "identity", fill = colorgroup)+
  geom_errorbar(aes(ymin = Accuracy_machine_90, ymax = ymaxerrorbar_machine_90),
                stat = "identity",
                width = 0,    
                size = 1) +  
  geom_errorbar(aes(ymin = ymaxerrorbar_machine_90, ymax = ymaxerrorbar_machine_90),
                stat = "identity",
                width = 0.28, 
                size = 1)+
  labs(title = "e",x = "HTPIS 90%",y="")+
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(expand = c(0,0),breaks= seq(0, 1,by= 0.25), )+
  public_themes

p_machine_95 <- ggplot(dcf_data, aes(x = Xlabel, y = Accuracy_machine_95)) + 
  geom_bar(stat= "identity", fill = colorgroup)+
  geom_errorbar(aes(ymin = Accuracy_machine_95, ymax = ymaxerrorbar_machine_95),
                stat = "identity",
                width = 0,    
                size = 1) +  
  geom_errorbar(aes(ymin = ymaxerrorbar_machine_95, ymax = ymaxerrorbar_machine_95),
                stat = "identity",
                width = 0.28, 
                size = 1)+
  labs(title = "f",x = "HTPIS 95%",y="")+
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(expand = c(0,0),breaks= seq(0, 1,by= 0.25), )+
  public_themes

p_machine_99 <- ggplot(dcf_data, aes(x = Xlabel, y = Accuracy_machine_99)) + 
  geom_bar(stat= "identity", fill = colorgroup)+
  geom_errorbar(aes(ymin = Accuracy_machine_99, ymax = ymaxerrorbar_machine_99),
                stat = "identity",
                width = 0,    
                size = 1) +  
  geom_errorbar(aes(ymin = ymaxerrorbar_machine_99, ymax = ymaxerrorbar_machine_99),
                stat = "identity",
                width = 0.28, 
                size = 1)+
  labs(title = "g",x = "HTPIS 99%",y="")+
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(expand = c(0,0),breaks= seq(0, 1,by= 0.25), )+
  public_themes

p_machine_999 <- ggplot(dcf_data, aes(x = Xlabel, y = Accuracy_machine_999)) + 
  geom_bar(stat= "identity", fill = colorgroup)+
  geom_errorbar(aes(ymin = Accuracy_machine_999, ymax = ymaxerrorbar_machine_999),
                stat = "identity",
                width = 0,    
                size = 1) +  
  geom_errorbar(aes(ymin = ymaxerrorbar_machine_999, ymax = ymaxerrorbar_machine_999),
                stat = "identity",
                width = 0.28, 
                size = 1)+
  labs(title = "h",x = "HTPIS 99.9%",y="")+
  coord_cartesian(ylim = c(0, 1.1)) +
  scale_y_continuous(expand = c(0,0),breaks= seq(0, 1,by= 0.25), )+
  public_themes

p_human + p_machine_75 + p_machine_80 + p_machine_85 + p_machine_90 + p_machine_95 + p_machine_99 + p_machine_999

library(tidyverse)

#Load in apple trait and ploidy
data_ploidy <- read_csv("data/TableS1.csv")

#Select only the trait of interest, get rid of NAs, and then table the results to see what number of diploids/triploids remain 

#Acidity
acid_data <- data_ploidy %>% select(apple_id, PLANTID, ploidy, acidity_17_harv)  %>% filter(acidity_17_harv != "NA") %>% mutate(acidity_17_harv=as.numeric(acidity_17_harv))
#table
table(acid_data$ploidy)
#diploid triploid 
# 624       94 
acid_data_diploid <- acid_data %>% filter(ploidy=="2x")
acid_data_triploid <- acid_data %>% filter(ploidy=="3x")

wilcox.test(acid_data_diploid$acidity_17_harv,acid_data_triploid$acidity_17_harv)
#W = 28129, p-value = 0.5226

#SSC
brix_data <- data_ploidy %>% select(apple_id, PLANTID, ploidy, brix_17_harv)  %>% filter(brix_17_harv != "NA") %>% mutate(brix_17_harv=as.numeric(brix_17_harv))
table(brix_data$ploidy)
#diploid triploid
#641       99 
brix_data_diploid <- brix_data %>% filter(ploidy=="2x")
brix_data_triploid <- brix_data %>% filter(ploidy=="3x")

wilcox.test(brix_data_diploid$brix_17_harv,brix_data_triploid$brix_17_harv)
#W = 28808, p-value = 0.1401

#SSC/Acidity
brix_acid_data <- data_ploidy %>% select(apple_id, PLANTID, ploidy, brix_acid_17_harv)  %>% filter(brix_acid_17_harv != "NA") %>% mutate(brix_acid_17_harv=as.numeric(brix_acid_17_harv))
table(brix_acid_data$ploidy)
#diploid triploid
# 624       94
brix_acid_data_diploid <- brix_acid_data %>% filter(ploidy=="2x")
brix_acid_data_triploid <- brix_acid_data %>% filter(ploidy=="3x")

wilcox.test(brix_acid_data_diploid$brix_acid_17_harv,brix_acid_data_triploid$brix_acid_17_harv)
#W = 30416, p-value = 0.5619


#Average Weight
weight_data <- data_ploidy %>% select(apple_id, PLANTID, ploidy, weight_avg_17_harv)  %>% filter(weight_avg_17_harv != "NA") %>% mutate(weight_avg_17_harv=as.numeric(weight_avg_17_harv))
table(weight_data$ploidy)
#diploid triploid
#642      101 
weight_data_diploid <- weight_data %>% filter(ploidy=="2x")
weight_data_triploid <- weight_data %>% filter(ploidy=="3x")

wilcox.test(weight_data_diploid$weight_avg_17_harv,weight_data_triploid$weight_avg_17_harv)
#W = 27306, p-value = 0.01075

#Firmness
firmness_data <- data_ploidy %>% select(apple_id, PLANTID, ploidy, firmness_avg_17_harv)  %>% filter(firmness_avg_17_harv != "NA") %>% mutate(firmness_avg_17_harv=as.numeric(firmness_avg_17_harv))
table(firmness_data$ploidy)
#diploid triploid
#642      101
firmness_data_diploid <- firmness_data %>% filter(ploidy=="2x")
firmness_data_triploid <- firmness_data %>% filter(ploidy=="3x")

wilcox.test(firmness_data_diploid$firmness_avg_17_harv,firmness_data_triploid$firmness_avg_17_harv)
#W = 28410, p-value = 0.04548


#%Firmness
per_firm_data <- data_ploidy %>% select(apple_id, PLANTID, ploidy, percent_firmness_avg_17)  %>% filter(percent_firmness_avg_17 != "NA") %>% mutate(percent_firmness_avg_17=as.numeric(percent_firmness_avg_17))
table(per_firm_data$ploidy)
#diploid triploid
#415      61
per_firm_data_diploid <- per_firm_data %>% filter(ploidy=="2x")
per_firm_data_triploid <- per_firm_data %>% filter(ploidy=="3x")

wilcox.test(per_firm_data_diploid$percent_firmness_avg_17,per_firm_data_triploid$percent_firmness_avg_17)
#W = 13185, p-value = 0.5993

#Phenolic Content
tpc_data <- data_ploidy %>% select(apple_id, PLANTID, ploidy, tpc)  %>% filter(tpc != "NA") %>% mutate(tpc=as.numeric(tpc))
table(tpc_data$ploidy)
#diploid triploid
#380       47
tpc_diploid <- tpc_data %>% filter(ploidy=="2x")
tpc_triploid <- tpc_data %>% filter(ploidy=="3x")

wilcox.test(tpc_diploid$tpc,tpc_triploid$tpc)
#W = 6467, p-value = 0.002033

#Flowering time
flowering_time_data <- data_ploidy %>% select(apple_id, PLANTID, ploidy, flowering_jul_16_harv)  %>% filter(flowering_jul_16_harv != "NA") %>% mutate(flowering_jul_16_harv=as.numeric(flowering_jul_16_harv))
table(flowering_time_data$ploidy)
#diploid triploid
#793      135
flowering_data_diploid <- flowering_time_data %>% filter(ploidy=="2x")
flowering_data_triploid <- flowering_time_data %>% filter(ploidy=="3x")

wilcox.test(flowering_data_diploid$flowering_jul_16_harv,flowering_data_triploid$flowering_jul_16_harv)
#W = 48776, p-value = 0.09884

#Ripen
ripen_data <- data_ploidy %>% select(apple_id, PLANTID, ploidy, time_ripen_2017) %>% filter(time_ripen_2017 != "NA") %>% mutate(time_ripen_2017=as.numeric(time_ripen_2017))
table(ripen_data$ploidy)
#diploid triploid
# 623       94
ripen_data_diploid <- ripen_data %>% filter(ploidy=="2x")
ripen_data_triploid <- ripen_data %>% filter(ploidy=="3x")

wilcox.test(ripen_data_diploid$time_ripen_2017,ripen_data_triploid$time_ripen_2017)
#W = 26152, p-value = 0.09471

#Harvest
harvest_data <- data_ploidy %>% select(apple_id, PLANTID, ploidy, date_jul_17_harv)  %>% filter(date_jul_17_harv != "NA") %>% mutate(date_jul_17_harv=as.numeric(date_jul_17_harv))
table(harvest_data$ploidy)
#diploid triploid
# 645      101
harvest_data_diploid <- harvest_data %>% filter(ploidy=="2x")
harvest_data_triploid <- harvest_data %>% filter(ploidy=="3x")

wilcox.test(harvest_data_diploid$date_jul_17_harv,harvest_data_triploid$date_jul_17_harv)
#W = 28310, p-value = 0.03431


#Boxplots for the traits used
b1 = data_ploidy %>% ggplot(aes(x=as.factor(ploidy), y=as.numeric(acidity_17_harv), fill=ploidy))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=9,face="bold"))+
  xlab("Ploidy")+ylab("Acidity (g/L)")+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  theme(legend.position = "none")


b2 = data_ploidy %>% ggplot(aes(x=as.factor(ploidy), y=as.numeric(brix_17_harv), fill=ploidy))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=9,face="bold"))+
  xlab("Ploidy")+ylab("SSC (Brix)")+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  theme(legend.position = "none")

b3 = data_ploidy %>% ggplot(aes(x=as.factor(ploidy), y=as.numeric(brix_acid_17_harv), fill=ploidy))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=9,face="bold"))+
  xlab("Ploidy")+ylab("SSC/Acidity (Brix/g/L)")+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  theme(legend.position = "none")

b4 = data_ploidy %>% ggplot(aes(x=as.factor(ploidy), y=as.numeric(weight_avg_17_harv), fill=ploidy))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=9,face="bold"))+
  xlab("Ploidy")+ylab("Weight (g)")+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  theme(legend.position = "none")

b5 = data_ploidy %>% ggplot(aes(x=as.factor(ploidy), y=as.numeric(firmness_avg_17_harv), fill=ploidy))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=9,face="bold"))+
  xlab("Ploidy")+ylab("Firmness (kg/cm2)")+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  theme(legend.position = "none")

b6 = data_ploidy %>% ggplot(aes(x=as.factor(ploidy), y=as.numeric(percent_firmness_avg_17), fill=ploidy))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=9,face="bold"))+
  xlab("Ploidy")+ylab("%Firmness")+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  theme(legend.position = "none")

b7 = data_ploidy %>% ggplot(aes(x=as.factor(ploidy), y=as.numeric(tpc), fill=ploidy))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=9,face="bold"))+
  xlab("Ploidy")+ylab("Phenolic Content (umol/g)")+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  theme(legend.position = "none")

b8 = data_ploidy %>% ggplot(aes(x=as.factor(ploidy), y=as.numeric(flowering_jul_16_harv), fill=ploidy))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=9,face="bold"))+
  xlab("Ploidy")+ylab("Flowering (Julian date)")+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  theme(legend.position = "none")

b9 = data_ploidy %>% ggplot(aes(x=as.factor(ploidy), y=as.numeric(date_jul_17_harv), fill=ploidy))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=9,face="bold"))+
  xlab("Ploidy")+ylab("Harvest Date (Julian date)")+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  theme(legend.position = "none")

b10 = data_ploidy %>% ggplot(aes(x=as.factor(ploidy), y=as.numeric(time_ripen_2017), fill=ploidy))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=9,face="bold"))+
  xlab("Ploidy")+ylab("Time to Ripen (days)")+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  theme(legend.position = "none")


require(cowplot)
pdf("figures/Figure3.pdf", width=8, height=10)
plot_grid(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10, align='h', ncol=2, nrow = 5, labels="auto")
dev.off()

library(tidyverse)
library(readxl)

#Get ACCIDs for the ABC from Watts et all 2023 (Supplemental dataset 1) (https://www.maxapress.com/article/doi/10.48130/FruRes-2023-0032) in order to be able to link with USDA data 

supp_data <- read_excel("./data/Dataset_S1.xlsx",  sheet = "phenotype_data")

apple_ids_usda <- supp_data %>% select(apple_id, PLANTID, ACCID)

#Load in heterozygosity data (calculated using PLINK)

het_tab <- read_table("./data/apple_ploidy_heterozygosity.txt")
colnames(het_tab)=c("apple_id", "ID", "ob_hom", "ex_hom", "markers", "f")
het_tab <- het_tab %>% select(-ID, -ex_hom, -f)

het_info_tab <- het_tab %>% left_join(apple_ids_usda)

#Link the data with the USDA flow cytometry data previously published by Migicovsky et al. 2021 (https://www.nature.com/articles/s41438-020-00441-7) for USDA. 

#USDA information with 

usda_data <-  read.csv(file = "data/usda_ploidy_info.csv")

#Reduce down to relevant information

usda_data <- usda_data %>% select(FullSampleName, cultivar, ACCID, ploidy)

#Make sure ACCID is a character for both datasets
het_info_tab$ACCID <- as.character(het_info_tab$ACCID)
usda_data$ACCID <- as.character(usda_data$ACCID)

#match up with the samples I actually have and only keep those, linking it with the ABC data
samples_ploidy <- het_info_tab %>% left_join(usda_data)

#Any samples sequenced more than once will appear in duplicate here (ie: 222 = Golden Delicious appears 8 times) so we only keep distinct ones 
samples_ploidy <- samples_ploidy %>% select(-FullSampleName) %>% distinct()

table(samples_ploidy[,"ploidy"])
# 2x 2x/4x    3x    4x 
#141     1    46     5 

het_info_usda <- mutate(samples_ploidy, het=(markers-ob_hom)/markers)

het_info_usda %>% group_by(ploidy) %>%
  summarize(Mean = mean(het, na.rm=TRUE))
# PLOIDY  Mean
# <chr>  <dbl>
#   1 2x     0.355
# 2 2x/4x  0.405
# 3 3x     0.471
# 4 4x     0.389
# 5 NA     0.367

#Histogram of heterozgosity
ploidy_hist <- het_info_usda %>% ggplot(aes(x=het))+
  geom_histogram(bins=100)+
  theme(legend.position = "none")+
  labs(x="Heterozygosity by individual", y="Count")+
  theme_classic()+
  geom_vline(xintercept=0.355, colour="#56B4E9")+
  geom_vline(xintercept=0.471, colour="#E69F00")+
  geom_vline(xintercept=0.425, colour="#999999")+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

#Boxplot of heterozgosity by ploidy 
ploidy_boxplot <- het_info_usda %>% filter(ploidy=="2x" |ploidy=="3x") %>%
  ggplot(aes(x=ploidy, y=het,fill=ploidy))+
  geom_boxplot()+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  xlab("Ploidy")+ylab("Heterozygosity by individual")+
  theme_classic()+
  geom_hline(yintercept=0.425, colour="#999999")+ 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")

#Save plot
require(cowplot)
pdf("figures/Figure1.pdf", width=8, height=4)
plot_grid(ploidy_hist, ploidy_boxplot, align='h', ncol=2, labels="auto")
dev.off()

#Based on this threshold, how many triploids are incorrectly identified as diploids? How many diploids are incorrectly identified as triploids? 

known_ploidy <- het_info_usda %>% filter(ploidy=="2x" |ploidy=="3x")
nrow(known_ploidy)
#187 of known ploidy

#Filter down to triploids and look at labels

trip <- known_ploidy %>% filter(het >=0.425)
table(trip$ploidy)
#2x 3x 
#7 41 

#We label 7 known diploids as triploids

dip <- known_ploidy %>% filter(het <0.425)
table(dip$ploidy)
#2x  3x 
#134   5 

#We label 5 known triploids as diploids.

#12 errors out of 188
(12/187)*100
#6.417112

#Wilcoxon test comparing
usda_dip <- het_info_usda %>% filter(ploidy=="2x")
usda_trip <- het_info_usda %>% filter(ploidy=="3x")

wilcox.test(usda_dip$het, usda_trip$het)
#W = 369, p-value < 2.2e-16

#Based on this threshold, how many samples are diploids and how many are triploids? 

trips <- het_info_usda %>% filter(het >=0.425)
dips <- het_info_usda %>% filter(het <0.425)

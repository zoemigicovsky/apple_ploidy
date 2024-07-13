library(tidyverse)

#Load in apple ploidy labels
apple_ploidy <- read_csv("data/abc_apple_ploidy.csv")

#Add data to Watts et al table Dataset_S1.xlsx
supp_data <- read_excel("data/Dataset_S1.xlsx",  sheet = "phenotype_data")

#Only keep rows that occur in both tables
data_ploidy <- supp_data %>% inner_join(apple_ploidy)

#Information about the release year of the accessions
release_year_data <- data_ploidy %>% select(apple_id, PLANTID, ploidy, release_year)  %>% filter(release_year != "NA") %>% mutate(release_year=as.numeric(release_year))
table(release_year_data$ploidy)
#diploid triploid
#299       32 
#There are 299+32 (331) with both ploidy and release year data
release_year_data_diploid <- release_year_data %>% filter(ploidy=="2x")
release_year_data_triploid <- release_year_data %>% filter(ploidy=="3x")

wilcox.test(release_year_data_diploid$release_year,release_year_data_triploid$release_year)
#W = 5800, p-value = 0.04839

#Density Plot
fig4 <- release_year_data %>% ggplot(aes(x=as.numeric(release_year), fill=ploidy))+
  geom_density(alpha=0.4)+
  theme_classic()+
  theme(axis.text=element_text(size=11),axis.title=element_text(size=12,face="bold"))+
  ylab("Density")+xlab("Release Year")+
  scale_fill_manual(values=c( "#56B4E9", "#E69F00"))+
  theme(legend.position = "none")

require(cowplot)
pdf("figures/Figure4.pdf", width=8, height=4)
fig4
dev.off()

#Mean and median
#Diploids
mean(release_year_data_diploid$release_year, na.rm=T)
#1947
median(release_year_data_diploid$release_year, na.rm=T)
#1957

#Triploids
mean(release_year_data_triploid$release_year, na.rm=T)
#1919
median(release_year_data_triploid$release_year, na.rm=T)
#1946

#Number of accessions before 1900s

#Diploids
sum(release_year_data_diploid$release_year<1900, na.rm=T)
#34 out of the 299

34/299
#0.1137124

#Triploids
sum(release_year_data_triploid$release_year<1900, na.rm=T)
#10 out of the 32

10/32
#0.3125

0.3125/0.1137124
#for triploids, 2.748161 times more likely to be from before 1900 

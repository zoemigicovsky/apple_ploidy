library(tidyverse)

#Load in apple ploidy labels
apple_ploidy <- read_csv("data/abc_apple_ploidy.csv")

#Run genomic PCA and label diploids and triploids. PCA was run using tassel.

genomic_pcs <- read_table("data/ploidy_pcs.txt")
colnames(genomic_pcs)[1] <- "apple_id"

#Add in ploidy labels 

genomic_pcs_ploidy <- genomic_pcs %>% inner_join(apple_ploidy)

#Add in % Variance explained

eigenvalue <- read_delim("data/ploidy_pcs_eigenvalue.txt")

#Analysis here is written in Java, which uses 0-based arrays instead of 1 based arrays, so 0 is the first PC.

pc1_var <- eigenvalue[1,"proportion of total"]
pc1_var <- round(pc1_var*100, digits=2)
pc2_var <- eigenvalue[2,"proportion of total"]
pc2_var <- round(pc2_var*100, digits=2)

p <- ggplot(genomic_pcs_ploidy, aes(x=PC1, y=PC2))
figure2a <- p + geom_point(data = subset(genomic_pcs_ploidy, ploidy=="2x"),color="#56B4E9", size=3, alpha=0.9, stroke=0)+ 
  geom_point(data = subset(genomic_pcs_ploidy, ploidy=="3x"),color="#E69F00", size=3, alpha=0.9, stroke=0)+ 
  theme_classic()+
  labs(x=paste("PC1 (", pc1_var, "%)", sep=""), y=paste("PC2 (", pc2_var, "%)", sep=""))

#IBS comparison

#Load in similarity file, this was generated using PLINK

ibs_mat <-  read.table("data/ploidy_ibs.mibs")
#Get sample names from het table
het <- read.table("data/apple_ploidy_heterozygosity.txt", header=T)
sample_names <- het[,1]
colnames(ibs_mat) <- sample_names
rownames(ibs_mat) <- sample_names

#Convert from a matrix into a table with all pairwise comparisons 
xy <- t(combn(colnames(ibs_mat), 2))
ibs_tab <- data.frame(xy, dist=ibs_mat[xy])

#Get list of triploids and list of diploids 
diploids <- apple_ploidy %>% filter(ploidy=="2x")
diploids <- diploids$apple_id
triploids <- apple_ploidy %>% filter(ploidy=="3x")
triploids <- triploids$apple_id

#Add ploidy into similiarity table 
ibs_tab <- ibs_tab %>% mutate(ploidy_1=ifelse(X1 %in% triploids, "3x", "2x")) %>% mutate(ploidy_2=ifelse(X2 %in% triploids, "3x", "2x")) %>% mutate(ploidy_comp=ifelse(ploidy_1  =="3x" & ploidy_2 =="3x", "trip-trip", ifelse(ploidy_1  =="2x" & ploidy_2  =="2x", "dip-dip","trip-dip")))

dip_dip<- ibs_tab %>% filter(ploidy_comp=="dip-dip")
trip_trip<- ibs_tab %>% filter(ploidy_comp=="trip-trip")
trip_dip <- ibs_tab %>% filter(ploidy_comp=="trip-dip")

wilcox.test(trip_dip$dist,trip_trip$dist)
#W = 112369979, p-value < 2.2e-16

wilcox.test(dip_dip$dist,trip_trip$dist)
#W = 172679636, p-value < 2.2e-16

figure2b <- ibs_tab %>% ggplot(aes(x=as.numeric(dist), fill=ploidy_comp))+
  geom_density(alpha=0.4)+
  theme_classic()+
  labs(x ="IBS values for pairwise comparisons", y = "Density")+ 
  scale_fill_manual(values=c( "#56B4E9",  "#999999","#E69F00"))+
  theme(legend.position = "none")

#Save plot
require(cowplot)
pdf("figures/Figure2.pdf", width=8, height=4)
plot_grid(figure2a, figure2b, align='h', ncol=2, labels="auto")
dev.off()

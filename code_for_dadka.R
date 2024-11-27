##############################DENDROGRAM#######################################


setwd("E:/PhD štúdium/články/profiliny/profilins_code")

#substitute(paste(italic('Scatterplot of'), ' x vs. y')))
SM<- read.csv("SPCF_MAT.tsv", sep = "\t" , header = F, row.names = 1)
DM<- read.csv("DGNR_MAT.tsv", sep = "\t" , header = F, row.names = 1)
rownames(DM) <-c("P. crispum", "C. annuum", 
                 "D. carota", "S. tuberosum", 
                 "A. graveolens", "B. oleracea var. capitata f. alba", 
                 "L. sativa", "B. oleracea var. italica", 
                 "A. cepa", "B. oleracea var. capitata f. rubra", 
                 "C. maxima ssp. Maxima",  
                 "I. batatas", "B. oleracea var. gongylodes","B. vulgaris subsp. vulgaris")
rownames(SM) <-c("P. crispum", "C. annuum", 
                 "D. carota", "S. tuberosum", 
                 "A. graveolens", "B. oleracea var. capitata f. alba", 
                 "L. sativa", "B. oleracea var. italica", 
                 "A. cepa", "B. oleracea var. capitata f. rubra", 
                 "C. maxima ssp. Maxima", "B. oleracea var. gongylodes", 
                 "I. batatas", "B. vulgaris subsp. vulgaris")


#Vypocet distancnej matice
dist_SM <- dist(SM, method = "binary", diag = TRUE)
dist_DM <- dist(DM, method = "binary", diag = TRUE)

#klastrovanie 
Dhc <- hclust(dist_DM, method =  "average")
Shc <- hclust(dist_SM, method =  "average")

#Vypocet kofenetickej korelacie
dd <- cophenetic(Dhc)
cor(dist_DM, dd) #CF = 0.7978952
ss <- cophenetic(Shc)
cor(dist_SM, ss) #CF = 0.9348385

#kreslenie
require(graphics)

par(mfrow = c(1, 2))
plot(as.dendrogram(Dhc),horiz=T, hang = -1,main = "Degenerate primers")
plot(as.dendrogram(Shc),horiz=T, hang = -1,main = "Non-degenerate primers")
mtext('CP = 0.80',  side=1, line=-1, at=1.5)
mtext('CP = 0.93', side=1, line=-1, at=-1)





# skuska hrame sa s kniznicou DENDEXTEND

#install.packages("magrittr")
#install.packages("dendextend")
library("magrittr")
library("dendextend")

Ddendro <- as.dendrogram(Dhc)
Sdendro <- as.dendrogram(Shc)

dl <- dendlist(Ddendro, Sdendro)
tanglegram(dl, sort = TRUE, common_subtrees_color_lines = FALSE, highlight_distinct_edges  = FALSE, highlight_branches_lwd = FALSE)



#porovnanie roznych metod klastrovania

dend1 <- SM %>% dist(method = "binary") %>% hclust("com") %>% as.dendrogram
dend2 <- SM %>% dist(method = "binary") %>% hclust("single") %>% as.dendrogram
dend3 <- SM %>% dist(method = "binary") %>% hclust("ave") %>% as.dendrogram
dend4 <- SM %>% dist(method = "binary") %>% hclust("centroid") %>% as.dendrogram


dend1234 <- dendlist("Complete" = dend1, "Single" = dend2, "Average" = dend3, "Centroid" = dend4)
par(mfrow = c(2,2))
plot(dend1, main = "Complete")
plot(dend2, main = "Single")
plot(dend3, main = "Average")
plot(dend4, main = "Centroid")


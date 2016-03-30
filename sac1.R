library(igraph)
library('proxy')

# ===== Configuree alpha from command line argument ==== #

args <- commandArgs(trailingOnly = TRUE)
alpha=as.numeric(args[1])

# ===== Export data ==== #

graphPath <- "/home/achaluv/Documents/academics/BI/06.Topic-7.Project-6.MarketSegmentation.AttributedGraphCommunityDetection/data/fb_caltech_small_edgelist.txt"
attrPath <- "/home/achaluv/Documents/academics/BI/06.Topic-7.Project-6.MarketSegmentation.AttributedGraphCommunityDetection/data/fb_caltech_small_attrlist.csv"
g <- read.graph(file=graphPath, format = c("edgelist"))
attrData<- read.csv(attrPath)

# ===== Modularity and Cosine Similarity functions ==== #

modValue <- function(j, g, communityStructure, i){
  communityStructure[i] <- j
  modularity(g, communityStructure)
}

communityStructure <- 1:vcount(g)

cosineSim <- function(j, data, communityStructure, i){
  jComm = communityStructure[which(communityStructure == j)]
  jComm = apply(data[jComm,], 2, mean)            
  dist(rbind(data[i,], jComm), method="cosine")
}


iterationLimit <- 15

# ===== Phase 1 ==== #
for(k in 1:iterationLimit){
  print(k)
  print(length(unique(communityStructure)))
  noOfMods = 0
  for(i in 1:vcount(g)){
    oldMod<-modularity(g,membership = communityStructure)
    ineighbors<-neighbors(g,i)
    
    if(length(ineighbors)){
      modularvec = sapply(unique(communityStructure[ineighbors]), modValue , g, communityStructure, i)
      cosSim = sapply(unique(communityStructure[ineighbors]), cosineSim , attrData, communityStructure, i)
      deltaScore = alpha * (modularvec - oldMod) + ((1 - alpha) * cosSim)
      max <- max(deltaScore)
      if(max > 0){
        noOfMods = noOfMods + 1
        iComm <- unique(communityStructure[ineighbors])
        communityStructure[i] <- iComm[which.max(deltaScore)]
      }  
    }
  }
}


oldCommStructure=communityStructure
# ===== Phase 2 ==== #
for(h in 1:iterationLimit)
{
  g2 <- contract.vertices(g, communityStructure)
  g3 <- simplify(g2, remove.multiple = TRUE, remove.loops = TRUE)
  for(k in 1:iterationLimit){
    print(k)
    print(length(unique(communityStructure)))
    noOfMods = 0
    for(i in 1:vcount(g)){
      oldMod<-modularity(g,membership = communityStructure)
      ineighbors<-neighbors(g,i)
      
      if(length(ineighbors)){
        modularvec = sapply(unique(communityStructure[ineighbors]), modValue , g, communityStructure, i)
        cosSim = sapply(unique(communityStructure[ineighbors]), cosineSim , attrData, communityStructure, i)
        deltaScore = alpha * (modularvec - oldMod) + ((1 - alpha) * cosSim)
        max <- max(deltaScore)
        if(max > 0){
          noOfMods = noOfMods + 1
          iComm <- unique(communityStructure[ineighbors])
          communityStructure[i] <- iComm[which.max(deltaScore)]
        }  
      }
    }
  }
  if(isTRUE(all.equal(oldCommStructure,communityStructure)))
  {
    break
  }
  oldCommStructure=communityStructure
}

# ===== Write the communities into file ==== #

fileName<-paste("communities",alpha,sep="_")
fileName<-paste(fileName,"txt",sep=".")
fileDes<-file(fileName,"w")

for(i in 1:length(unique(communityStructure)))
{
  finalComm <- vector("numeric")
  for(j in 1:vcount(g))
  {
    if(communityStructure[j]==unique(communityStructure)[i]){
      finalComm <- append(finalComm,j,after = length(finalComm))
    }
  }
  cat(as.character(finalComm), file=fileDes,sep = ",")
  cat("\n", file=fileDes)
}
close(fileDes)


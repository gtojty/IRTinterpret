###############################################################
# IRT model analysis
# Author: Tao Gong (tgong@ets.org), Educational Testing Service
###############################################################

pkgInstall <- function(x){
  #' This function checks if a package (x) exists, if not, install it
  #' @param x string name of a package
  if (!require(x,character.only = TRUE)){
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
# required R packages (not all codes need all)
pkgInstall("psych"); require(psych)
pkgInstall("car"); require(car)
pkgInstall("plyr"); require(plyr)
pkgInstall("pastecs"); require(pastecs)
pkgInstall("ggplot2"); require(ggplot2)
pkgInstall("scales"); require(scales)
pkgInstall("plotly"); require(plotly)
pkgInstall("Rmisc"); require(Rmisc)
pkgInstall("GGally"); require(GGally)
pkgInstall("reshape2"); require(reshape2)
pkgInstall("plotrix"); require(plotrix)
pkgInstall("MASS"); require(MASS)
pkgInstall("stringr"); require(stringr)
pkgInstall("Hmisc"); require(Hmisc)
pkgInstall("TraMineR"); require(TraMineR)
pkgInstall("TraMineRextras"); require(TraMineRextras)
pkgInstall("cluster"); require(cluster)
pkgInstall("gtools"); require(gtools)
pkgInstall("ggpubr"); require(ggpubr)
pkgInstall("compare"); require(compare)
pkgInstall("mirt"); require(mirt)
pkgInstall('ltm'); require(ltm)
pkgInstall("tidyr"); require(tidyr)
pkgInstall("lmerTest"); require(lmerTest)
pkgInstall("ordinal"); require(ordinal)

# for drawing figures
geom.text.size = 6; theme.size = 4*geom.text.size
settings <- theme_bw() + 
  theme(plot.title=element_text(size=theme.size), axis.title=element_text(size=theme.size, face="bold"), 
        axis.text.x=element_text(angle=45, hjust=1, size=theme.size), axis.text.y=element_text(size=theme.size),
        panel.grid.major=element_line(colour="grey", linetype="dotted", size=1), 
        legend.title=element_text(size=theme.size), legend.text=element_text(size=theme.size))

savefig <- function(fileName, dpi, width, height, units, type){
  #' Function to save figures
  #' @param fileName file name of the figure
  #' @param dpi resolution, e.g., 300
  #' @param width figure width in inches (units)
  #' @param height figure height in inches (units)
  #' @param units unit name for width and height, e.g., 'in'
  #' @param type figure type: 'png', png figure; 'pdf', pdf figure; 'both', both png and pdf figures
  
  if(type=="png"){ file <- paste(fileName, ".png", sep=""); ggsave(file, dpi = dpi, width = width, height = height, units = units) }
  if(type=="pdf"){ file <- paste(fileName, ".pdf", sep=""); ggsave(file, dpi = dpi, width = width, height = height, units = units) }
  if(type=="both"){
    file <- paste(fileName, ".png", sep=""); ggsave(file, dpi = dpi, width = width, height = height, units = units)
    file <- paste(fileName, ".pdf", sep=""); ggsave(file, dpi = dpi, width = width, height = height, units = units)
  }
}


setwd(".") # set current working directory. 
numItem <- 50; numAg <- 200; numRun <- 10; numT <- 4000; numTypeItem <- 5; step <- 50; chkStep <- 800/4
testpointlist <- seq(0,numT,step); teachIDlist <- seq(1,numRun,1); chkpointlist <- seq(0,numT,chkStep)
semchangepointlist <- c(800, 1600, 2400, 3200, 4000)
folderList <- c("updfreq", "updrate", "updboth", "updnone")

# prepare data and draw figures
prepData <- function(direct, showprogress=FALSE){
  #' function to prepare data for drawing and analyses
  #' @param direct directory for a condition
  #' @param showprogress whether or not show on screen the progress
  
  resDF <- data.frame(); itemscDF <- data.frame() # store all runs' results
  for(runID in seq(1,numRun,1)){
    # 1) get students parameters
    if(showprogress) cat(paste("Student/Item Info Run: ", runID, "\n", sep=""))
    
    # population info
    popinfo <- read.csv(file.path(direct, 'teach_com1', runID, 'indmat_0.csv'))
    popDF <- data.frame()
    for(ag in seq(0,numAg-1,1)){
      popDF <- rbind(popDF, data.frame(ind = ag, 
                                       updrate = unique(popinfo$updrate[popinfo$ind==ag]), 
                                       updfreq = unique(popinfo$updfreq[popinfo$ind==ag])))
    }
    write.csv(popDF, file.path(direct, paste('test_com1_teach', runID, sep=""), "popInfo.csv"), row.names = FALSE)
    
    # 2) get question info
    iteminfo <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), "1", "exam_answers.csv"))
    names(iteminfo) <- c('U', 'M')
    teacherinfo <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), "1", "exam_answers_teacher.csv"))
    names(teacherinfo) <- c('U', 'M_t')
    newinfo <- merge(iteminfo, teacherinfo, by='U')
    newinfo$type <- NA
    for(item in seq(0, numItem-1, 1)){
      if(item<numItem/numTypeItem){ newinfo$type[newinfo$M==paste('M', item, sep="")] <- 1 }
      if((numItem/numTypeItem<=item)&(item<2*numItem/numTypeItem)){ newinfo$type[newinfo$M==paste('M', item, sep="")] <- 2 }
      if((2*numItem/numTypeItem<=item)&(item<3*numItem/numTypeItem)){ newinfo$type[newinfo$M==paste('M', item, sep="")] <- 3 }
      if((3*numItem/numTypeItem<=item)&(item<4*numItem/numTypeItem)){ newinfo$type[newinfo$M==paste('M', item, sep="")] <- 4 }
      if((4*numItem/numTypeItem<=item)&(item<numItem)) { newinfo$type[newinfo$M==paste('M', item, sep="")] <- 5 }
    }
    write.csv(newinfo, file.path(direct, paste('test_com1_teach', runID, sep=""), 'itemInfo.csv'), row.names = FALSE)                     
  
    # 3) get student's info and scores at certain points
    if(showprogress) cat(paste("TestPoint Info Run: ", runID, "\n", sep=""))
    
    popDF <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), "popInfo.csv"))
    popDF$ind <- paste('Ag', popDF$ind, sep="")
    itemDF <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), 'itemInfo.csv'))
    
    for(testpoint in testpointlist){
      answersheet <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), "1", paste('answer_sheet_', testpoint, ".csv", sep="")))
      names(answersheet)[1] <- "ind"
    
      # (a) merge answersheet with popDF
      newpopDF <- merge(popDF, answersheet, by='ind')
      newpopDF$Sc <- apply(newpopDF[,paste("U", seq(0, numItem-1, 1), sep="")], 1, sum)
      # add scores based on type
      newpopDF$Sc_1 <- NA; newpopDF$Sc_2 <- NA; newpopDF$Sc_3 <- NA; newpopDF$Sc_4 <- NA; newpopDF$Sc_5 <- NA
      for(type in seq(1, numTypeItem, 1)){
          itemList <- as.character(itemDF$U[itemDF$type==type])
          newpopDF[, paste('Sc_', type, sep="")] <- apply(newpopDF[, itemList], 1, sum)
      }
      # sort by ind value
      newpopDF$oldind <- gsub("Ag", "", newpopDF$ind); newpopDF$oldind <- as.numeric(newpopDF$oldind)
      newpopDF <- newpopDF[order(newpopDF$oldind),]; newpopDF <- subset(newpopDF, select = -c(oldind))
      write.csv(newpopDF, file.path(direct, paste('test_com1_teach', runID, sep=""), paste("popInfoSc_", testpoint, ".csv", sep="")), row.names = FALSE)
      # add data to resDF
      newpopDF$runID <- runID; newpopDF$testTime <- testpoint
      chocol_pop <- c("runID", "testTime", "ind", "updrate", "updfreq", "Sc", paste("Sc_", seq(1, numTypeItem, 1), sep=""))
      resDF <- rbind(resDF, newpopDF[, chocol_pop])
      
      # (b) merge item info into newitemDF
      scDF <- data.frame('U'= paste('U', seq(0, numItem-1, 1), sep=""), 
                         'Sc' = apply(newpopDF[,paste("U", seq(0, numItem-1, 1), sep="")], 2, sum))
      newitemDF <- merge(itemDF, scDF, by='U')
      write.csv(newitemDF, file.path(direct, paste('test_com1_teach', runID, sep=""), paste("itemInfoSc_", testpoint, ".csv", sep="")), row.names = FALSE)
      # add data to itemscDF
      newitemDF$runID <- runID; newitemDF$testTime <- testpoint
      chocol_item <- c("runID", "testTime", "type", "U", "M", "M_t", "Sc")
      itemscDF <- rbind(itemscDF, newitemDF[, chocol_item])
    }
  }
  resDF$ind <- gsub("Ag", "", resDF$ind); resDF$ind <- as.numeric(resDF$ind)
  resDF <- resDF[order(resDF[,'runID'], resDF[,'testTime'], resDF[,'ind']),]
  write.csv(resDF, file.path(direct, "AllResPop.csv"), row.names = FALSE)
  itemscDF <- itemscDF[order(itemscDF[,'runID'], itemscDF[,'testTime'], itemscDF[,'type']),]
  write.csv(itemscDF, file.path(direct, "AllResItem.csv"), row.names = FALSE)
}

drawData <- function(direct, showprogress=FALSE){
  #' draw agent's score and item's score figures
  #' @param direct directory for a condition
  #' @param showprogress whether or not show on screen the progress
  
  # analysis
  resDF <- read.csv(file.path(direct, "AllResPop.csv"))
  itemscDF <- read.csv(file.path(direct, "AllResItem.csv"))
  
  # draw correctness figure
  sizex <- 12; sizey <- 8
  for(runID in seq(1, numRun, 1)){
    if(showprogress) cat(paste("draw figures Run ", runID, "\n", sep=""))
    
    df <- resDF[(resDF$runID==runID)&(resDF$testTime%in%chkpointlist),]
    # 1) pop score
    filename <- file.path(direct, paste('AgentSc_run', runID, sep=''))
    title <- paste("Agent Scores in Run ", runID, sep="")
    xlab <- 'Testing Rounds'; xlim <- c(-100,4100); ylab <- 'Scores'; ylim <- c(-1, numItem+5)
    ggplot(df, aes(x = testTime, y = Sc)) +
      geom_point(colour='black', shape=16, alpha=.6, size=1, position=position_jitter(width=50)) + 
      geom_boxplot(data = df, aes(x = df$testTime, y = df$Sc, group = df$testTime), fill='gray', color='red', size=1, outlier.shape=NA) + 
      #geom_smooth(method='loess', color="blue", size=1.5) + 
      stat_summary(fun.y=mean, geom="line", color = 'blue', size=1.5) + 
      ggtitle(title) + scale_x_continuous(name=xlab, limits=xlim, breaks=chkpointlist) + 
      ylab(ylab) + ylim(ylim) + 
      settings
    savefig(filename, 300, sizex, sizey, "in", "both")
    
    # 2) pop by item type score
    # get the long data
    df_long <- gather(df, type, val, paste("Sc_", seq(1, numTypeItem, 1), sep=""), factor_key=TRUE)
    group.mean <- summarySE(df_long, measurevar = "val", groupvars = c("testTime", "type"), na.rm = TRUE)
    group.mean$type <- factor(group.mean$type)
    filename <- file.path(direct, paste('AgentSc_byItemType_run', runID, sep=''))
    title <- paste("Agent Scores (by Item Types) in Run ", runID, sep="")
    xlab <- 'Testing Rounds'; xlim <- c(-100,4100); ylab <- 'Scores'; ylim <- c(-1, numItem/numTypeItem+1)
    ggplot(group.mean, aes(x = testTime, y = val, group = type, color = type)) +
      geom_line(size=1.5) + 
      geom_errorbar(aes(ymin=val-se, ymax=val+se), size=1.5, position=position_dodge(width=100)) + 
      ggtitle(title) + scale_x_continuous(name=xlab, limits=xlim, breaks=chkpointlist) + 
      ylab(ylab) + ylim(ylim) + 
      settings
    savefig(filename, 300, sizex, sizey, "in", "both")
    
    
    # 3) item score
    df <- itemscDF[(itemscDF$runID==runID)&(itemscDF$testTime%in%chkpointlist),]
    group.mean <- summarySE(df, measurevar = "Sc", groupvars = c("testTime", "type"), na.rm = TRUE)
    group.mean$type <- factor(group.mean$type)
    filename <- file.path(direct, paste('ItemSc_run', runID, sep=''))
    title <- paste("Item Scores in Run ", runID, sep="")
    xlab <- 'Testing Rounds'; xlim <- c(-100,4100); ylab <- 'Scores'; ylim <- c(-1, numAg+5)
    ggplot(group.mean, aes(x = testTime, y = Sc, group = type, color = type)) +
      geom_line(size=1.5) + 
      geom_errorbar(aes(ymin=Sc-se, ymax=Sc+se), size=1.5, position=position_dodge(width=100)) + 
      ggtitle(title) + scale_x_continuous(name=xlab, limits=xlim, breaks=chkpointlist) + 
      ylab(ylab) + ylim(ylim) + 
      settings
    savefig(filename, 300, sizex, sizey, "in", "both")
  }
}

# get the score data
for(folder in folderList){
  cat(paste("Processing folder: ", folder, "\n", sep=""))
  direct <- file.path(".", folder)
  prepData(direct, showprogress = TRUE) # prepare data
  drawData(direct, showprogress = TRUE) # draw figures
}

resAllDF <- data.frame(); resAllDF_long <- data.frame(); itemscAllDF <- data.frame()
for(folder in folderList){
  direct <- file.path(".", folder)
  resDF <- read.csv(file.path(direct, "AllResPop.csv")); resDF$cond <- folder
  resDF_long <- gather(resDF, type, val, paste("Sc_", seq(1, numTypeItem, 1), sep=""), factor_key=TRUE)
  itemscDF <- read.csv(file.path(direct, "AllResItem.csv")); itemscDF$cond <- folder
  resAllDF <- rbind(resAllDF, resDF)
  resAllDF_long <- rbind(resAllDF_long, resDF_long)
  itemscAllDF <- rbind(itemscAllDF, itemscDF)
}
write.csv(resAllDF, file.path(".", "resAll.csv"), row.names = FALSE)
write.csv(resAllDF_long, file.path(".", "resAll_long.csv"), row.names = FALSE)
write.csv(itemscAllDF, file.path(".", "itemscAll.csv"), row.names = FALSE)


# statistical analyses; whether individual difference affects students' scores
resAllDF <- read.csv(file.path(".", "resAll.csv")); resAllDF$cond <- relevel(resAllDF$cond, "updnone")
resAllDF_long <- read.csv(file.path(".", "resAll_long.csv")); resAllDF_long$cond <- relevel(resAllDF_long$cond, "updnone")
itemscAllDF <- read.csv(file.path(".", "itemscAll.csv")); itemscAllDF$cond <- relevel(itemscAllDF$cond, "updnone"); itemscAllDF$type <- factor(itemscAllDF$type)

# condition "updnone"
# already draw score figures

# condition "updrate"
folder <- "updrate"
resAllDF_sub <- resAllDF[(resAllDF$cond==folder) & (resAllDF$testTime %in% semchangepointlist),]
resAllDF_long_sub <- resAllDF_long[(resAllDF_long$cond==folder) & (resAllDF_long$testTime %in% semchangepointlist), ]
itemscAllDF_sub <- itemscAllDF[(itemscAllDF$cond==folder) & (itemscAllDF$testTime %in% semchangepointlist), ]

# effects of updrate and updfreq and condition on agents' scores
summary(lmer(Sc ~ updrate + (1|runID) + (1|testTime) + (1|ind), data=resAllDF_sub,
             control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 51047.4
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -10.9280  -0.5544   0.0472   0.6135   3.0184 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ind      (Intercept)  0.1687  0.4108  
# runID    (Intercept)  0.3417  0.5845  
# testTime (Intercept) 13.0380  3.6108  
# Residual              9.4631  3.0762  
# Number of obs: 10000, groups:  ind, 200; runID, 10; testTime, 5
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   32.2635     1.6287    4.1367   19.81 2.95e-05 ***
#   updrate        9.0502     0.3165 9126.6094   28.60  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# updrate -0.059

summary(lmer(val ~ updrate + (1|runID) + (1|testTime) + (1|ind), data=resAllDF_long_sub,
             control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7))))
# REML criterion at convergence: 190429.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.2815 -0.5807  0.1409  0.7354  2.7329 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ind      (Intercept) 0.003787 0.06153 
# runID    (Intercept) 0.013524 0.11629 
# testTime (Intercept) 0.521485 0.72214 
# Residual             2.632938 1.62263 
# Number of obs: 50000, groups:  ind, 200; runID, 10; testTime, 5
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept) 6.451e+00  3.259e-01 4.144e+00   19.79 2.92e-05 ***
#   updrate     1.815e+00  7.375e-02 1.872e+04   24.62  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# updrate -0.068

summary(lmer(Sc ~ type + (1|runID) + (1|testTime), data=itemscAllDF_sub,
             control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 26837.5
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.7163 -0.5030  0.1625  0.7305  1.7748 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# runID    (Intercept)    0.0    0.00   
# testTime (Intercept)  203.3   14.26   
# Residual             2698.2   51.94   
# Number of obs: 2500, groups:  runID, 10; testTime, 5
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)  182.451      6.830    4.998   26.71 1.38e-06 ***
#   type2        -17.280      3.463 2491.000   -4.99 6.46e-07 ***
#   type3        -57.564      3.239 2491.000  -17.77  < 2e-16 ***
#   type4        -59.677      3.375 2491.000  -17.68  < 2e-16 ***
#   type5        -67.925      3.375 2491.000  -20.12  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) type2  type3  type4 
# type2 -0.253                     
# type3 -0.271  0.535              
# type4 -0.260  0.513  0.548       
# type5 -0.260  0.513  0.548  0.526


# condition "updfreq"
folder <- "updfreq"
resAllDF_sub <- resAllDF[(resAllDF$cond==folder) & (resAllDF$testTime %in% semchangepointlist),]
resAllDF_long_sub <- resAllDF_long[(resAllDF_long$cond==folder) & (resAllDF_long$testTime %in% semchangepointlist), ]
itemscAllDF_sub <- itemscAllDF[(itemscAllDF$cond==folder) & (itemscAllDF$testTime %in% semchangepointlist), ]

# effects of updrate and updfreq and condition on agents' scores
summary(lmer(Sc ~ updfreq + (1|runID) + (1|testTime) + (1|ind), data=resAllDF_sub,
             control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 47086.8
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.0205 -0.6780  0.0297  0.6867  3.5944 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ind      (Intercept)  0.0000  0.0000  
# runID    (Intercept)  0.2568  0.5068  
# testTime (Intercept) 10.8009  3.2865  
# Residual              6.4481  2.5393  
# Number of obs: 10000, groups:  ind, 200; runID, 10; testTime, 5
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   36.0503     1.4844    4.1593  24.286 1.22e-05 ***
#   updfreq       -0.3441     0.2602 9988.3493  -1.322    0.186    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# updfreq -0.088

summary(lmer(val ~ updfreq + (1|runID) + (1|testTime) + (1|ind), data=resAllDF_long_sub, 
             control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7))))
# REML criterion at convergence: 185873.5
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.3797 -0.5554  0.1696  0.7826  2.6146 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ind      (Intercept) 0.00000  0.0000  
# runID    (Intercept) 0.01005  0.1002  
# testTime (Intercept) 0.43193  0.6572  
# Residual             2.40667  1.5513  
# Number of obs: 50000, groups:  ind, 200; runID, 10; testTime, 5
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  7.210e+00  2.978e-01  4.213e+00  24.208 1.11e-05 ***
#   updfreq     -6.863e-02  7.110e-02  4.999e+04  -0.965    0.334    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# updfreq -0.120

summary(lmer(Sc ~ type + (1|runID) + (1|testTime), data=itemscAllDF_sub,
             control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 26820.9
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.8186 -0.4502  0.1745  0.6854  1.7167 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# runID    (Intercept)    0.0    0.00   
# testTime (Intercept)  167.5   12.94   
# Residual             2681.1   51.78   
# Number of obs: 2500, groups:  runID, 10; testTime, 5
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)  185.120      6.282    5.210  29.470 5.35e-07 ***
#   type2        -15.564      3.452 2491.000  -4.509 6.82e-06 ***
#   type3        -57.398      3.229 2491.000 -17.776  < 2e-16 ***
#   type4        -58.448      3.365 2491.000 -17.372  < 2e-16 ***
#   type5        -66.708      3.365 2491.000 -19.827  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) type2  type3  type4 
# type2 -0.275                     
# type3 -0.294  0.535              
# type4 -0.282  0.513  0.548       
# type5 -0.282  0.513  0.548  0.526


# condition "updboth"
folder <- "updboth"
resAllDF_sub <- resAllDF[(resAllDF$cond==folder) & (resAllDF$testTime %in% semchangepointlist),]
resAllDF_long_sub <- resAllDF_long[(resAllDF_long$cond==folder) & (resAllDF_long$testTime %in% semchangepointlist), ]
itemscAllDF_sub <- itemscAllDF[(itemscAllDF$cond==folder) & (itemscAllDF$testTime %in% semchangepointlist), ]

# effects of updrate and updfreq and condition on agents' scores
withInt <- lmer(Sc ~ updrate*updfreq + (1|runID) + (1|testTime) + (1|ind), data=resAllDF_sub,
                control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)))
withoutInt <- lmer(Sc ~ updrate + updfreq + (1|runID) + (1|testTime) + (1|ind), data=resAllDF_sub,
                   control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)))
anova(withInt, withoutInt)
# withoutInt: Sc ~ updrate + updfreq + (1 | runID) + (1 | testTime) + (1 | ind)
# withInt: Sc ~ updrate * updfreq + (1 | runID) + (1 | testTime) + (1 | ind)
# Df   AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# withoutInt  7 51174 51224 -25580    51160                         
# withInt     8 51176 51234 -25580    51160 0.0712      1     0.7896

summary(withoutInt)
# REML criterion at convergence: 51158.1
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -10.6597  -0.5533   0.0465   0.6234   3.0831 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ind      (Intercept)  0.2392  0.4891  
# runID    (Intercept)  0.2876  0.5362  
# testTime (Intercept) 13.7506  3.7082  
# Residual              9.5369  3.0882  
# Number of obs: 10000, groups:  ind, 200; runID, 10; testTime, 5
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept) 3.216e+01  1.678e+00 4.190e+00  19.167 3.06e-05 ***
#   updrate     1.048e+01  3.169e-01 9.574e+03  33.082  < 2e-16 ***
#   updfreq     1.212e-02  3.225e-01 9.509e+03   0.038     0.97    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) updrat
# updrate -0.055       
# updfreq -0.096 -0.016

withInt <- lmer(val ~ updrate*updfreq + (1|runID) + (1|testTime) + (1|ind), data=resAllDF_long_sub,
             control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7)))
withoutInt <- lmer(val ~ updrate + updfreq + (1|runID) + (1|testTime) + (1|ind), data=resAllDF_long_sub,
                control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7)))
anova(withInt, withoutInt)
# withoutInt: val ~ updrate + updfreq + (1 | runID) + (1 | testTime) + (1 | ind)
# withInt: val ~ updrate * updfreq + (1 | runID) + (1 | testTime) + (1 | ind)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# withoutInt  7 190481 190542 -95233   190467                         
# withInt     8 190483 190553 -95233   190467 0.0647      1     0.7992

summary(withoutInt)
# REML criterion at convergence: 190473.8
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.1709 -0.5626  0.1346  0.7313  2.6723 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ind      (Intercept) 0.006664 0.08163 
# runID    (Intercept) 0.011352 0.10655 
# testTime (Intercept) 0.549906 0.74156 
# Residual             2.633311 1.62275 
# Number of obs: 50000, groups:  ind, 200; runID, 10; testTime, 5
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  6.433e+00  3.363e-01  4.225e+00  19.131 2.89e-05 ***
#   updrate      2.102e+00  7.379e-02  2.524e+04  28.483  < 2e-16 ***
#   updfreq     -2.208e-03  7.506e-02  2.404e+04  -0.029    0.977    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) updrat
# updrate -0.064       
# updfreq -0.111 -0.016

summary(lmer(Sc ~ type + (1|runID) + (1|testTime), data=itemscAllDF_sub,
             control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 26844.1
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.6542 -0.4673  0.1681  0.7164  1.8454 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# runID    (Intercept)    0.0    0.00   
# testTime (Intercept)  214.7   14.65   
# Residual             2705.1   52.01   
# Number of obs: 2500, groups:  runID, 10; testTime, 5
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)  184.529      6.996    4.946  26.376 1.64e-06 ***
#   type2        -17.916      3.467 2491.000  -5.167 2.57e-07 ***
#   type3        -59.721      3.243 2491.000 -18.413  < 2e-16 ***
#   type4        -58.667      3.380 2491.000 -17.359  < 2e-16 ***
#   type5        -69.983      3.380 2491.000 -20.707  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) type2  type3  type4 
# type2 -0.248                     
# type3 -0.265  0.535              
# type4 -0.254  0.513  0.548       
# type5 -0.254  0.513  0.548  0.526


# all together
withInt <- lmer(Sc ~ updrate*updfreq + (1|runID) + (1|testTime) + (1|ind) + (1|cond), 
                data=resAllDF[resAllDF$testTime %in% semchangepointlist,],
                control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7)))
withoutInt <- lmer(Sc ~ updrate + updfreq + (1|runID) + (1|testTime) + (1|ind) + (1|cond), 
                   data=resAllDF[resAllDF$testTime %in% semchangepointlist,],
                   control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7)))
anova(withInt, withoutInt)
# withoutInt: Sc ~ updrate + updfreq + (1 | runID) + (1 | testTime) + (1 | ind) + (1 | cond)
# withInt: Sc ~ updrate * updfreq + (1 | runID) + (1 | testTime) + (1 | ind) + (1 | cond)
# Df    AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# withoutInt  8 198668 198737 -99326   198652                         
# withInt     9 198670 198748 -99326   198652 0.0139      1     0.9062

summary(withoutInt)
# REML criterion at convergence: 198652.5
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -12.0291  -0.5976   0.0425   0.6452   3.4674 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# ind      (Intercept)  0.02869 0.1694  
# runID    (Intercept)  0.04675 0.2162  
# testTime (Intercept) 12.53956 3.5411  
# cond     (Intercept)  0.13921 0.3731  
# Residual              8.36157 2.8916  
# Number of obs: 40000, groups:  ind, 200; runID, 10; testTime, 5; cond, 4
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)    32.6325     1.6007     4.1711  20.386 2.46e-05 ***
#   updrate         9.7371     0.2048 39338.4256  47.541  < 2e-16 ***
#   updfreq        -0.2850     0.2088 39544.5726  -1.365    0.172    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) updrat
# updrate -0.038       
# updfreq -0.065 -0.008

withInt <- lmer(val ~ updrate*updfreq + (1|runID) + (1|testTime) + (1|ind) + (1|cond), 
                data=resAllDF_long[resAllDF_long$testTime %in% semchangepointlist, ],
                control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7)))
withoutInt <- lmer(val ~ updrate + updfreq + (1|runID) + (1|testTime) + (1|ind) + (1|cond), 
                   data=resAllDF_long[resAllDF_long$testTime %in% semchangepointlist, ],
                   control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e7)))
anova(withInt, withoutInt)
# withoutInt: val ~ updrate + updfreq + (1 | runID) + (1 | testTime) + (1 | ind) + (1 | cond)
# withInt: val ~ updrate * updfreq + (1 | runID) + (1 | testTime) + (1 | ind) + (1 | cond)
# Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
# withoutInt  8 755694 755775 -377839   755678                        
# withInt     9 755696 755788 -377839   755678 0.011      1     0.9163

summary(withoutInt)
# REML criterion at convergence: 755686.5
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.3991 -0.5725  0.1641  0.7495  2.7527 
# 
# Random effects:
#   Groups   Name        Variance  Std.Dev.
# ind      (Intercept) 0.0002601 0.01613 
# runID    (Intercept) 0.0018257 0.04273 
# testTime (Intercept) 0.5012137 0.70796 
# cond     (Intercept) 0.0055508 0.07450 
# Residual             2.5600161 1.60001 
# Number of obs: 200000, groups:  ind, 200; runID, 10; testTime, 5; cond, 4
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)  6.527e+00  3.205e-01  4.202e+00  20.364 2.33e-05 ***
#   updrate      1.949e+00  5.049e-02  1.415e+05  38.597  < 2e-16 ***
#   updfreq     -5.800e-02  5.149e-02  1.513e+05  -1.126     0.26    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) updrat
# updrate -0.047       
# updfreq -0.080 -0.008

summary(lmer(Sc ~ type + (1|runID) + (1|testTime) + (1|cond), 
             data=itemscAllDF[itemscAllDF$testTime %in% semchangepointlist, ],
             control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 107422
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.7504 -0.4757  0.1675  0.7182  1.8119 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# runID    (Intercept)    0.000  0.000  
# testTime (Intercept)  199.242 14.115  
# cond     (Intercept)    1.051  1.025  
# Residual             2707.252 52.031  
# Number of obs: 10000, groups:  runID, 10; testTime, 5; cond, 4
# 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)  184.143      6.451    4.303  28.545 4.53e-06 ***
#   type2        -17.322      1.734 9988.000  -9.988  < 2e-16 ***
#   type3        -58.147      1.622 9988.000 -35.841  < 2e-16 ***
#   type4        -58.797      1.690 9988.000 -34.782  < 2e-16 ***
#   type5        -68.232      1.690 9988.000 -40.363  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) type2  type3  type4 
# type2 -0.134                     
# type3 -0.144  0.535              
# type4 -0.138  0.513  0.548       
# type5 -0.138  0.513  0.548  0.526

# # clmm2
# resAllDF$Sc <- factor(resAllDF$Sc)
# summary(clmm2(Sc ~ updrate*updfreq + (1|runID) + (1|testTime) + (1|ind) + (1|cond), data = resAllDF, Hess = TRUE, nAGQ = 10))
# resAllDF_long$val <- factor(resAllDF_long$val); 
# summary(clmm2(val ~ unpdrate*updfreq*type + (1|runID) + (1|testTime) + (1|ind) + (1|cond), data = resAllDF_long, Hess = TRUE, nAGQ = 10))
# itemscAllDF$Sc <- factor(itemscAllDF$Sc)
# summary(clmm2(Sc ~ type + (1|runID) + (1|testTime)+ (1|cond), data = itemscAllDF, Hess = TRUE, nAGQ = 10))



# IRT analysis
getCorrectWrong <- function(answer_sheet, score){
  #' Get the vector of items that are all correct or wrong
  #' @param answer_sheet answer sheet of students
  #' @param score 0 (all wrong) or 1 (all correct)
  #' @return a vector of all correct or wrong items
  vec <- c()
  for(item in names(answer_sheet)){
    if(sum(answer_sheet[,item]==score)==nrow(answer_sheet)){
      vec <- c(vec, item)
    }
  }
  return(vec)
}

IRTanalysis <- function(direct, answersheet_all, popDF, package){
  #' IRT analysis
  #' @param direct directory for a condition
  #' @param answersheet_all combined answersheet
  #' @param popDF population parameters
  #' @param package IRT model package
  #' @return popDF result df with theta
  
  # train IRT model to get theta value of each agent
  IRTsheet <- answersheet_all[,paste('U', seq(0, numItem-1, 1), sep="")]
  #row.names(IRTsheet) <- answersheet_all$ind
  
  # remvoe all correct or all wrong items
  all_wrong_items <- getCorrectWrong(IRTsheet, 0)
  all_correct_items <- getCorrectWrong(IRTsheet, 1)
  if((length(all_wrong_items)!=0)|(length(all_correct_items)!=0)){
    IRTsheet_new <- IRTsheet[, -which(colnames(IRTsheet) %in% c(all_wrong_items, all_correct_items))]
  }else{ IRTsheet_new <- IRTsheet }
  
  if(length(names(IRTsheet_new))<=4){
    cat("no way to train IRT model, all correct/wrong!\n")
    theta <- rep(NA, nrow(IRTsheet)) 
  }else{
    # train the model
    if(package=='ltm'){
      # using ltm package
      result <- try(IRTmodel <- ltm(as.matrix(IRTsheet_new) ~ z1, IRT.param=TRUE, control = list(GHk = 20, iter.em = 2000)), 
                    silent = TRUE) # 2PL model
      #result <- try(IRTmodel <- rasch(IRTsheet_new, IRT.param=TRUE, control = list(GHk = 20, iter.em=2000)), 
      #              silent = TRUE) # Rasch model
      if(class(result)[1]=='try-error'){ # no convergence!
        cat("IRT model no convergence!\n")
        theta <- rep(NA, nrow(IRTsheet)) 
      }else{ # convergence
        #summary(IRTmodel)
        theta_val <- factor.scores(IRTmodel)$score.dat
        theta <- c()
        collength <- length(names(IRTsheet_new))
        for(i in 1:nrow(IRTsheet_new)){
          ag <- row.names(IRTsheet_new)[i]
          for(j in 1:nrow(theta_val)){
            if(collength==sum(IRTsheet_new[ag, 1:collength]==theta_val[j,1:collength])){
              theta <- c(theta, theta_val$z1[j])
              break
            }
          }
        }
      }
    }
    if(package=='mirt'){
      # using mirt package
      #IRTmodel <- mirt(IRTsheet_new, model=1, itemtype='Rasch', SE=TRUE, technical = list(NCYCLES = 2000))
      IRTmodel <- mirt(IRTsheet_new, model=1, itemtype ='2PL', SE=TRUE, technical = list(NCYCLES = 2000))
      #summary(IRTmodel)
      theta <- fscores(IRTmodel)
    }
  }
  
  # add theta to pop info
  popDF$theta <- theta
  return(popDF)
}

IRTData <- function(direct, package, numPop){
  #' prepare IRT models and thetaes of students and answer sheet populations at different chosen sampling points
  #' also create IRT models only based on 200 learning
  #' @param direct directory for a condition
  #' @param package 'ltm' or 'mirt'
  #' @param numPop number of random populations
  
  # pop 1: run 1 (800) + run 2 (1600) + run 3 (2400) + run 4 (3200) + run 5 (4000)
  answersheet_all <- data.frame()
  popDF <- data.frame()
  runIDlist <- c(1,2,3,4,5)
  for(runID in runIDlist){
    semchangepoint <- semchangepointlist[runID]
    
    answersheet <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), "1", paste('answer_sheet_', semchangepoint, ".csv", sep="")))
    names(answersheet)[1] <- "ind"
    answersheet$ind <- paste("Ag", (200*(runID-1)):(199+200*(runID-1)), sep="")
    answersheet_all <- rbind(answersheet_all, answersheet)
    
    newpopDF <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), paste("popInfoSc_", semchangepoint, ".csv", sep="")))
    newpopDF$ind <- paste("Ag", (200*(runID-1)):(199+200*(runID-1)), sep="")
    popDF <- rbind(popDF, newpopDF)
  }
  write.csv(answersheet_all, file.path(direct, "answersheet_pop1.csv"), row.names = FALSE)
  
  popDF <- IRTanalysis(direct, answersheet_all, popDF, package)
  popDFfileName <- file.path(direct, "popInfoScTheta_pop1.csv")
  write.csv(popDF, popDFfileName, row.names = FALSE)
  
  
  # pop 2: run 6 (800) + run 7 (1600) + run 8 (2400) + run 9 (3200) + run 10 (4000)
  answersheet_all <- data.frame()
  popDF <- data.frame()
  runIDlist <- c(6,7,8,9,10)
  for(runID in runIDlist){
    semchangepoint <- semchangepointlist[runID-5]
    
    answersheet <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), "1", paste('answer_sheet_', semchangepoint, ".csv", sep="")))
    names(answersheet)[1] <- "ind"
    row.names(answersheet) <- paste("Ag", (200*(runID-5-1)):(199+200*(runID-5-1)), sep="")
    answersheet_all <- rbind(answersheet_all, answersheet)

    newpopDF <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), paste("popInfoSc_", semchangepoint, ".csv", sep="")))
    newpopDF$ind <- paste("Ag", (200*(runID-1)):(199+200*(runID-1)), sep="")
    popDF <- rbind(popDF, newpopDF)
  }
  write.csv(answersheet_all, file.path(direct, "answersheet_pop2.csv"), row.names=FALSE)
  
  popDF <- IRTanalysis(direct, answersheet_all, popDF, package)
  popDFfileName <- file.path(direct, "popInfoScTheta_pop2.csv")
  write.csv(popDF, popDFfileName, row.names = FALSE)
  
  
  # pop 3: 10 randomly created population
  answersheet_allpop <- data.frame()
  popDF_allpop <- data.frame()
  for(i in 1:numPop){
    runIDlist <- sample(1:10, 5)
    answersheet_all <- data.frame()
    popDF <- data.frame()
    cur <- 1
    for(runID in runIDlist){
      semchangepoint <- semchangepointlist[cur]
      
      answersheet <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), "1", paste('answer_sheet_', semchangepoint, ".csv", sep="")))
      names(answersheet)[1] <- "ind"
      row.names(answersheet) <- paste("Ag", (200*(cur-1)):(199+200*(cur-1)), sep="")
      answersheet_all <- rbind(answersheet_all, answersheet)

      newpopDF <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), paste("popInfoSc_", semchangepoint, ".csv", sep="")))
      newpopDF$ind <- paste("Ag", (200*(runID-1)):(199+200*(runID-1)), sep="")
      popDF <- rbind(popDF, newpopDF)

      cur <- cur + 1
    }
    answersheet_all$pop <- i
    answersheet_allpop <- rbind(answersheet_allpop, answersheet_all)
    
    popDF <- IRTanalysis(direct, answersheet_all, popDF, package)
    popDF$pop <- i
    popDF_allpop <- rbind(popDF_allpop, popDF)
  }
  write.csv(answersheet_allpop, file.path(direct, "answersheet_randompop.csv"), row.names=FALSE)
  
  popDFfileName <- file.path(direct, "popInfoScTheta_randompop.csv")
  write.csv(popDF_allpop, popDFfileName, row.names = FALSE)
}

IRTData_fixL <- function(direct, package, numPop, lowL){
  #' prepare IRT models and thetaes of students and answer sheet populations at fixed chosen sampling points
  #' also create IRT models only based on 200 learning
  #' @param direct directory for a condition
  #' @param package 'ltm' or 'mirt'
  #' @param numPop number of random populations
  #' @param lowL lowest learning point
  
  # pop 1: run 1 (800) + run 2 (1600) + run 3 (2400) + run 4 (3200) + run 5 (4000)
  # only at 200 learning
  answersheet_all <- data.frame()
  popDF <- data.frame()
  runIDlist <- c(1,2,3,4,5)
  for(runID in runIDlist){
    semchangepoint <- lowL
    
    answersheet <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), "1", paste('answer_sheet_', semchangepoint, ".csv", sep="")))
    names(answersheet)[1] <- "ind"
    answersheet$ind <- paste("Ag", (200*(runID-1)):(199+200*(runID-1)), sep="")
    answersheet_all <- rbind(answersheet_all, answersheet)
    
    newpopDF <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), paste("popInfoSc_", semchangepoint, ".csv", sep="")))
    newpopDF$ind <- paste("Ag", (200*(runID-1)):(199+200*(runID-1)), sep="")
    popDF <- rbind(popDF, newpopDF)
  }
  write.csv(answersheet_all, file.path(direct, paste("answersheet_pop1_", semchangepoint, ".csv", sep="")), row.names = FALSE)
  
  popDF <- IRTanalysis(direct, answersheet_all, popDF, package)
  popDFfileName <- file.path(direct, paste("popInfoScTheta_pop1_", semchangepoint, ".csv", sep=""))
  write.csv(popDF, popDFfileName, row.names = FALSE)
  
  
  # pop 2: run 6 (800) + run 7 (1600) + run 8 (2400) + run 9 (3200) + run 10 (4000)
  # only at 200 learning
  answersheet_all <- data.frame()
  popDF <- data.frame()
  runIDlist <- c(6,7,8,9,10)
  for(runID in runIDlist){
    semchangepoint <- lowL
    
    answersheet <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), "1", paste('answer_sheet_', semchangepoint, ".csv", sep="")))
    names(answersheet)[1] <- "ind"
    row.names(answersheet) <- paste("Ag", (200*(runID-5-1)):(199+200*(runID-5-1)), sep="")
    answersheet_all <- rbind(answersheet_all, answersheet)
    
    newpopDF <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), paste("popInfoSc_", semchangepoint, ".csv", sep="")))
    newpopDF$ind <- paste("Ag", (200*(runID-1)):(199+200*(runID-1)), sep="")
    popDF <- rbind(popDF, newpopDF)
  }
  write.csv(answersheet_all, file.path(direct, paste("answersheet_pop2_", semchangepoint, ".csv", sep="")), row.names=FALSE)
  
  popDF <- IRTanalysis(direct, answersheet_all, popDF, package)
  popDFfileName <- file.path(direct, paste("popInfoScTheta_pop2_", semchangepoint, ".csv", sep=""))
  write.csv(popDF, popDFfileName, row.names = FALSE)
  
  
  # pop 3: 10 randomly created population
  # only at 200 learning
  answersheet_allpop <- data.frame()
  popDF_allpop <- data.frame()
  for(i in 1:numPop){
    runIDlist <- sample(1:10, 5)
    answersheet_all <- data.frame()
    popDF <- data.frame()
    cur <- 1
    for(runID in runIDlist){
      semchangepoint <- lowL
      
      answersheet <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), "1", paste('answer_sheet_', semchangepoint, ".csv", sep="")))
      names(answersheet)[1] <- "ind"
      row.names(answersheet) <- paste("Ag", (200*(cur-1)):(199+200*(cur-1)), sep="")
      answersheet_all <- rbind(answersheet_all, answersheet)
      
      newpopDF <- read.csv(file.path(direct, paste('test_com1_teach', runID, sep=""), paste("popInfoSc_", semchangepoint, ".csv", sep="")))
      newpopDF$ind <- paste("Ag", (200*(runID-1)):(199+200*(runID-1)), sep="")
      popDF <- rbind(popDF, newpopDF)
      
      cur <- cur + 1
    }
    answersheet_all$pop <- i
    answersheet_allpop <- rbind(answersheet_allpop, answersheet_all)
    
    popDF <- IRTanalysis(direct, answersheet_all, popDF, package)
    popDF$pop <- i
    popDF_allpop <- rbind(popDF_allpop, popDF)
  }
  write.csv(answersheet_allpop, file.path(direct, paste("answersheet_randompop_", semchangepoint, ".csv", sep="")), row.names=FALSE)
  
  popDFfileName <- file.path(direct, paste("popInfoScTheta_randompop_", semchangepoint, ".csv", sep=""))
  write.csv(popDF_allpop, popDFfileName, row.names = FALSE)
}


# all analysis
package <- 'mirt'
numPop <- 10
set.seed(1234)
for(folder in folderList){
  cat(paste("Processing folder: ", folder, "\n", sep=""))
  direct <- file.path(".", folder)
  IRTData(direct, package, numPop) # IRT data
}

# analysis
# cond updnone

# cond updrate
folder <- 'updrate'
pop1 <- read.csv(file.path(folder, 'popInfoScTheta_pop1.csv'))
pop2 <- read.csv(file.path(folder, 'popInfoScTheta_pop2.csv'))
popRandom <- read.csv(file.path(folder, 'popInfoScTheta_randompop.csv'))

# correlation between theta and Sc
rcorr(as.matrix(pop1[,c("theta", "Sc", "updrate")]), type="pearson")
# theta   Sc updrate
# theta    1.00 0.90    0.15
# Sc       0.90 1.00    0.17
# updrate  0.15 0.17    1.00
# 
# n= 1000 

rcorr(as.matrix(pop2[,c("theta", "Sc", "updrate")]), type="pearson")
# theta   Sc updrate
# theta    1.00 0.75    0.12
# Sc       0.75 1.00    0.21
# updrate  0.12 0.21    1.00
# 
# n= 1000 

rcorr(as.matrix(popRandom[,c("theta", "Sc", "updrate")]), type="pearson")
# theta   Sc updrate
# theta    1.00 0.85    0.15
# Sc       0.85 1.00    0.20
# updrate  0.15 0.20    1.00
# 
# n= 10000 

# theta vs. updrate
summary(lm(theta ~ updrate, pop1))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.9126 -0.6199  0.1741  0.6756  2.0424 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.41091    0.08824  -4.656 3.65e-06 ***
#   updrate      1.35258    0.27567   4.907 1.08e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8795 on 998 degrees of freedom
# Multiple R-squared:  0.02355,	Adjusted R-squared:  0.02258 
# F-statistic: 24.07 on 1 and 998 DF,  p-value: 1.082e-06

summary(lm(theta ~ updrate, pop2))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6029 -0.7501  0.1005  0.7080  2.1842 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.32485    0.08764  -3.707 0.000222 ***
#   updrate      1.07860    0.27632   3.903 0.000101 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8683 on 998 degrees of freedom
# Multiple R-squared:  0.01504,	Adjusted R-squared:  0.01405 
# F-statistic: 15.24 on 1 and 998 DF,  p-value: 0.0001012

summary(lmer(theta ~ updrate + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 25572.4
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.9728 -0.7108  0.2098  0.7342  2.8866 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# pop      (Intercept) 0.0000   0.0000  
# Residual             0.7546   0.8687  
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   -0.38680    0.02768 9998.00000  -13.97   <2e-16 ***
#   updrate        1.28002    0.08696 9998.00000   14.72   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# updrate -0.949

# score vs. updrate
summary(lm(Sc ~ updrate, pop1))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -32.218  -2.783   0.984   3.312   9.731 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  32.2183     0.4686  68.755  < 2e-16 ***
#   updrate       7.9419     1.4639   5.425 7.26e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4.67 on 998 degrees of freedom
# Multiple R-squared:  0.02865,	Adjusted R-squared:  0.02768 
# F-statistic: 29.43 on 1 and 998 DF,  p-value: 7.259e-08

summary(lm(Sc ~ updrate, pop2))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -32.542  -2.584   0.414   2.859  10.543 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  32.5420     0.4211  77.287  < 2e-16 ***
#   updrate       9.2095     1.3275   6.937 7.17e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4.172 on 998 degrees of freedom
# Multiple R-squared:  0.04601,	Adjusted R-squared:  0.04505 
# F-statistic: 48.13 on 1 and 998 DF,  p-value: 7.172e-12

summary(lmer(Sc ~ updrate + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 57961.9
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -7.4188 -0.5515  0.1733  0.6865  2.2862 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# pop      (Intercept)  0.02796 0.1672  
# Residual             19.24554 4.3870  
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   32.3901     0.1495  198.8173  216.69   <2e-16 ***
#   updrate        9.0994     0.4392 9992.2643   20.72   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# updrate -0.888


# cond updfreq
folder <- 'updfreq'
pop1 <- read.csv(file.path(folder, 'popInfoScTheta_pop1.csv'))
pop2 <- read.csv(file.path(folder, 'popInfoScTheta_pop2.csv'))
popRandom <- read.csv(file.path(folder, 'popInfoScTheta_randompop.csv'))

# correlation between theta and Sc
rcorr(as.matrix(pop1[,c("theta", "Sc", "updfreq")]), type="pearson")
# theta   Sc updfreq
# theta    1.00 0.84       0
# Sc       0.84 1.00       0
# updfreq  0.00 0.00       1
# 
# n= 1000

rcorr(as.matrix(pop2[,c("theta", "Sc", "updfreq")]), type="pearson")
# theta   Sc updfreq
# theta    1.00 0.84       0
# Sc       0.84 1.00       0
# updfreq  0.00 0.00       1
# 
# n= 1000

rcorr(as.matrix(popRandom[,c("theta", "Sc", "updfreq")]), type="pearson")
# theta   Sc updfreq
# theta    1.00 0.84       0
# Sc       0.84 1.00       0
# updfreq  0.00 0.00       1
# 
# n= 10000 

# theta vs. updfreq
summary(lm(theta ~ updfreq, pop1))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.5396 -0.5746  0.1882  0.6408  2.3314 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -0.01232    0.14522  -0.085    0.932
# updfreq      0.02448    0.28354   0.086    0.931
# 
# Residual standard error: 0.8876 on 998 degrees of freedom
# Multiple R-squared:  7.47e-06,	Adjusted R-squared:  -0.0009945 
# F-statistic: 0.007455 on 1 and 998 DF,  p-value: 0.9312

summary(lm(theta ~ updfreq, pop2))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3205 -0.6496  0.1669  0.6503  2.1700 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -0.01694    0.14590  -0.116    0.908
# updfreq      0.03383    0.28668   0.118    0.906
# 
# Residual standard error: 0.8781 on 998 degrees of freedom
# Multiple R-squared:  1.395e-05,	Adjusted R-squared:  -0.000988 
# F-statistic: 0.01393 on 1 and 998 DF,  p-value: 0.9061

summary(lmer(theta ~ updfreq + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 25702.6
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.8634 -0.6924  0.1912  0.7488  2.6526 
# 
# Random effects:
#   Groups   Name        Variance  Std.Dev. 
# pop      (Intercept) 2.190e-21 4.680e-11
# Residual             7.645e-01 8.744e-01
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)
# (Intercept) -8.455e-04  4.563e-02  9.998e+03  -0.019    0.985
# updfreq      1.715e-03  8.914e-02  9.998e+03   0.019    0.985
# 
# Correlation of Fixed Effects:
#   (Intr)
# updfreq -0.981

# Sc vs. updfreq
summary(lm(Sc ~ updfreq, pop1))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -13.4931  -2.4960   0.5032   2.5063   8.5077 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 35.47724    0.65125  54.476   <2e-16 ***
#   updfreq      0.03932    1.27155   0.031    0.975    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.98 on 998 degrees of freedom
# Multiple R-squared:  9.584e-07,	Adjusted R-squared:  -0.001001 
# F-statistic: 0.0009564 on 1 and 998 DF,  p-value: 0.9753

summary(lm(Sc ~ updfreq, pop2))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.4433  -2.4561   0.5684   3.5445  11.5747 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  35.2855     0.6641  53.131   <2e-16 ***
#   updfreq       0.2672     1.3049   0.205    0.838    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.997 on 998 degrees of freedom
# Multiple R-squared:  4.202e-05,	Adjusted R-squared:  -0.0009599 
# F-statistic: 0.04193 on 1 and 998 DF,  p-value: 0.8378

summary(lmer(Sc ~ updfreq + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 55713.5
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.5328 -0.6698  0.0925  0.7707  2.8662 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# pop      (Intercept)  0.04751 0.218   
# Residual             15.36268 3.920   
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   35.8526     0.2159  476.0429 166.069   <2e-16 ***
#   updfreq        0.1213     0.3997 9991.7797   0.304    0.761    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# updfreq -0.930


# cond updboth
folder <- 'updboth'
pop1 <- read.csv(file.path(folder, 'popInfoScTheta_pop1.csv'))
pop2 <- read.csv(file.path(folder, 'popInfoScTheta_pop2.csv'))
popRandom <- read.csv(file.path(folder, 'popInfoScTheta_randompop.csv'))

# correlation between theta and Sc
rcorr(as.matrix(pop1[,c("theta", "Sc", "updrate", "updfreq")]), type="pearson")
# theta   Sc updrate updfreq
# theta    1.00 0.91    0.15    0.06
# Sc       0.91 1.00    0.21    0.02
# updrate  0.15 0.21    1.00    0.02
# updfreq  0.06 0.02    0.02    1.00
# 
# n= 1000 

rcorr(as.matrix(pop2[,c("theta", "Sc", "updrate", "updfreq")]), type="pearson")
# theta   Sc updrate updfreq
# theta    1.00 0.75    0.16    0.02
# Sc       0.75 1.00    0.28    0.06
# updrate  0.16 0.28    1.00    0.01
# updfreq  0.02 0.06    0.01    1.00
# 
# n= 1000 

rcorr(as.matrix(popRandom[,c("theta", "Sc", "updrate", "updfreq")]), type="pearson")
# theta    Sc updrate updfreq
# theta    1.00  0.87    0.16   -0.01
# Sc       0.87  1.00    0.22   -0.01
# updrate  0.16  0.22    1.00    0.02
# updfreq -0.01 -0.01    0.02    1.00
# 
# n= 10000 

# theta vs. updrate*updfreq
anova(lm(theta ~ updrate*updfreq, pop1), lm(theta ~ updrate + updfreq, pop1))
# Model 1: theta ~ updrate * updfreq
# Model 2: theta ~ updrate + updfreq
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    996 796.24                           
# 2    997 796.37 -1  -0.13539 0.1694 0.6808
summary(lm(theta ~ updrate + updfreq, pop1))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.3420 -0.3763  0.1975  0.6432  2.2747 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.6526     0.1646  -3.965 7.87e-05 ***
#   updrate       1.2907     0.2774   4.652 3.73e-06 ***
#   updfreq       0.5287     0.2801   1.888   0.0594 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8937 on 997 degrees of freedom
# Multiple R-squared:  0.02501,	Adjusted R-squared:  0.02305 
# F-statistic: 12.79 on 2 and 997 DF,  p-value: 3.284e-06

anova(lm(theta ~ updrate*updfreq, pop2), lm(theta ~ updrate + updfreq, pop2))
# Model 1: theta ~ updrate * updfreq
# Model 2: theta ~ updrate + updfreq
# Res.Df    RSS Df   Sum of Sq  F Pr(>F)
# 1    996 748.67                         
# 2    997 748.67 -1 -1.6721e-05  0 0.9962
summary(lm(theta ~ updrate + updfreq, pop2))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7759 -0.7300  0.1357  0.6743  2.2066 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.5044     0.1653  -3.052  0.00233 ** 
#   updrate       1.3833     0.2760   5.013 6.35e-07 ***
#   updfreq       0.1791     0.2826   0.634  0.52636    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8666 on 997 degrees of freedom
# Multiple R-squared:  0.02505,	Adjusted R-squared:  0.02309 
# F-statistic: 12.81 on 2 and 997 DF,  p-value: 3.221e-06

a <- lmer(theta ~ updrate*updfreq + (1|pop), popRandom,
          control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))) 
b <- lmer(theta ~ updrate + updfreq + (1|pop), popRandom,
          control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6)))
anova(a,b)
# Models:
# b: theta ~ updrate + updfreq + (1 | pop)
# a: theta ~ updrate * updfreq + (1 | pop)
# Df   AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# b  5 25773 25809 -12881    25763                         
# a  6 25774 25818 -12881    25762 0.2237      1     0.6363
summary(lmer(theta ~ updrate + updfreq + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 25776.2
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.1347 -0.5762  0.1924  0.7250  2.7149 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# pop      (Intercept) 0.00     0.0000  
# Residual             0.77     0.8775  
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   -0.37183    0.05232 9997.00000  -7.107 1.26e-12 ***
#   updrate        1.47024    0.08857 9997.00000  16.600  < 2e-16 ***
#   updfreq       -0.13322    0.08919 9997.00000  -1.494    0.135    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) updrat
# updrate -0.489       
# updfreq -0.847 -0.019

# Sc vs. updrate*updfreq
anova(lm(Sc ~ updrate*updfreq, pop1), lm(Sc ~ updrate + updfreq, pop1))
# Model 1: Sc ~ updrate * updfreq
# Model 2: Sc ~ updrate + updfreq
# Res.Df   RSS Df Sum of Sq      F Pr(>F)
# 1    996 25956                           
# 2    997 25963 -1   -7.9366 0.3046 0.5812
summary(lm(Sc ~ updrate + updfreq, pop1))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -31.786  -2.369   1.252   3.519   9.674 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  31.4733     0.9398  33.490  < 2e-16 ***
#   updrate      10.5624     1.5842   6.667 4.31e-11 ***
#   updfreq       0.7199     1.5991   0.450    0.653    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5.103 on 997 degrees of freedom
# Multiple R-squared:  0.043,	Adjusted R-squared:  0.04108 
# F-statistic:  22.4 on 2 and 997 DF,  p-value: 3.051e-10

anova(lm(Sc ~ updrate*updfreq, pop2), lm(Sc ~ updrate + updfreq, pop2))
# Model 1: Sc ~ updrate * updfreq
# Model 2: Sc ~ updrate + updfreq
# Res.Df   RSS Df Sum of Sq      F Pr(>F)
# 1    996 13450                           
# 2    997 13452 -1   -1.5866 0.1175 0.7318
summary(lm(Sc ~ updrate + updfreq, pop2))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -24.9576  -2.2043   0.3636   2.5632   8.9434 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  30.9895     0.7005  44.238   <2e-16 ***
#   updrate      10.5721     1.1698   9.038   <2e-16 ***
#   updfreq       2.0723     1.1977   1.730   0.0839 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.673 on 997 degrees of freedom
# Multiple R-squared:  0.07864,	Adjusted R-squared:  0.07679 
# F-statistic: 42.55 on 2 and 997 DF,  p-value: < 2.2e-16

a <- lmer(Sc ~ updrate*updfreq + (1|pop), popRandom,
          control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))) 
b <- lmer(Sc ~ updrate + updfreq + (1|pop), popRandom,
          control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6)))
anova(a,b)
# Models:
#   b: Sc ~ updrate + updfreq + (1 | pop)
# a: Sc ~ updrate * updfreq + (1 | pop)
# Df   AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
# b  5 58665 58701 -29328    58655                         
# a  6 58666 58710 -29327    58654 0.4606      1     0.4973
summary(lmer(Sc ~ updrate + updfreq + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 58657.3
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -7.1667 -0.5346  0.1829  0.6941  2.3857 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# pop      (Intercept)  0.06929 0.2632  
# Residual             20.62386 4.5414  
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   32.6172     0.2833  677.6398  115.14   <2e-16 ***
#   updrate       10.3990     0.4584 9989.7094   22.68   <2e-16 ***
#   updfreq       -0.8170     0.4616 9989.3081   -1.77   0.0768 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) updrat
# updrate -0.468       
# updfreq -0.809 -0.019


# extreme effects of updfreq
# for folder "updfreq" and "updboth", do IRT at 800 learning
# all analysis
package <- 'mirt'
numPop <- 10
lowL <- 800
set.seed(1234)
for(folder in folderList){
  cat(paste("Processing folder: ", folder, "\n", sep=""))
  direct <- file.path(".", folder)
  IRTData_fixL(direct, package, numPop, lowL) # IRT data
}

# analysis
# cond updfreq
folder <- 'updfreq'
pop1 <- read.csv(file.path(folder, paste("popInfoScTheta_pop1_", lowL, ".csv", sep="")))
pop2 <- read.csv(file.path(folder, paste("popInfoScTheta_pop2_", lowL, ".csv", sep="")))
popRandom <- read.csv(file.path(folder, paste("popInfoScTheta_randompop_", lowL, ".csv", sep="")))

summary(lm(theta ~ updfreq, pop1))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.69479 -0.47790  0.09971  0.59379  2.34983 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.09774    0.13280   0.736    0.462
# updfreq     -0.19459    0.25929  -0.750    0.453
# 
# Residual standard error: 0.8117 on 998 degrees of freedom
# Multiple R-squared:  0.000564,	Adjusted R-squared:  -0.0004374 
# F-statistic: 0.5632 on 1 and 998 DF,  p-value: 0.4531

thres <- 1/10
lowb <- quantile(pop1$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop1$updfreq, 1-thres, na.rm = TRUE)
t.test(pop1$Sc[pop1$updfreq < lowb], pop1$Sc[pop1$updfreq > uppb])
# t = 1.0615, df = 197.26, p-value = 0.2898
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3688583  1.2288583
# sample estimates:
#   mean of x mean of y 
# 30.57     30.14
t.test(pop1$theta[pop1$updfreq < lowb], pop1$theta[pop1$updfreq > uppb])
# t = 0.42239, df = 197.13, p-value = 0.6732
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.1800680  0.2782293
# sample estimates:
#   mean of x  mean of y 
# 0.06546890 0.01638828 

summary(lm(theta ~ updfreq, pop2))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.20005 -0.52014  0.08583  0.59096  2.20594 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -0.08303    0.12918  -0.643    0.521
# updfreq      0.16617    0.25383   0.655    0.513
# 
# Residual standard error: 0.7774 on 998 degrees of freedom
# Multiple R-squared:  0.0004292,	Adjusted R-squared:  -0.0005723 
# F-statistic: 0.4286 on 1 and 998 DF,  p-value: 0.5128

thres <- 1/10
lowb <- quantile(pop2$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop2$updfreq, 1-thres, na.rm = TRUE)
t.test(pop2$Sc[pop2$updfreq < lowb], pop2$Sc[pop2$updfreq > uppb])
# t = 1.7183, df = 197.91, p-value = 0.0873
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.0989154  1.4389154
# sample estimates:
#   mean of x mean of y 
# 31.17     30.50 
t.test(pop2$theta[pop2$updfreq < lowb], pop2$theta[pop2$updfreq > uppb])
# t = 0.41585, df = 196.96, p-value = 0.678
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.1775002  0.2723613
# sample estimates:
#   mean of x   mean of y 
# 0.050124554 0.002693998 

summary(lmer(theta ~ updfreq + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 23785.8
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.4430 -0.6741  0.0386  0.6962  3.2790 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# pop      (Intercept) 0.0000   0.0000  
# Residual             0.6312   0.7945  
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)   -0.04871    0.04146 9998.00000  -1.175    0.240
# updfreq        0.09698    0.08099 9998.00000   1.197    0.231
# 
# Correlation of Fixed Effects:
#   (Intr)
# updfreq -0.981

thres <- 1/10
for(i in 1:numPop){
  lowb <- quantile(popRandom$updfreq[popRandom$pop==i], thres, na.rm = TRUE)
  uppb <- quantile(popRandom$updfreq[popRandom$pop==i], 1-thres, na.rm = TRUE)
  print(t.test(popRandom$Sc[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$Sc[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
  print(t.test(popRandom$theta[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$theta[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
}


# cond updboth
folder <- 'updboth'
pop1 <- read.csv(file.path(folder, paste("popInfoScTheta_pop1_", lowL, ".csv", sep="")))
pop2 <- read.csv(file.path(folder, paste("popInfoScTheta_pop2_", lowL, ".csv", sep="")))
popRandom <- read.csv(file.path(folder, paste("popInfoScTheta_randompop_", lowL, ".csv", sep="")))

summary(lm(theta ~ updrate + updfreq, pop1))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.75441 -0.46061  0.06638  0.53066  1.84446 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.04123    0.13816  -7.536 1.08e-13 ***
#   updrate      3.31524    0.23290  14.234  < 2e-16 ***
#   updfreq      0.09713    0.23510   0.413     0.68    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7502 on 997 degrees of freedom
# Multiple R-squared:  0.1692,	Adjusted R-squared:  0.1676 
# F-statistic: 101.6 on 2 and 997 DF,  p-value: < 2.2e-16

thres <- 1/10
lowb <- quantile(pop1$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop1$updfreq, 1-thres, na.rm = TRUE)
# t.test(pop1$Sc[pop1$updfreq < lowb], pop1$Sc[pop1$updfreq > uppb])
# t = -1.3921, df = 191.06, p-value = 0.1655
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.6434847  0.2834847
# sample estimates:
#   mean of x mean of y 
# 28.60     29.28 
t.test(pop1$theta[pop1$updfreq < lowb], pop1$theta[pop1$updfreq > uppb])
# t = -0.071364, df = 193.26, p-value = 0.9432
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2334827  0.2171765
# sample estimates:
#   mean of x  mean of y 
# 0.02960853 0.03776164 

summary(lm(theta ~ updrate + updfreq, pop2))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.55185 -0.50937 -0.06166  0.45713  2.04562 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.1408     0.1529  -0.921 0.357461    
# updrate       0.9446     0.2553   3.700 0.000228 ***
#   updfreq      -0.2844     0.2614  -1.088 0.276977    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8017 on 997 degrees of freedom
# Multiple R-squared:  0.0146,	Adjusted R-squared:  0.01262 
# F-statistic: 7.383 on 2 and 997 DF,  p-value: 0.000656

thres <- 1/10
lowb <- quantile(pop2$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop2$updfreq, 1-thres, na.rm = TRUE)
t.test(pop2$Sc[pop2$updfreq < lowb], pop2$Sc[pop2$updfreq > uppb])
# t = -0.042773, df = 193.53, p-value = 0.9659
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.9422097  0.9022097
# sample estimates:
#   mean of x mean of y 
# 29.73     29.75 
t.test(pop2$theta[pop2$updfreq < lowb], pop2$theta[pop2$updfreq > uppb])
# t = -0.68117, df = 197.92, p-value = 0.4966
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3020528  0.1469578
# sample estimates:
#   mean of x    mean of y 
# -0.002613168  0.074934314

summary(lmer(theta ~ updrate + updfreq + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 23469
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -4.5899 -0.6903 -0.0168  0.6667  4.1639 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# pop      (Intercept) 0.0000   0.0000  
# Residual             0.6113   0.7819  
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   -0.61022    0.04661 9997.00000 -13.091   <2e-16 ***
#   updrate        1.99048    0.07892 9997.00000  25.222   <2e-16 ***
#   updfreq        0.03242    0.07947 9997.00000   0.408    0.683    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) updrat
# updrate -0.489       
# updfreq -0.847 -0.019

thres <- 1/10
for(i in 1:numPop){
  lowb <- quantile(popRandom$updfreq[popRandom$pop==i], thres, na.rm = TRUE)
  uppb <- quantile(popRandom$updfreq[popRandom$pop==i], 1-thres, na.rm = TRUE)
  print(t.test(popRandom$Sc[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$Sc[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
  print(t.test(popRandom$theta[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$theta[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
}
# no significance


# for folder "updfreq" and "updboth", do IRT at 100 learning
# all analysis
package <- 'mirt'
numPop <- 10
lowL <- 100
set.seed(1234)
for(folder in folderList){
  cat(paste("Processing folder: ", folder, "\n", sep=""))
  direct <- file.path(".", folder)
  IRTData_fixL(direct, package, numPop, lowL) # IRT data
}

# analysis
# cond updfreq
folder <- 'updfreq'
pop1 <- read.csv(file.path(folder, paste("popInfoScTheta_pop1_", lowL, ".csv", sep="")))
pop2 <- read.csv(file.path(folder, paste("popInfoScTheta_pop2_", lowL, ".csv", sep="")))
popRandom <- read.csv(file.path(folder, paste("popInfoScTheta_randompop_", lowL, ".csv", sep="")))

summary(lm(theta ~ updfreq, pop1))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.25960 -0.55562 -0.06766  0.50780  2.15360 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   0.1230     0.1376   0.894    0.372
# updfreq      -0.2448     0.2686  -0.911    0.362
# 
# Residual standard error: 0.8408 on 998 degrees of freedom
# Multiple R-squared:  0.0008315,	Adjusted R-squared:  -0.0001697 
# F-statistic: 0.8305 on 1 and 998 DF,  p-value: 0.3624

thres <- 1/10
lowb <- quantile(pop1$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop1$updfreq, 1-thres, na.rm = TRUE)
t.test(pop1$Sc[pop1$updfreq < lowb], pop1$Sc[pop1$updfreq > uppb])
# t = 0.12699, df = 195.87, p-value = 0.8991
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.726479  0.826479
# sample estimates:
#   mean of x mean of y 
# 13.92     13.87 
t.test(pop1$theta[pop1$updfreq < lowb], pop1$theta[pop1$updfreq > uppb])
# t = 0.9757, df = 196.88, p-value = 0.3304
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.1140155  0.3373135
# sample estimates:
#   mean of x   mean of y 
# 0.06059644 -0.05105257 

summary(lm(theta ~ updfreq, pop2))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.96191 -0.60830 -0.03647  0.57459  2.58530 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  -0.2609     0.1369  -1.905   0.0570 .
# updfreq       0.5222     0.2691   1.941   0.0526 .
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8241 on 998 degrees of freedom
# Multiple R-squared:  0.00376,	Adjusted R-squared:  0.002762 
# F-statistic: 3.767 on 1 and 998 DF,  p-value: 0.05256

thres <- 1/10
lowb <- quantile(pop2$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop2$updfreq, 1-thres, na.rm = TRUE)
t.test(pop2$Sc[pop2$updfreq < lowb], pop2$Sc[pop2$updfreq > uppb])
# t = -0.80168, df = 192.23, p-value = 0.4237
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.9342836  0.3942836
# sample estimates:
#   mean of x mean of y 
# 12.91     13.18 
t.test(pop2$theta[pop2$updfreq < lowb], pop2$theta[pop2$updfreq > uppb])
# t = -2.5435, df = 195.9, p-value = 0.01175
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.53361046 -0.06751266
# sample estimates:
#   mean of x  mean of y 
# -0.1239719  0.1765896 

summary(lmer(theta ~ updfreq + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 24962.9
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.86487 -0.70044 -0.01799  0.65927  2.93069 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# pop      (Intercept) 0.00     0.0000  
# Residual             0.71     0.8426  
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)   -0.01940    0.04397 9998.00000  -0.441    0.659
# updfreq        0.03860    0.08590 9998.00000   0.449    0.653
# 
# Correlation of Fixed Effects:
#   (Intr)
# updfreq -0.981

thres <- 1/10
for(i in 1:numPop){
  lowb <- quantile(popRandom$updfreq[popRandom$pop==i], thres, na.rm = TRUE)
  uppb <- quantile(popRandom$updfreq[popRandom$pop==i], 1-thres, na.rm = TRUE)
  print(t.test(popRandom$Sc[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$Sc[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
  print(t.test(popRandom$theta[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$theta[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
}


# cond updboth
folder <- 'updboth'
pop1 <- read.csv(file.path(folder, paste("popInfoScTheta_pop1_", lowL, ".csv", sep="")))
pop2 <- read.csv(file.path(folder, paste("popInfoScTheta_pop2_", lowL, ".csv", sep="")))
popRandom <- read.csv(file.path(folder, paste("popInfoScTheta_randompop_", lowL, ".csv", sep="")))

summary(lm(theta ~ updrate + updfreq, pop1))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.3439 -0.5508  0.1078  0.5635  2.2952 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  -0.3307     0.1550  -2.133  0.03317 * 
#   updrate       0.7433     0.2613   2.844  0.00454 **
#   updfreq       0.2148     0.2638   0.814  0.41562   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8419 on 997 degrees of freedom
# Multiple R-squared:  0.0088,	Adjusted R-squared:  0.006812 
# F-statistic: 4.426 on 2 and 997 DF,  p-value: 0.0122

thres <- 1/10
lowb <- quantile(pop1$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop1$updfreq, 1-thres, na.rm = TRUE)
t.test(pop1$Sc[pop1$updfreq < lowb], pop1$Sc[pop1$updfreq > uppb])
# t = -0.91975, df = 197.64, p-value = 0.3588
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.4148512  0.5148512
# sample estimates:
#   mean of x mean of y 
# 12.27     12.72 
t.test(pop1$theta[pop1$updfreq < lowb], pop1$theta[pop1$updfreq > uppb])
# t = -0.044022, df = 194.47, p-value = 0.9649
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2588186  0.2475169
# sample estimates:
#   mean of x   mean of y 
# -0.04177586 -0.03612501 

summary(lm(theta ~ updrate + updfreq, pop2))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6053 -0.5489  0.1093  0.6029  2.0919 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.04616    0.15636  -0.295   0.7679    
# updrate      1.05710    0.26110   4.049 5.55e-05 ***
#   updfreq     -0.54072    0.26733  -2.023   0.0434 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8199 on 997 degrees of freedom
# Multiple R-squared:  0.01993,	Adjusted R-squared:  0.01796 
# F-statistic: 10.14 on 2 and 997 DF,  p-value: 4.387e-05

thres <- 1/10
lowb <- quantile(pop2$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop2$updfreq, 1-thres, na.rm = TRUE)
t.test(pop2$Sc[pop2$updfreq < lowb], pop2$Sc[pop2$updfreq > uppb])
# t = 0.11661, df = 197.84, p-value = 0.9073
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.7955874  0.8955874
# sample estimates:
#   mean of x mean of y 
# 11.91     11.86 
t.test(pop2$theta[pop2$updfreq < lowb], pop2$theta[pop2$updfreq > uppb])
# t = 1.9203, df = 197.45, p-value = 0.05627
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.005566365  0.418283490
# sample estimates:
#   mean of x   mean of y 
# 0.02026065 -0.18609791 

summary(lmer(theta ~ updrate + updfreq + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 25007
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.2946 -0.7258  0.0285  0.7400  2.9447 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# pop      (Intercept) 0.000    0.0000  
# Residual             0.713    0.8444  
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   -0.12430    0.05034 9997.00000  -2.469   0.0136 *  
#   updrate        0.70280    0.08523 9997.00000   8.246   <2e-16 ***
#   updfreq       -0.17011    0.08582 9997.00000  -1.982   0.0475 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) updrat
# updrate -0.489       
# updfreq -0.847 -0.019

thres <- 1/10
for(i in 1:numPop){
  lowb <- quantile(popRandom$updfreq[popRandom$pop==i], thres, na.rm = TRUE)
  uppb <- quantile(popRandom$updfreq[popRandom$pop==i], 1-thres, na.rm = TRUE)
  print(t.test(popRandom$Sc[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$Sc[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
  print(t.test(popRandom$theta[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$theta[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
}
# no significance


# for folder "updfreq" and "updboth", do IRT at 200 learning
# all analysis
package <- 'mirt'
numPop <- 10
lowL <- 200
set.seed(1234)
for(folder in folderList){
  cat(paste("Processing folder: ", folder, "\n", sep=""))
  direct <- file.path(".", folder)
  IRTData_fixL(direct, package, numPop, lowL) # IRT data
}

# analysis
# cond updfreq
folder <- 'updfreq'
pop1 <- read.csv(file.path(folder, paste("popInfoScTheta_pop1_", lowL, ".csv", sep="")))
pop2 <- read.csv(file.path(folder, paste("popInfoScTheta_pop2_", lowL, ".csv", sep="")))
popRandom <- read.csv(file.path(folder, paste("popInfoScTheta_randompop_", lowL, ".csv", sep="")))

summary(lm(theta ~ updfreq, pop1))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.2750 -0.5839  0.0084  0.5542  2.2302 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   0.2676     0.1330   2.012   0.0444 *
#   updfreq      -0.5324     0.2596  -2.051   0.0405 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8127 on 998 degrees of freedom
# Multiple R-squared:  0.004196,	Adjusted R-squared:  0.003199 
# F-statistic: 4.206 on 1 and 998 DF,  p-value: 0.04055

thres <- 1/10
lowb <- quantile(pop1$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop1$updfreq, 1-thres, na.rm = TRUE)
t.test(pop1$Sc[pop1$updfreq < lowb], pop1$Sc[pop1$updfreq > uppb])
# t = -0.25334, df = 197.62, p-value = 0.8003
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.7905631  0.6105631
# sample estimates:
#   mean of x mean of y 
# 19.60     19.69
t.test(pop1$theta[pop1$updfreq < lowb], pop1$theta[pop1$updfreq > uppb])
# t = 1.1556, df = 197.61, p-value = 0.2492
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.09593093  0.36752077
# sample estimates:
#   mean of x   mean of y 
# 0.10699691 -0.02879801 

summary(lm(theta ~ updfreq, pop2))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.41138 -0.51142  0.04012  0.59568  1.98602 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   0.1624     0.1418   1.145    0.252
# updfreq      -0.3250     0.2786  -1.167    0.244
# 
# Residual standard error: 0.8532 on 998 degrees of freedom
# Multiple R-squared:  0.001362,	Adjusted R-squared:  0.0003614 
# F-statistic: 1.361 on 1 and 998 DF,  p-value: 0.2436

thres <- 1/10
lowb <- quantile(pop2$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop2$updfreq, 1-thres, na.rm = TRUE)
t.test(pop2$Sc[pop2$updfreq < lowb], pop2$Sc[pop2$updfreq > uppb])
# t = 0.98591, df = 190.37, p-value = 0.3254
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3802603  1.1402603
# sample estimates:
#   mean of x mean of y 
# 19.21     18.83 
t.test(pop2$theta[pop2$updfreq < lowb], pop2$theta[pop2$updfreq > uppb])
# t = 2.2904, df = 197.8, p-value = 0.02305
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.0380422 0.5092586
# sample estimates:
#   mean of x  mean of y 
# 0.1034512 -0.1701992 

summary(lmer(theta ~ updfreq + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 24553.5
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -3.06896 -0.63320  0.04853  0.70158  2.81786 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# pop      (Intercept) 0.0000   0.0000  
# Residual             0.6815   0.8255  
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)    0.05032    0.04290 9998.00000   1.173    0.241
# updfreq       -0.10035    0.08397 9998.00000  -1.195    0.232
# 
# Correlation of Fixed Effects:
#   (Intr)
# updfreq -0.981

thres <- 1/10
for(i in 1:numPop){
  lowb <- quantile(popRandom$updfreq[popRandom$pop==i], thres, na.rm = TRUE)
  uppb <- quantile(popRandom$updfreq[popRandom$pop==i], 1-thres, na.rm = TRUE)
  print(t.test(popRandom$Sc[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$Sc[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
  print(t.test(popRandom$theta[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$theta[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
}


# cond updboth
folder <- 'updboth'
pop1 <- read.csv(file.path(folder, paste("popInfoScTheta_pop1_", lowL, ".csv", sep="")))
pop2 <- read.csv(file.path(folder, paste("popInfoScTheta_pop2_", lowL, ".csv", sep="")))
popRandom <- read.csv(file.path(folder, paste("popInfoScTheta_randompop_", lowL, ".csv", sep="")))

summary(lm(theta ~ updrate + updfreq, pop1))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.26637 -0.59530 -0.03169  0.53404  2.36392 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.2612     0.1504  -1.737   0.0827 .  
# updrate       1.2629     0.2535   4.983 7.39e-07 ***
#   updfreq      -0.2324     0.2559  -0.908   0.3639    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8165 on 997 degrees of freedom
# Multiple R-squared:  0.02492,	Adjusted R-squared:  0.02296 
# F-statistic: 12.74 on 2 and 997 DF,  p-value: 3.446e-06

thres <- 1/10
lowb <- quantile(pop1$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop1$updfreq, 1-thres, na.rm = TRUE)
t.test(pop1$Sc[pop1$updfreq < lowb], pop1$Sc[pop1$updfreq > uppb])
# t = 1.2281, df = 197.86, p-value = 0.2209
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.07932061  0.34118384
# sample estimates:
#   mean of x   mean of y 
# -0.01645826 -0.14738987
t.test(pop1$theta[pop1$updfreq < lowb], pop1$theta[pop1$updfreq > uppb])
# t = 1.2281, df = 197.86, p-value = 0.2209
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.07932061  0.34118384
# sample estimates:
#   mean of x   mean of y 
# -0.01645826 -0.14738987 

summary(lm(theta ~ updrate + updfreq, pop2))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.40879 -0.53036 -0.00058  0.51214  2.87392 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.5759     0.1439  -4.002 6.74e-05 ***
#   updrate       2.6261     0.2403  10.930  < 2e-16 ***
#   updfreq      -0.4223     0.2460  -1.717   0.0863 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7545 on 997 degrees of freedom
# Multiple R-squared:  0.109,	Adjusted R-squared:  0.1072 
# F-statistic: 60.97 on 2 and 997 DF,  p-value: < 2.2e-16

thres <- 1/10
lowb <- quantile(pop2$updfreq, thres, na.rm = TRUE)
uppb <- quantile(pop2$updfreq, 1-thres, na.rm = TRUE)
t.test(pop2$Sc[pop2$updfreq < lowb], pop2$Sc[pop2$updfreq > uppb])
# t = -0.5123, df = 195.54, p-value = 0.609
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.3094079  0.7694079
# sample estimates:
#   mean of x mean of y 
# 17.95     18.22 
t.test(pop2$theta[pop2$updfreq < lowb], pop2$theta[pop2$updfreq > uppb])
# t = 0.85627, df = 196.58, p-value = 0.3929
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.1296384  0.3286022
# sample estimates:
#   mean of x   mean of y 
# 0.02029994 -0.07918200 

summary(lmer(theta ~ updrate + updfreq + (1|pop), popRandom,
             control = lmerControl(optimizer = "nloptwrap", optCtrl=list(maxfun=1e6))))
# REML criterion at convergence: 24148.4
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.4446 -0.7174  0.0219  0.7336  3.7025 
# 
# Random effects:
#   Groups   Name        Variance  Std.Dev. 
# pop      (Intercept) 4.464e-20 2.113e-10
# Residual             6.543e-01 8.089e-01
# Number of obs: 10000, groups:  pop, 10
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   -0.38658    0.04766 9997.00000  -8.111 5.61e-16 ***
#   updrate        1.57925    0.08016 9997.00000  19.701  < 2e-16 ***
#   updfreq       -0.17380    0.08131 9997.00000  -2.137   0.0326 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) updrat
# updrate -0.487       
# updfreq -0.847 -0.021

thres <- 1/10
for(i in 1:numPop){
  lowb <- quantile(popRandom$updfreq[popRandom$pop==i], thres, na.rm = TRUE)
  uppb <- quantile(popRandom$updfreq[popRandom$pop==i], 1-thres, na.rm = TRUE)
  print(t.test(popRandom$Sc[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$Sc[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
  print(t.test(popRandom$theta[(popRandom$updfreq < lowb) & (popRandom$pop==i)], popRandom$theta[(popRandom$updfreq > uppb) & (popRandom$pop==i)]))
}
# no significance


IRTanalysis <- function(direct, com){
  #' IRT analysis of an answer_sheet
  #' @param direct directory of answer sheet file
  #' @param com communication/learning time for testing to take place
  
  answer_sheet <- read.csv(file.path(direct, paste("answer_sheet_", com, ".csv", sep="")), row.names=1, header=TRUE)
  dim(answer_sheet)
  correctness_table <- apply(answer_sheet, 2, table)
  
  all_wrong_items <- getCorrectWrong(answer_sheet, 0)
  all_correct_items <- getCorrectWrong(answer_sheet, 1)
  answer_sheet_new <- answer_sheet[, -which(colnames(answer_sheet) %in% c(all_wrong_items, all_correct_items))]
  
  m1 <- mirt(answer_sheet_new, 1)
  summary(m1)
  coef(m1, IRTpars=TRUE)
  typeof(m1)
  theta1 <- fscores(m1)
  hist(theta1)
  cor(theta1, apply(answer_sheet_new, 1, sum))
  x11()
  plot(theta1, apply(answer_sheet_new, 1, sum))
  
  mod1 <- mirt(answer_sheet_new, 1, SE=TRUE)
  mod2 <- mirt(answer_sheet_new, 1, itemtype = 'Rasch')
  mod3 <- mirt(answer_sheet_new, 2)
  
  itemplot(mod1, 2)
  itemplot(mod1, 2, CE = TRUE)
  itemplot(mod1, 2, type = 'info')
  itemplot(mod1, 2, type = 'info', CE = TRUE)
  
  mods <- list(twoPL = mod1, onePL = mod2)
  itemplot(mods, 1, type = 'RE')
  
  #multidimensional
  itemplot(mod3, 4, type = 'info')
  itemplot(mod3, 4, type = 'infocontour')
  itemplot(mod3, 4, type = 'tracecontour')
  
  #polytomous items
  pmod <- mirt(answer_sheet_new, 1, SE=TRUE)
  itemplot(pmod, 3)
  itemplot(pmod, 3, CE = TRUE)
  itemplot(pmod, 3, type = 'score')
  itemplot(pmod, 3, type = 'infotrace')
  
  # use the directlabels package to put labels on tracelines
  library(directlabels)
  plt <- itemplot(pmod, 3)
  direct.label(plt, 'top.points')
  
  # change colour theme of plots
  bwtheme <- standard.theme("pdf", color=FALSE)
  plot(pmod, type='trace', par.settings=bwtheme)
  itemplot(pmod, 1, type = 'trace', par.settings=bwtheme)
  
  itemplot(pmod, 1, type = 'infoSE')
  
  
  m2 <- mirt(answer_sheet_new, 1, "Rasch")
  coef(m2, IRTpars=TRUE)
  typeof(m2)
  theta2 <- fscores(m2)
  hist(theta2)
  cor(theta2, apply(answer_sheet_new, 1, sum))
  x11()
  plot(theta2, apply(answer_sheet_new, 1, sum))
  
}


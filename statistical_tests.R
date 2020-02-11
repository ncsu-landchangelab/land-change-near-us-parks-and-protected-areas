#---------------------------------------
# Statistical tests
#---------------------------------------

# Used packages
library(gridExtra)
library(ggthemes)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggpubr)
library(reshape2)
library(dgof)                   # Kolmogorov-Smirnov test
library(data.table)

# Used functions
# Friedman function (https://gist.github.com/sithjaisong/59a6496efd6c882d0201e87e0c68351d)
friedman.test.with.post.hoc <- function(formu, data, to.print.friedman = T, to.post.hoc.if.signif = T,  
                                        to.plot.parallel = F, to.plot.boxplot = F, signif.P = .05, color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
{
  # formu is a formula of the shape: 	Y ~ X | block
  # data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor) ]]
  
  # Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, then that entire block should be removed.
  
  
  # Loading needed packages
  if(!require(coin))
  {
    print("You are missing the package 'coin', we will now try to install it...")
    install.packages("coin")
    library(coin)
  }
  
  if(!require(multcomp))
  {
    print("You are missing the package 'multcomp', we will now try to install it...")
    install.packages("multcomp")
    library(multcomp)
  }
  
  if(!require(colorspace))
  {
    print("You are missing the package 'colorspace', we will now try to install it...")
    install.packages("colorspace")
    library(colorspace)
  }
  
  
  # get the names out of the formula
  formu.names <- all.vars(formu)
  Y.name <- formu.names[1]
  X.name <- formu.names[2]
  block.name <- formu.names[3]
  
  if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]	# In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
  
  # Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
  
  # stopping in case there is NA in the Y vector
  if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
  
  # make sure that the number of factors goes with the actual values present in the data:
  data[,X.name ] <- factor(data[,X.name ])
  data[,block.name ] <- factor(data[,block.name ])
  number.of.X.levels <- length(levels(data[,X.name ]))
  if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
  
  # making the object that will hold the friedman test and the other.
  the.sym.test <- symmetry_test(formu, data = data,	### all pairwise comparisons
                                teststat = "max",
                                xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
                                ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
  )
  # if(to.print.friedman) { print(the.sym.test) }
  
  
  if(to.post.hoc.if.signif)
  {
    if(pvalue(the.sym.test) < signif.P)
    {
      # the post hoc test
      The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")	# this is the post hoc of the friedman test
      
      
      # plotting
      if(to.plot.parallel & to.plot.boxplot)	par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
      
      if(to.plot.parallel)
      {
        X.names <- levels(data[, X.name])
        X.for.plot <- seq_along(X.names)
        plot.xlim <- c(.7 , length(X.for.plot)+.3)	# adding some spacing from both sides of the plot
        
        if(color.blocks.in.cor.plot)
        {
          blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
        } else {
          blocks.col <- 1 # black
        }
        
        data2 <- data
        if(jitter.Y.in.cor.plot) {
          data2[,Y.name] <- jitter(data2[,Y.name])
          par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"
        } else {
          par.cor.plot.text <- "Parallel coordinates plot"
        }
        
        # adding a Parallel coordinates plot
        matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
                                  direction="wide")[,-1])  ,
                type = "l",  lty = 1, axes = FALSE, ylab = Y.name,
                xlim = plot.xlim,
                col = blocks.col,
                main = par.cor.plot.text)
        axis(1, at = X.for.plot , labels = X.names) # plot X axis
        axis(2) # plot Y axis
        points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
      }
      
      if(to.plot.boxplot)
      {
        # first we create a function to create a new Y, by substracting different combinations of X levels from each other.
        subtract.a.from.b <- function(a.b , the.data)
        {
          the.data[,a.b[2]] - the.data[,a.b[1]]
        }
        
        temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
                             direction="wide") 	#[,-1]
        wide.data <- as.matrix(t(temp.wide[,-1]))
        colnames(wide.data) <- temp.wide[,1]
        
        Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
        names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
        
        the.ylim <- range(Y.b.minus.a.combos)
        the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))	# adding some space for the labels
        is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
        
        boxplot(Y.b.minus.a.combos,
                names = names.b.minus.a.combos ,
                col = is.signif.color,
                main = "Boxplots (of the differences)",
                ylim = the.ylim
        )
        legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
        abline(h = 0, col = "blue")
        
      }
      
      list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
      if(to.print.friedman) {print(list.to.return)}
      return(list.to.return)
      
    }	else {
      print("The results where not significant, There is no need for a post hoc test")
      return(the.sym.test)
    }
  }
  
  # Original credit (for linking online, to the package that performs the post hoc test) goes to "David Winsemius", see:
  # http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
}

randomSample = function(df,n) { 
  return (df[sample(nrow(df), n),])
}

# Data directory
setwd("Drive:/directory/path")

# Read observed and random data
prot_areas_extent_obs <-read_excel("observed.xlsx", sheet=1)                              # Observed data - housing density and impervious surface
prot_areas_extent_rnd <- read_excel("random.xlsx", sheet=1)                               # Random data - housing density and impervious surface

prot_areas_extent_obs$Group <-"Observed"
prot_areas_extent_rnd$Group <-"Random"

#  Perform Kolmogorov-Smirnov (KS) test
ks.test(prot_areas_extent_obs$HD2Y90, prot_areas_extent_rnd$HD2Y90)
ks.test(prot_areas_extent_obs$HD2Y00, prot_areas_extent_rnd$HD2Y00)
ks.test(prot_areas_extent_obs$HD2Y10, prot_areas_extent_rnd$HD2Y10)
ks.test(prot_areas_extent_obs$PCTIMP, prot_areas_extent_rnd$PCTIMP)
ks.test(prot_areas_extent_obs$PCTIMP01, prot_areas_extent_rnd$PCTIMP01)

combined <-rbind(prot_areas_extent_obs, prot_areas_extent_rnd)
combined_melt <- melt(combined[c("HD2Y90",  "HD2Y00",  "HD2Y10", "PCTIMP", "PCTIMP01", "Group")], id.var = "Group")
combined_mean <- aggregate(list(combined_melt$value), by = list(combined_melt$Group, combined_melt$variable),  mean)
combined_sum <- aggregate(list(combined_melt$value), by = list(combined_melt$Group, combined_melt$variable),  sum)
combined <-rbind(combined_mean, combined_sum)
names(combined)<-c("Group", "variable", "value")
is.num <- sapply(combined, is.numeric)
combined[is.num] <- lapply(combined[is.num], round, 2)
combined$Dist <-10                                                                        # Assign distance

# Friedman test to assess repeated measures of housing density between 1990, 2000, and 2010 of all parks and protected areas
prot_areas_CONUS_extent_obs <-read_excel("observed.xlsx", sheet=1)
prot_areas_CONUS_extent_rnd <- read_excel("random.xlsx", sheet=1)

prot_areas_CONUS_extent_obs$Group <-"Observed"
prot_areas_CONUS_extent_rnd$Group <-"Random"
combined_CONUS <-rbind(prot_areas_CONUS_extent_obs , prot_areas_CONUS_extent_rnd)

combined_CONUS <- combined_CONUS[which(combined_CONUS$HD2Y90 < 600),]                    # Remove outliers
combined_CONUS <- combined_CONUS[which(combined_CONUS$HD2Y00 < 600),]                    # Remove outliers
combined_CONUS <- combined_CONUS[which(combined_CONUS$HD2Y10 < 600),]                    # Remove outliers

#----
# Comparision of housing densities
combined_CONUS_selected_atts <-combined_CONUS[c(4:6, 9:10, 18)]
combined_CONUS_selected_atts_HDs <- combined_CONUS_selected_atts[c("HD2Y90", "HD2Y00", "HD2Y10", "Group")]
com_hd_subset <-subset(combined_CONUS_selected_atts_HDs, Group=="Observed")
com_hd_melt <- melt(com_hd_subset, id.var = "Group")
com_hd_melt$variable <- factor(com_hd_melt$variable, levels = c("HD2Y90", "HD2Y00", "HD2Y10")) # Set the preferred ordering of groups in tables

# Kruskal.test
kruskal.test(value ~ variable, data = com_hd_melt)                                       # Kruskal.test; Since data is not normal and we can not meet ANOVA assumptions
com_hd_subset_sampled <-randomSample(com_hd_subset, 25000)                               # random sample of 25000, 50000, 75000, and 100000 observations

com_hd_subset_sampled$ID <- seq.int(nrow(com_hd_subset_sampled))                         # Add row ID number
com_hd_subset_sampledID <-com_hd_subset_sampled[c("HD2Y90", "HD2Y00",  "HD2Y10", "ID")]
com_hd_subset_sampledID_melt <- melt(com_hd_subset_sampledID, id.var = "ID")

friedman.test.with.post.hoc(value ~ variable | ID , com_hd_subset_sampledID_melt)	     # Friedman's Test

# Housing density data for Wilcox text:
combined_CONUS_HDs <-combined_CONUS[c( "HD2Y90","HD2Y00","HD2Y10")]
combined_CONUS_HDsMelt <-melt(data = combined_CONUS_HDs, measure.vars = c("HD2Y90","HD2Y00","HD2Y10"))

# Remove 0 and NA values:
combined_CONUS_HDsMelt[combined_CONUS_HDsMelt==0] <- NA
sum(is.na(combined_CONUS_HDsMelt$value))
combined_CONUS_HDsMelt<-combined_CONUS_HDsMelt[complete.cases(combined_CONUS_HDsMelt),]

# Compute summary statistics by groups:
group_by(combined_CONUS_HDsMelt, variable) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE)
    )

# Kruskal.test - a nonparametric test
kruskal.test(value ~ variable, data = combined_CONUS_HDsMelt)                            # We know that there is a significant difference between groups, but we don't know which pairs of groups are different.

# Wilcox test
pairwise.wilcox.test(combined_CONUS_HDsMelt$value, combined_CONUS_HDsMelt$variable,  p.adjust.method = "bonferroni")
pairwise.wilcox.test(combined_CONUS_HDsMelt$value, combined_CONUS_HDsMelt$variable,  p.adjust.method = "BH")

#---
# Impervious surfaces
combined_CONUS_IMPs <-combined_CONUS[c("PCTIMP", "PCTIMP01")]
combined_CONUS_IMPsMelt <-melt(data = combined_CONUS_IMPs, measure.vars = c("PCTIMP", "PCTIMP01"))

# Remove 0 and NA values:
combined_CONUS_IMPsMelt[combined_CONUS_IMPsMelt==0] <- NA
sum(is.na(combined_CONUS_IMPsMelt$value))
combined_CONUS_IMPsMelt<-combined_CONUS_IMPsMelt[complete.cases(combined_CONUS_IMPsMelt),]

# Compute summary statistics by groups:
group_by(combined_CONUS_IMPsMelt, variable) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE)
  )

# Wilcox text - a nonparametric test                                                      
wilcox.test(value ~ variable, data = combined_CONUS_IMPsMelt)     

#----
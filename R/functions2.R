#************************************ GENERAL OPTIONS ************************************#
library(ggplot2)
library(stargazer)
library(cem)
library(MatchIt)

##### SET DEFAULT THEME
t <- theme(axis.line = element_line(), panel.background = element_blank(), 
           plot.background = element_blank(), panel.grid.major=element_blank(),
           panel.grid.minor=element_blank(), 
           plot.title=element_text(size=12, face = "bold"),
           axis.text.x = element_text(size = 11, color = "black"), 
           axis.text.y = element_text(size = 11, color = "black"),
           axis.title.x = element_text(size = 12, color = "black", face = "bold"),
           axis.title.y = element_text(size = 12, color = "black", face = "bold"),
           legend.title=element_blank(),
           legend.text=element_text(size = 10),
           legend.key = element_blank())

options("cr-counter"=0)

#************************************ CREATE CDATA OBJECT ***************************************#

#' Load data.
#'
#' This function allows you to load your data into 
#' @param out A vector of outcomes
#' @param tx A numeric vector indicating whether a subject is "treatment" (1) or "control" (0)
#' @param cov A data frame of covariates
#' @param cov_type Vector indicating covariate types, 1=continuous, 2=ordinal, 3=categorical, defaults to continuous
#' @param family Distribution of outcome variable, defaults to "gaussian"
#' @param design Study design, defaults to "cohort'
#' @keywords load
#' @export
#' @examples
#' cdata()
cdata <- function(out, tx, cov = NULL, cov_type = c(1),
                        family = "gaussian", design = "cohort"){
  
  # put together in a data frame
  df <- data.frame(outcome = out, tx = tx, cov)
  
  # options
  ops <- c(family, design)
  type <- cov_type
  
  return(list(df=df, ops=ops, cov_type=cov_type))
}

#************************************ DESCRIPTIVE PLOTS ***************************************#

#' Explore outcome distributions
#'
#' This function allows you to explore overlap in the distribution of outcomes by treatment and control
#' @param cdata An object created with the cdata command
#' @param col The color of your histogram bars, defaults to "purple4"
#' @param col_line The color of your lines, defaults to "#d3d3d3"
#' @param bin_no The number of histogram bins, defaults to 30
#' @param theme Ggplot theme
#' @export
#' @examples
#' des_out()
des_out <- function(cdata, bin_no=30, col_bar= "purple4", col_line="#d3d3d3", quints = TRUE, theme = t) {
  
  # extract data
  df <- cdata[[1]]
  ops <- cdata[[2]]
  
  # FOR CONTINUOUS OUTCOME
  if(ops[1] == "gaussian") {
    
    ## MAKE GRAPH
    g <- ggplot(df, aes(x = outcome, y = ..density..)) + geom_histogram(fill = col_bar, bins = bin_no) + t + 
      facet_grid(tx~., scale="free_y") + labs(title = "Outcome distributions")
    
    ## OUTPUT TEXT
    text <- "Examine the distribution of your outcome in the treatment and control variables.  If there appear to be outliers, you may want to consider how those will affect the analysis."
  }
  
  # FOR BINARY OUTCOME -- UNDER CONSTRUCTION
  if(ops[1] == "logistic") {
    g <- ggplot(df, aes(x = tx, y = outcome)) + geom_point() + stat_smooth() + t
  }
  
  # RETURN DATA
  if (quints) return(list(g, text)) 
  if (!quints) return(list(g, text))
}

#' Explore covariates
#'
#' This function allows you to explore overlap and imbalance in outcomes by covariates
#' @param cdata An object created with the cdata command
#' @param covs A vector of covariate names to include, defaults to all
#' @param col_bar The color of your histogram bars, defaults to "purple4"
#' @param col_line The color of your lines, defaults to "#d3d3d3"
#' @param bin_no The number of histogram bins, defaults to 30
#' @param quints Adds quintiles to plots, 1 for control, 2 for treatment, defaults to none 0
#' @param theme Ggplot theme
#' @examples
#' des_covs()
des_covs <- function(cdata, covs = c(1), bin_no=30, quints = 0, col_bar= "purple4", col_line="#d3d3d3", 
                     theme = theme_bw()){
  ## Set up empty list
  out <- list()
  
  ## Set up covariate names to pull
  df <- cdata$df
  cov_types <- cdata$cov_type
  
  ## Make happy graphs
  for(i in 1:length(cov_types)) {
    if(cov_types[i]==1) {
      g <- ggplot(df, aes_string(colnames(df)[i+2], "..density..")) + 
        geom_histogram(fill = col_bar, bins = bin_no) + theme + 
        facet_grid(tx~., scale="free_y") +
        labs(title = "Covariate distributions")
      
      if (quints == 1) g <- geom_vline(xintercept = quantile(df$out[df$tx==0], c(seq(.2, .8, by=.2))))
      if (quints == 2) g <- geom_vline(xintercept = quantile(df$out[df$tx==1], c(seq(.2, .8, by=.2))))
    }                                                            
    if(cov_types[i]!=1) {
      g <- ggplot(df, aes_string(colnames(df)[i+2])) + 
        geom_bar(fill = col_bar) + theme + 
        facet_grid(tx~., scale="free_y") + labs(title = "Covariate distributions")
    }
    
    out[[i]] <- g
    }
  
  out[[length(cov_types)+1]] <- "[Use arrows in R studio to view multiple plots.] 1) The distribution of covariates variable in the treatment and control groups should overlap.  If they don't, you cannot impute missing potential outcomes in regions in which they do not overlap.  If there are areas of one distribution for which there are few or no observations of the other, you may want to trim these.  2) Distributions of covariates in treatment and control should also be balanced.  If they are not, you may want to adjust for them in regression or consider matching methods."
  
  return(out)
}

#************************************ REGRESSION ***************************************#

#' Examine regression diagnostics
#'
#' @param cdata An object created with the cdata command
#' @param covs A vector of covariate names to include, defaults to all
#' @param trim A logical vector used to trim the data set, defaults to keeping the entire dataset
#' @export
#' @examples
#' reg_diag()
reg_diag <- function(cdata, covs=c(1), trim = c(NA)){
  
  # data frame
  df <- cdata$df
  if (covs != 1) df <- df[c("outcome", "tx", covs)]
  if (length(trim)>1) df <- df[trim,]
  
  # run regression
  mod <- lm(outcome~., data=df)
  
  # make da plots
  resid <- ggplot(mod, aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth(se = FALSE) + theme_bw()
  
  stdresid <- ggplot(mod, aes(.fitted, .stdresid)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth(se = FALSE) + theme_bw()
  
  qq <- ggplot(mod) +
    stat_qq(aes(sample = .stdresid)) +
    geom_abline() + theme_bw()
  
  cooksd <- ggplot(mod, aes(.hat, .stdresid)) +
    geom_point(aes(size = .cooksd)) +
    geom_smooth(se = FALSE, size = 0.5) + theme_bw()
  
  text <- "[Use arrows in R studio to view multiple plots.]  Use these plots to investigate key OLS assumptions.  Residual plots should show residuals evenly scattered around 0.  Check for a fan shape indicating lack of homoscedasticity.  The QQ plot provides a measure of normality of residuals.  Cook's D can allow you to identify and investigate high leverage points.  Ensure a good fit before viewing model results."
  # return them
  return(list(resid, stdresid, qq, cooksd, text))
}

#' Print multiple regression results
#'
#' @param cdata An object created with the cdata command
#' @export
#' @examples
#' cause_reg()
cause_reg <- function(cdata, covs = (1), digits = 2){
  
  # data frame
  df <- cdata$df
  if (covs != 1) df <- df[c("outcome", "tx", covs)]
  
  # run regression
  mod <- lm(outcome~., data=df)
  
  # counter
  options("cr-counter"=1+getOption("cr-counter"))
  text <- paste("You have run", getOption("cr-counter"), "regression(s).")
  
  # make graph
  v <- stargazer(mod, type = "text", ci = TRUE, digits = 2)
  
  return(list(text, mod))
  
}

#************************************ MATCHING ***************************************#

# Prepare to generate outputs related to matching
match.covars <- function(n.covars = 0){
  
  if (n.covars == 0){n.covars = (ncol(h$df)-2)}
  
  # Set up difference in means, L1, and regression results
  matchbal = data.frame("all"=rep(NA, n.covars),"trimmed"=NA,"pscore"=NA,"mahalanobis"=NA)
  matchimbal = data.frame("all"=NA,"trimmed"=NA,"pscore"=NA,"mahalanobis"=NA)
  matchreg = data.frame("all"=NA,"trimmed"=NA,"pscore"=NA,"mahalanobis"=NA)
  rownames(matchbal) <- colnames(h$df)[1:n.covars + 2]
  matchbal$all <- apply(as.matrix(h$df[h$df$tx==1, 3:(n.covars+2)]), 2, mean) - apply(as.matrix(h$df[h$df$tx==0, 3:(n.covars+2)]), 2, mean)
  matchimbal$all <- list(cem::imbalance(group=h$df$tx, data=h$df[ , 1:n.covars+2], drop=c("outcome", "tx")))
  matchreg$all <- list(lm(outcome ~ tx+sex+antih+barb+chemo+cage+cigar+lgest+lmotage+lpbc415+lpbc420+motht+motwt+mbirth+psydrug+respir+ses+sib, data = h$df))
  
  # Calculate propensity scores are trim both treatment groups
  pscores.logit <- glm(tx ~ sex+antih+barb+chemo+cage+cigar+lgest+lmotage+lpbc415+lpbc420+motht+motwt+mbirth+psydrug+respir+ses+sib, family = "binomial", data = h$df)$fitted
  pscore.treat <- pscores.logit[h$df$tx==1]
  pscore.control <- pscores.logit[h$df$tx==0]
  common.support <- c(max(min(pscore.treat), min(pscore.control)), min(max(pscore.treat), max(pscore.control)))
  common.df <- h$df[(pscores.logit >= common.support[1]) & (pscores.logit <= common.support[2]), ]
  matchbal$trimmed <- apply(as.matrix(common.df[common.df$tx==1, 3:(n.covars+2)]), 2, mean) - apply(as.matrix(common.df[common.df$tx==0, 3:(n.covars+2)]), 2, mean)
  matchimbal$trimmed <- list(cem::imbalance(group=common.df$tx, data=common.df[ , 1:(n.covars+2)], drop=c("outcome", "tx")))
  matchreg$trimmed <- list(lm(outcome ~ tx+sex+antih+barb+chemo+cage+cigar+lgest+lmotage+lpbc415+lpbc420+motht+motwt+mbirth+psydrug+respir+ses+sib, data = common.df))
  
  # Output comparison histogram
  png("PScore.png", height=8, width=10, units = "in", res = 300)
  par(mfrow = c(1, 2))
  hist(pscore.treat[(pscore.treat >= common.support[1]) & (pscore.treat <= common.support[2])], col = "red", breaks = 10, main = "Histogram of Trimmed\nData Propensity Scores:\nActive Treatment Group", xlab = "Propensity Score")
  hist(pscore.control[(pscore.control >= common.support[1]) & (pscore.control <= common.support[2])], col = "blue", breaks = 10, main = "Histogram of Trimmed\nData Propensity Scores:\nControl Group", xlab = "Propensity Score")
  par(mfrow = c(1, 1))
  dev.off()
  
  # Nearest neighbor matching on propensity score
  pscore.match <- matchit(tx ~ sex+antih+barb+chemo+cage+cigar+lgest+lmotage+lpbc415+lpbc420+motht+motwt+mbirth+psydrug+respir+ses+sib, data = h$df, method = "nearest", distance = "logit", discard="control")
  pscore.data <- match.data(pscore.match)
  matchbal$pscore <- apply(as.matrix(pscore.data[pscore.data$tx==1, 3:(n.covars+2)]), 2, mean) - apply(as.matrix(pscore.data[pscore.data$tx==0, 3:(n.covars+2)]), 2, mean)
  matchimbal$pscore <- list(cem::imbalance(group=pscore.data$tx, data=pscore.data[ , 1:(n.covars+2)], drop=c("outcome", "tx")))
  matchreg$pscore <- list(lm(outcome ~ tx+sex+antih+barb+chemo+cage+cigar+lgest+lmotage+lpbc415+lpbc420+motht+motwt+mbirth+psydrug+respir+ses+sib, data = pscore.data))
  
  # Mahalanobis distance nearest neighbor matching
  mahalanobis.match <- matchit(tx ~ sex+antih+barb+chemo+cage+cigar+lgest+lmotage+lpbc415+lpbc420+motht+motwt+mbirth+psydrug+respir+ses+sib, data = h$df, method = "nearest", distance = "mahalanobis", discard="control")
  mahalanobis.data <- match.data(mahalanobis.match)
  matchbal$mahalanobis <- apply(as.matrix(mahalanobis.data[mahalanobis.data$tx==1, 3:(n.covars+2)]), 2, mean) - apply(as.matrix(mahalanobis.data[mahalanobis.data$tx==0, 3:(n.covars+2)]), 2, mean)
  matchimbal$mahalanobis <- list(cem::imbalance(group=mahalanobis.data$tx, data=mahalanobis.data[ , 1:(n.covars+2)], drop=c("outcome", "tx")))
  matchreg$mahalanobis <- list(lm(outcome ~ tx+sex+antih+barb+chemo+cage+cigar+lgest+lmotage+lpbc415+lpbc420+motht+motwt+mbirth+psydrug+respir+ses+sib, data = mahalanobis.data))
  
  # Create comparison plot
  png(paste("Balcomp", n.covars, ".png", sep=""), height=8, width=10, units = "in", res = 300)
  plot(1, 0, type = "n", main = "Covariate Balance Comparison by Method \n (Treated Mean Minus Control Mean)", ylab = "Difference in Means",
       xlab = "               All Data            Trimmed by PScore     Matched on PScore    Mahalanobis Matched", xlim = c(0, 5), ylim = c(min(matchbal), max(matchbal)))
  for (x in 1:n.covars){
    lines(1:4, matchbal[x, 1:4], type = "b", pch = 20)
    text(x=0.75*runif(1), y=as.numeric(matchbal[x, 1]), colnames(h$df)[x+2])
    text(x=4.25+0.75*runif(1), y=as.numeric(matchbal[x, 4]), colnames(h$df)[x+2])}
  abline(h=0, lty = 2, col = "red")
  dev.off()
  
  # Output is a three-element list: difference in means, L1, and regression results  
  return(list("matchbal"=matchbal, "matchimbal"=matchimbal, "matchreg"=matchreg))}

#' Generate match output
#'
#' @param cdata An object created with the cdata command
#' @export
#' @examples
#' match.compare()
match.compare <- function(h){
  
  # Compute outputs
  newmatch <- lapply(2:(ncol(h$df)-2), function (x) match.covars(x))
  # This also generates the PScore histograms and balance comparison graphs as .png files
  
  # Compare imbalance as a table
  imbalance.chart <- data.frame("n.covar"=2:(ncol(h$df)-2), "all"=NA, "trimmed"=NA, "pscore"=NA, "mahalanobis"=NA)
  for (i in 1:(ncol(h$df)-3)){
    imbalance.chart[i, ] <- c(i+1, sapply(1:4, function(x) {newmatch[[i]]$matchimbal[[x]][[1]]$L1$L1}))}
  print(round(imbalance.chart, 2))
  
  # Compare treatment effect as a table
  ate.chart <- rbind(t(sapply(1:4, function (x) summary(newmatch[[16]]$matchreg[[x]][[1]])$coef["tx", ])))
  print(round(ate.chart, 3))
  }

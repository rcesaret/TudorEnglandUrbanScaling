
### Data Replication File for Cesaretti et al. "Increasing Returns to Scale in the Provincial Towns of Tudor England" ########
#
# Last updated: 2019-08-09
#
# Please direct any questions and comments to Rudolf Cesaretti ( rcesaret@asu.edu )
# 
# Analyses in this 'Data Replication' script were performed in R versions 3.4.3 (2017-11-30)
# and 3.6.0 (2019-04-26). R-Studio is ideal for viewing and manipulating this file. 
# 
# The subsections of this script, indicated by triple-pound-sign comment lines, specify which subsections they correspond
# to in the main text (e.g. "4.1 Data Quality") and in the online Appendix (e.g. "A1.1 Distributional Analysis")
#
# The additional R packages required in each section are indicated at the top of each section.
# To download the requisite packages to your library, simply un-comment-out the function "install.packages()", and make
# sure you have a secure connection to the internet.
#
# This file requires that the working-directory be set to a local folder that includes "main_dataset.csv",
# "add_taxpayers.csv", "WCS.csv", "resid_coords_geog.csv", and "TudorCounties.csv" which are included in the
# online supplementary materials
#
# Commands to export figures (as .tif files) and tables (as .csv files) have also been commented-out


########################################## Preliminaries ############################################

# clear workspace
rm(list = ls())

# set working directory to local folder containing .csv files included in the supplementary materials 
setwd("C:/Users/TJ McMote/Desktop/Submission/Resubmission") #C:/Users/TJ McMote/Desktop/Submission/Resubmission

# Upload datasets from working directory
main_dataset <- read.csv(file="main_dataset.csv", header=TRUE, sep=",")
add_taxpayers <- read.csv(file="add_taxpayers.csv", header=TRUE, sep=",")
WCS <- read.csv(file="WCS.csv", header=TRUE, sep=",")
resid_coords_geog <- read.csv("resid_coords_geog.csv", header = T)
TC <- read.csv(file="TudorCounties.csv", header=TRUE, sep=",")

#####################################################################################################
########### Analysis and Figures from 4.1 Data Quality and A1.1 Distributional Analysis ###########
#####################################################################################################

#install.packages("MASS")
#install.packages("NSM3")
library(MASS)
library(NSM3)


#### Figure A1: Histograms of the Lay Subsidy data (n = 93) ####

#tiff(filename = "Fig_A1_Hists.tif", width = 9, height = 4, units = "in", res=300,  compression = "lzw", bg = "white", family = "", restoreConsole = TRUE, type ="windows")
par(mfrow = c(1, 2))

# Figure A1a
FigA1a <- hist(main_dataset$Log_Tax, breaks=seq(2,7,0.2), plot = FALSE)
pos <- pretty(FigA1a$density, n = 6)
freq <- round(pos * length(main_dataset$Log_Tax) * with(FigA1a, breaks[2] - breaks[1]))
new.mai <- old.mai <- par("mai")
new.mai[4] <- old.mai[2]
par(mai = new.mai)
graphics:::plot.histogram(FigA1a, freq = FALSE, col="gray70", family="sans", main="(a)", cex.main=2, 
                          font.main=2, font.lab=2, xlab = "Log Tax (£)", ylab="Frequency",
                          border="black", yaxt='n')
Axis(side = 2, at = pos, labels = freq)
Axis(side = 4, at = pos, labels = pos)
mtext("Density           ", side = 4, line = 3, family="sans", font=2)
polygon(density(main_dataset$Log_Tax), col = rgb(0, 0, 1, 0.3))
fit<-fitdistr(main_dataset$Log_Tax,"normal")$estimate
xfit<-seq((min(main_dataset$Log_Tax)-1),(max(main_dataset$Log_Tax)+1),length=50) 
yfit<-dnorm(xfit,fit[1],fit[2])
lines(xfit, yfit, col="red", lwd=2)

# Figure A1b
FigA1b <- hist(main_dataset$Log_Taxpayers, breaks=seq(4.5,7.5,0.2), plot = FALSE)
pos <- pretty(FigA1b$density, n = 6)
freq <- round(pos * length(main_dataset$Log_Taxpayers) * with(FigA1b, breaks[2] - breaks[1]))
new.mai <- old.mai <- par("mai")
new.mai[4] <- old.mai[2]
par(mai = new.mai)
graphics:::plot.histogram(FigA1b, freq = FALSE, col="gray70", family="sans", main="(b)", cex.main=2, 
                          font.main=2, font.lab=2, xlab = "Log Taxpayers", ylab="          Frequency",
                          border="black", yaxt='n')
Axis(side = 2, at = pos, labels = freq)
Axis(side = 4, at = pos, labels = pos)
mtext("Density", side = 4, line = 3, family="sans", font=2)
polygon(density(main_dataset$Log_Taxpayers), col = rgb(0, 0, 1, 0.3))
fit<-fitdistr(main_dataset$Log_Taxpayers,"normal")$estimate
xfit<-seq((min(main_dataset$Log_Taxpayers)-2),(max(main_dataset$Log_Taxpayers)+1),length=50) 
yfit<-dnorm(xfit,fit[1],fit[2])
lines(xfit, yfit, col="red", lwd=2)

# reset graphics
par(mai = old.mai)
#dev.off()
par(mfrow = c(1, 1))



#### Kolmogorov-Smirnov test for Log Tax data (n = 93) ####

fit<-fitdistr(main_dataset$Log_Tax,"normal")$estimate
ks.test(main_dataset$Log_Tax, "pnorm",fit[1],fit[2])


#### Kolmogorov-Smirnov test for Log Taxpayers data (n = 93) ####

fit<-fitdistr(main_dataset$Log_Taxpayers,"normal")$estimate
ks.test(main_dataset$Log_Taxpayers, "pnorm",fit[1],fit[2])


#### Figure A2: Kolmogorov-Smirnov Test Plots with 95% C.I.s ####

#tiff(filename = "Fig_A2_KS.tif", width = 8, height = 4, units = "in", res=300,  compression = "lzw", bg = "white", family = "", restoreConsole = TRUE, type ="windows")
par(mfrow = c(1, 2))

#Figure A2a
ecdf.ks.CI(main_dataset$Log_Tax, family="sans", main= "(a)", cex.main=2, font.main=2, font.lab=2, xlab="Log Tax (£)", ylab="ECDF(Log Tax)")
fit<-fitdistr(main_dataset$Log_Tax,"normal")$estimate
Norm1 <- rnorm(10000, fit[1],fit[2])
lines(ecdf(Norm1), do.points = FALSE, verticals=T, lwd=2, col="blue")

#Figure A2b
ecdf.ks.CI(main_dataset$Log_Taxpayers, main= "(b)", cex.main=2, font.main=2, font.lab=2, xlab="Log Taxpayers", ylab="ECDF(Log Taxpayers)")
fit<-fitdistr(main_dataset$Log_Taxpayers,"normal")$estimate
Norm2 <- rnorm(10000, fit[1],fit[2])
lines(ecdf(Norm2), do.points = FALSE, verticals=T, lwd=2, col="blue")

# reset graphics
#dev.off()
par(mfrow = c(1, 1))



#### Figure A3: Zipfian Tail of Taxpayer Count Log-Log Rank-Size Plot ####

#tiff(filename = "Fig_A3_Zipf.tif", width = 5, height = 5, units = "in", res=300,  compression = "lzw", bg = "white", family = "", restoreConsole = TRUE, type ="windows")
plot(main_dataset$Log_Rank, main_dataset$Log_Taxpayers, main= "Zipfian Taxpayers Tail", cex.main=2, pch=16, font.main=2, font.lab=2, xlab="Log Rank", ylab="Log Taxpayers")
abline(a=max(main_dataset$Log_Taxpayers), b=-0.35, col="red", lwd=2.5)
text(3, 7.15, "Ln(Taxpayers) = 7.27 - 0.35(Ln(Rank))", cex = 0.8, font=2)
#dev.off()


#### Kolmogorov-Smirnov test for Log Taxpayers (n = 134) using expanded sample of towns with at least 50 taxpayers ####

fit<-fitdistr(add_taxpayers$Log_Taxpayers,"normal")$estimate
ks.test(add_taxpayers$Log_Taxpayers, "pnorm",fit[1],fit[2])


#### Figure A4: Analysis of the Expanded 1524/5 Lay Subsidy Taxpayer Counts Dataset (expanded sample of towns with at least 50 taxpayers) ####

#tiff(filename = "Fig_A4_HistKS.tif", width = 8.5, height = 4, units = "in", res=300,  compression = "lzw", bg = "white", family = "", restoreConsole = TRUE, type ="windows")
par(mfrow = c(1, 2))

# Figure A4a
FigA4a <- hist(add_taxpayers$Log_Taxpayers, breaks=seq(3,8,0.2), plot = FALSE)
pos <- pretty(FigA4a$density, n = 6)
freq <- round(pos * length(add_taxpayers$Log_Taxpayers) * with(FigA4a, breaks[2] - breaks[1]))
new.mai <- old.mai <- par("mai")
new.mai[4] <- old.mai[2]
par(mai = new.mai)
graphics:::plot.histogram(FigA4a, freq = FALSE, col="gray70", family="sans", main="(a)", cex.main=2, 
                          font.main=2, font.lab=2, xlab = "Log Taxpayers", ylab="Frequency",
                          border="black", yaxt='n')
Axis(side = 2, at = pos, labels = freq)
Axis(side = 4, at = pos, labels = pos)
mtext("Density           ", side = 4, line = 3, family="sans", font=2)
polygon(density(add_taxpayers$Log_Taxpayers), col = rgb(0, 0, 1, 0.3))
fit<-fitdistr(add_taxpayers$Log_Taxpayers,"normal")$estimate
xfit<-seq((min(add_taxpayers$Log_Taxpayers)-1),(max(add_taxpayers$Log_Taxpayers)+1),length=50) 
yfit<-dnorm(xfit,fit[1],fit[2])
lines(xfit, yfit, col="red", lwd=2)
par(mai = old.mai)

# Figure A4b
ecdf.ks.CI(add_taxpayers$Log_Taxpayers, main= "(b)", cex.main=2, font.main=2, font.lab=2, xlab="Log Taxpayers", ylab="ECDF(Log Taxpayers)")
fit<-fitdistr(add_taxpayers$Log_Taxpayers,"normal")$estimate
Norm3 <- rnorm(10000, fit[1],fit[2])
lines(ecdf(Norm3), do.points = FALSE, verticals=T, lwd=2, col="blue")

# reset graphics
#dev.off()
par(mai = old.mai)
par(mfrow = c(1, 1))


#####################################################################################################
#################### Analysis and Figures from 5.1. Results by Error-Type Subset ####################
#####################################################################################################

library(zoo)
library(lmtest)
library(sandwich)
library(ggplot2)
library(devtools)
library(broom)
library(plyr)
library(MASS)
library(NSM3)
#install.packages("zoo")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("ggplot2")
#install.packages("devtools")
#install.packages("broom")
#install.packages("plyr")
#install.packages("MASS")
#install.packages("NSM3")



#### Regreesion Results Table for all Error-Type Subsets ####

out <- data.frame()
for (i in 1:4){
		EE = matrix( c(2, 2, 2, 1, 1, 2, 1, 1), nrow=2, ncol=4)
		x.sub <- subset(main_dataset, Problem < EE[1,i] & Rural < EE[2,i])
		x <- x.sub$Log_Taxpayers
		y <- x.sub$Log_Tax
		fm <- lm(y ~ x)
		tidy_fm <- tidy(fm)
		coeftest <- coeftest(fm, vcov = vcovHC(fm, "HC1"))
		tidy_coeftest <- tidy(coeftest)
		tidy_coeftest$term[1] <- "alpha"
		tidy_coeftest$term[2] <- "beta"
		tidy_coeftest$param <- c(1, 2)
		tidy_coeftest$estimate[1] <- exp(tidy_coeftest$estimate[1])
		if (EE[1,i] == 2 & EE[2,i] == 2){
			tidy_coeftest$analysis <- c("All", "All")
			tidy_coeftest$analysis_num <- c(1, 1)
		}
		if (EE[1,i] == 2 & EE[2,i] == 1){
			tidy_coeftest$analysis <- c("No Rural", "No Rural")
			tidy_coeftest$analysis_num <- c(2, 2)
		}
		if (EE[1,i] == 1 & EE[2,i] == 2){
			tidy_coeftest$analysis <- c("No Problem", "No Problem")
			tidy_coeftest$analysis_num <- c(3, 3)
		}
		if (EE[1,i] == 1 & EE[2,i] == 1){
			tidy_coeftest$analysis <- c("No Rural, No Problem", "No Rural, No Problem")
			tidy_coeftest$analysis_num <- c(4, 4)
		}
		tidy_coeftest$n <- c(length(x), length(x))
		tidy_coeftest$rsquared <- c(summary(fm)$r.squared, summary(fm)$r.squared)
		#tidy_coeftest$dataset_num <- c(1, 1)
		
		#set HC1 covariance matrix as an object
		Cov<-vcovHC(fm, "HC1")

		#calculate and report 95% confidence interval using HC1 covariance matrix
		tt <-qt(c(0.025,0.975),summary(fm)$df[2])
		se <- sqrt(diag(Cov))
		ci <-coef(fm) + se %o% tt

		tidy_coeftest$ci0.025 <- c(exp(ci[1,1]), ci[2,1])
		tidy_coeftest$ci0.975 <- c(exp(ci[1,2]), ci[2,2])
		out=rbind(out,tidy_coeftest)
}

## Table (dataframe) of unsorted results
View(out)

analysis_num <- out$analysis_num[c(TRUE, FALSE)]
Subset <- out$analysis[c(TRUE, FALSE)]
n <- out$n[c(TRUE, FALSE)]
R_squared <- out$rsquared[c(TRUE, FALSE)]
alpha_estimate <- out$estimate[c(TRUE, FALSE)]
alpha_ci_0.025 <- out$ci0.025[c(TRUE, FALSE)]
alpha_ci_0.975 <- out$ci0.975[c(TRUE, FALSE)]
beta_estimate <- out$estimate[c(FALSE, TRUE)]
beta_ci_0.025 <- out$ci0.025[c(FALSE, TRUE)]
beta_ci_0.975 <- out$ci0.975[c(FALSE, TRUE)]



#### Table 1 (dataframe): Scaling of 1524/5 Tax Paid with Taxpayer Count (in "5.1. Results by Error-Type Subset" from Main Text) ####

Table1 <- data.frame(analysis_num, Subset, n, R_squared, beta_estimate, beta_ci_0.025, beta_ci_0.975, alpha_estimate, alpha_ci_0.025, alpha_ci_0.975)
View(Table1)
#write.csv(Table1, file = "Scaling_Results_Table_Output.csv")

#### Figure 1. Log-linear Regression Plot of 1524/5 Tax ~ Taxpayer scaling relation for all towns (n = 93) ####

#tiff(filename = "Fig_1_ScalingPlot.tif", width = 6, height = 6, units = "in", res=300,  compression = "lzw", bg = "white", family = "", restoreConsole = TRUE, type ="windows")

ggplot(main_dataset, aes(x=main_dataset$Log_Taxpayers, y=main_dataset$Log_Tax)) +
geom_point(size=3, aes( colour = factor(main_dataset$GroupN), shape = factor(main_dataset$GroupN))) + 
scale_color_manual(name ="Error Type Subsets",values=c("grey60", "black","grey40", "grey50")) +
scale_shape_manual(name ="Error Type Subsets",values=c(18, 16, 15, 17)) +
geom_smooth(fullrange=TRUE, method =lm, se=TRUE, colour="red", size=1) +
scale_x_continuous(expand=c(0,0), limits=c(4.5,10)) +
scale_y_continuous(expand=c(0,0), limits=c(2,10)) +
coord_cartesian(xlim=c(4.5,8), ylim=c(2,7.5)) +
geom_abline(mapping = NULL, data = NULL, slope = 1.2695, intercept = -2.982366, na.rm = FALSE, show.legend = NA, colour="red", size=1) + 
geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = -1.4, na.rm = FALSE, show.legend = NA, colour="black", size=1) +
geom_abline(mapping = NULL, data = NULL, slope = 1.15, intercept = -2.28, na.rm = FALSE, show.legend = NA, colour="gold1", size=1) +
xlab("Log Taxpayers, 1524/5") +
ylab("Log Tax (£), 1524/5") +
theme_bw() +
theme(legend.justification=c(1,0), legend.position=c(0.9,0.1)) +
theme(legend.box.background = element_rect(colour = "black"), panel.border = element_rect(colour = "black", fill=NA), legend.background = element_blank()) +
theme(axis.text = element_text(size = 12, color="black")) +
theme(axis.title.y = element_text(size = 14, face = "bold")) +
theme(axis.title.x = element_text(size = 14, face = "bold")) +
theme(legend.title = element_text(face="bold"))

#dev.off()




############## Gaussian Residuals Cross-Checks #################


##### Residuals Tests for All Cases (n = 93) ######

fm <- lm(main_dataset$Log_Tax ~ main_dataset$Log_Taxpayers)
fm.resid <- resid(fm)

shapiro.test(fm.resid)

fit<-fitdistr(fm.resid,"normal")$estimate
ks.test(fm.resid, "pnorm",fit[1],fit[2])

#tiff(filename = "Residuals_Gaussian_distrib", width = 9, height = 5, units = "in", res=300,  compression = "lzw", bg = "white", family = "", restoreConsole = TRUE, type ="windows")
par(mfrow = c(1, 2))

# Residuals Histogram for full dataset
Resid1 <- hist(fm.resid, breaks=seq(-1.5,1.5,0.2), plot = FALSE)
pos <- pretty(Resid1$density, n = 6)
freq <- round(pos * length(fm.resid) * with(Resid1, breaks[2] - breaks[1]))
new.mai <- old.mai <- par("mai")
new.mai[4] <- old.mai[2]
par(mai = new.mai)
graphics:::plot.histogram(Resid1, freq = FALSE, col="gray70", family="sans", main="(a)", cex.main=2, 
                          font.main=2, font.lab=2, xlab = "Residuals", ylab="Frequency",
                          border="black", yaxt='n')
Axis(side = 2, at = pos, labels = freq)
Axis(side = 4, at = pos, labels = pos)
mtext("Density           ", side = 4, line = 3, family="sans", font=2)
polygon(density(fm.resid), col = rgb(0, 0, 1, 0.3))
fit<-fitdistr(fm.resid,"normal")$estimate
xfit<-seq((min(fm.resid)-1),(max(fm.resid)+1),length=50) 
yfit<-dnorm(xfit,fit[1],fit[2])
lines(xfit, yfit, col="red", lwd=2)
par(mai = old.mai)

# Figure A4b
ecdf.ks.CI(fm.resid, main= "(b)", cex.main=2, font.main=2, font.lab=2, xlab="Residuals", ylab="ECDF(Residuals)")
fit<-fitdistr(fm.resid,"normal")$estimate
Norm3 <- rnorm(10000, fit[1],fit[2])
lines(ecdf(Norm3), do.points = FALSE, verticals=T, lwd=2, col="blue")

# reset graphics
#dev.off()
par(mai = old.mai)
par(mfrow = c(1, 1))


##### Residuals Tests for No Rural/Municipal Subset (n = 68) ######


NoRur <- subset(main_dataset, Problem < 2 & Rural < 1)

fm.NR <- lm(NoRur$Log_Tax ~ NoRur$Log_Taxpayers)
fm.NR.resid <- resid(fm.NR)

shapiro.test(fm.NR.resid)

fit<-fitdistr(fm.NR.resid,"normal")$estimate
ks.test(fm.NR.resid, "pnorm",fit[1],fit[2])


##### Residuals Tests for No Problematic Returns Subset (n = 77) ######


NoProb <- subset(main_dataset, Problem < 1 & Rural < 2)

fm.NP <- lm(NoProb$Log_Tax ~ NoProb$Log_Taxpayers)
fm.NP.resid <- resid(fm.NP)

shapiro.test(fm.NP.resid)

fit<-fitdistr(fm.NP.resid,"normal")$estimate
ks.test(fm.NP.resid, "pnorm",fit[1],fit[2])


##### Residuals Tests for No Rural, No Problem Subset (n = 55) ######

NoRurNoProb <- subset(main_dataset, Problem < 1 & Rural < 1)

fm.NRNP <- lm(NoRurNoProb$Log_Tax ~ NoRurNoProb$Log_Taxpayers)
fm.NRNP.resid <- resid(fm.NRNP)

shapiro.test(fm.NRNP.resid)

fit<-fitdistr(fm.NRNP.resid,"normal")$estimate
ks.test(fm.NRNP.resid, "pnorm",fit[1],fit[2])



#####################################################################################################
##### Calculation of WCSs from "WCS1: Maximal Modifier Ranges" and "A1.4 Proxy Data WCS Models" #####
#####################################################################################################

WCS_Calculations <- main_dataset[-c(7:11)]

LinearRescale <- function(T, a, b, c, d){
	M <- NA
	for (i in 1:length(T)) {
		M[i] = ((b - a)*(T[i] - c))/(d - c) + a
		}
	return(M)
}

#### A3 WCS1: Maximal Modifier Rages ####

WCS_Calculations$WCS1_4_6_mods <- LinearRescale(WCS_Calculations$Taxpayers, 4, 6, 151, 1423)
WCS_Calculations$WCS1_4_6_pops <- round(WCS_Calculations$WCS1_4_6_mods * WCS_Calculations$Taxpayers, 0)
WCS_Calculations$WCS1_5_7_mods <- LinearRescale(WCS_Calculations$Taxpayers, 5, 7, 151, 1423)
WCS_Calculations$WCS1_5_7_pops <- round(WCS_Calculations$WCS1_5_7_mods * WCS_Calculations$Taxpayers, 0)
WCS_Calculations$WCS1_5.5_7.5_mods <- LinearRescale(WCS_Calculations$Taxpayers, 5.5, 7.5, 151, 1423)
WCS_Calculations$WCS1_5.5_7.5_pops <- round(WCS_Calculations$WCS1_5.5_7.5_mods * WCS_Calculations$Taxpayers, 0)
WCS_Calculations$WCS1_6_8_mods <- LinearRescale(WCS_Calculations$Taxpayers, 6, 8, 151, 1423)
WCS_Calculations$WCS1_6_8_pops <- round(WCS_Calculations$WCS1_6_8_mods * WCS_Calculations$Taxpayers, 0)
WCS_Calculations$WCS1_7_9_mods <- LinearRescale(WCS_Calculations$Taxpayers, 7, 9, 151, 1423)
WCS_Calculations$WCS1_7_9_pops <- round(WCS_Calculations$WCS1_7_9_mods * WCS_Calculations$Taxpayers, 0)

#### A4.1 Taxpayer Exemption Proportions - for WCS2 and WCS3  ####

WCS_Calculations$WCS_exemption_rate <- LinearRescale(WCS_Calculations$Taxpayers, 1.25, 2, 151, 1000)
WCS_Calculations$WCS_exemption_rate[1:3] <- 1/(1-0.5)
WCS_Calculations$WCS_exemption_rate_Honor <- WCS_Calculations$WCS_exemption_rate
WCS_Calculations$WCS_exemption_rate_Honor[3] <- 1/(1-0.48)
WCS_Calculations$WCS_exemption_rate_Honor[6] <- 1/(1-0.544)
WCS_Calculations$WCS_exemption_rate_Honor[7] <- 1/(1-0.27)
WCS_Calculations$WCS_exemption_rate_Honor[15] <- 1/(1-0.44)
WCS_Calculations$WCS_exemption_rate_Honor[16] <- 1/(1-0.37)
WCS_Calculations$WCS_exemption_rate_Honor[18] <- 1/(1-0.363)
WCS_Calculations$WCS_exemption_rate_Honor[22] <- 1/(1-0.333)
WCS_Calculations$WCS_exemption_rate_Honor[23] <- 1/(1-0.25)
WCS_Calculations$WCS_exemption_rate_Honor[44] <- 1/(1-0.327)
WCS_Calculations$WCS_exemption_rate_Honor[46] <- 1/(1-0.204)
WCS_Calculations$WCS_exemption_rate_Honor[55] <- 1/(1-0.203)
WCS_Calculations$WCS_exemption_rate_Honor[76] <- 1/(1-0.146)
WCS_Calculations$WCS_exemption_rate_Honor[85] <- 1/(1-0.262)

#### A4.2 WCS2: Taxpayers as Households - Mean Household Size (MHS) ####

WCS_Calculations$WCS2_MHS <- LinearRescale(WCS_Calculations$Taxpayers, 4.75, 3.83, 151, 1000)
WCS_Calculations$WCS2_MHS[1:3] <- 3.83
WCS_Calculations$WCS2_mods <- WCS_Calculations$WCS2_MHS * WCS_Calculations$WCS_exemption_rate
WCS_Calculations$WCS2_mods_Honor <- WCS_Calculations$WCS2_MHS * WCS_Calculations$WCS_exemption_rate_Honor
WCS_Calculations$WCS2_pops  <- round(WCS_Calculations$WCS2_mods * WCS_Calculations$Taxpayers, 0)
WCS_Calculations$WCS2_pops_Honor <- round(WCS_Calculations$WCS2_mods_Honor * WCS_Calculations$Taxpayers, 0)

#### A4.3 WCS3: Taxpayers as Adult Males - Sex Ratios and Age Structure ###

WCS_Calculations$WCS3_sex_ratio <- LinearRescale(WCS_Calculations$Taxpayers, 2.18, 2.39, 151, 1000)
WCS_Calculations$WCS3_sex_ratio[1:3] <- 2.39
WCS_Calculations$WCS3_age_struct <- LinearRescale(WCS_Calculations$Taxpayers, 1.66, 1.37, 151, 1000)
WCS_Calculations$WCS3_age_struct[1:3] <- 1.37
WCS_Calculations$WCS3_mods <- WCS_Calculations$WCS3_age_struct * WCS_Calculations$WCS3_sex_ratio * WCS_Calculations$WCS_exemption_rate
WCS_Calculations$WCS3_mods_Honor <- WCS_Calculations$WCS3_age_struct * WCS_Calculations$WCS3_sex_ratio * WCS_Calculations$WCS_exemption_rate_Honor
WCS_Calculations$WCS3_pops  <- round(WCS_Calculations$WCS3_mods * WCS_Calculations$Taxpayers, 0)
WCS_Calculations$WCS3_pops_Honor <- round(WCS_Calculations$WCS3_mods_Honor * WCS_Calculations$Taxpayers, 0)

#### View the WCS Calculation Table ####
View(WCS_Calculations)
#write.csv(WCS_Calculations, file = "WCS_Calculations_Table_Output.csv")



#####################################################################################################
######### Analysis and Figures from 5.3. Sensitivity Analysis and A1.6 Sensitivity Analysis #########
#####################################################################################################

library(zoo)
library(lmtest)
library(sandwich)
library(ggplot2)
library(devtools)
library(broom)
library(plyr)
#install.packages("zoo")
#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("ggplot2")
#install.packages("devtools")
#install.packages("broom")
#install.packages("plyr")


#### Full Sensitivity Analysis Script (output is a dataframe / table) ####

WCS_Table <- data.frame()
ZA <- c("1", "2", "3", "4", "5", "6", "Taxpayers", "Dyer (2000c)", "Dyer (2000c) Extended", "WCS1", "WCS1", "WCS1", "WCS1", "WCS1", "WCS2", "WCS2 Honor Data", "WCS3", "WCS3 Honor Data")
ZB <- c("1", "2", "3", "4", "5", "6", "NA", "NA", "NA", "[4, 6]", "[5, 7]", "[5.5, 7.5]", "[6, 8]", "[7, 9]", "[5.94, 7.66]", "[5.5, 9.05]", "[4.52, 6.55]", "[4.22, 7.42]")
ZC <- c(1, 2, 3, 4, 5, 6, NA, NA, NA, 1.5, 1.4, 1.36, 1.33, 1.28, 1.29, 1.65, 1.45, 1.76)
ZD <- c("1", "2", "3", "4", "5", "6", "Raw Data", "Pop Estimates", "Pop Estimates", "Extended Modifier Ranges", "Extended Modifier Ranges", "Extended Modifier Ranges", "Extended Modifier Ranges", "Extended Modifier Ranges", "Taxpayers as Households (MHS)", "Taxpayers as Households (MHS)", "Taxpayers as Adult Males (Sex Ratios and Age Structure)", "Taxpayers as Adult Males (Sex Ratios and Age Structure)")
for (j in 7:18) {
	out <- data.frame()
	Z <- colnames(WCS)[j] 
	Z1 <- ZA[j]
	Z2 <- ZB[j]
	Z3 <- ZC[j]
	Z4 <- ZD[j]
	for (i in 1:4){
		EE = matrix( c(2, 2, 2, 1, 1, 2, 1, 1), nrow=2, ncol=4)
		x.sub <- subset(WCS, Problem < EE[1,i] & Rural < EE[2,i])
		x <- log(x.sub[,j])
		y <- log(x.sub$Tax)
		if (j == 8) {
			new_DF <- x.sub[rowSums(is.na(x.sub)) > 0,]
			remove <- new_DF$Number
			x=x[-remove]
			y=y[-remove]
			}
		fm <- lm(y ~ x)
		tidy_fm <- tidy(fm)
		coeftest <- coeftest(fm, vcov = vcovHC(fm, "HC1"))
		tidy_coeftest <- tidy(coeftest)
		tidy_coeftest$term[1] <- "alpha"
		tidy_coeftest$term[2] <- "beta"
		tidy_coeftest$param <- c(1, 2)
		tidy_coeftest$estimate[1] <- exp(tidy_coeftest$estimate[1])
		if (EE[1,i] == 2 & EE[2,i] == 2){
			tidy_coeftest$analysis <- c("All", "All")
			tidy_coeftest$analysis_num <- c(1, 1)
		}
		if (EE[1,i] == 2 & EE[2,i] == 1){
			tidy_coeftest$analysis <- c("No Rural", "No Rural")
			tidy_coeftest$analysis_num <- c(2, 2)
		}
		if (EE[1,i] == 1 & EE[2,i] == 2){
			tidy_coeftest$analysis <- c("No Problem", "No Problem")
			tidy_coeftest$analysis_num <- c(3, 3)
		}
		if (EE[1,i] == 1 & EE[2,i] == 1){
			tidy_coeftest$analysis <- c("No Rural, No Problem", "No Rural, No Problem")
			tidy_coeftest$analysis_num <- c(4, 4)
		}
		tidy_coeftest$n <- c(length(x), length(x))
		tidy_coeftest$rsquared <- c(summary(fm)$r.squared, summary(fm)$r.squared)
		#tidy_coeftest$dataset_num <- c(1, 1)
		
		#set HC1 covariance matrix as an object
		Cov<-vcovHC(fm, "HC1")

		#calculate and report 95% confidence interval using HC1 covariance matrix
		tt <-qt(c(0.025,0.975),summary(fm)$df[2])
		se <- sqrt(diag(Cov))
		ci <-coef(fm) + se %o% tt

		tidy_coeftest$ci0.025 <- c(exp(ci[1,1]), ci[2,1])
		tidy_coeftest$ci0.975 <- c(exp(ci[1,2]), ci[2,2])
		out=rbind(out,tidy_coeftest)
	}
	WCS_analysis <- c(Z,Z,Z,Z)
	Group <- c(Z1,Z1,Z1,Z1)
	Modifier_Ranges <- c(Z2,Z2,Z2,Z2)
	b_a_ratio <- c(Z3,Z3,Z3,Z3)
	Estimation_Basis <- c(Z4,Z4,Z4,Z4)
	analysis_num <- out$analysis_num[c(TRUE, FALSE)]
	Subset <- out$analysis[c(TRUE, FALSE)]
	n <- out$n[c(TRUE, FALSE)]
	R_squared <- out$rsquared[c(TRUE, FALSE)]
	alpha_estimate <- out$estimate[c(TRUE, FALSE)]
	alpha_ci_0.025 <- out$ci0.025[c(TRUE, FALSE)]
	alpha_ci_0.975 <- out$ci0.975[c(TRUE, FALSE)]
	beta_estimate <- out$estimate[c(FALSE, TRUE)]
	beta_ci_0.025 <- out$ci0.025[c(FALSE, TRUE)]
	beta_ci_0.975 <- out$ci0.975[c(FALSE, TRUE)]
	Table <- data.frame(WCS_analysis, Group, Modifier_Ranges, b_a_ratio, Estimation_Basis, analysis_num, Subset, n, R_squared, beta_estimate, beta_ci_0.025, beta_ci_0.975, alpha_estimate, alpha_ci_0.025, alpha_ci_0.975)
	WCS_Table=rbind(WCS_Table,Table)
}


#### Table (dataframe) of Full WCS Results (Tables A5 and A9 in Appendix) ####
View(WCS_Table)
#write.csv(WCS_Table, file = "Sensitivity_Analysis_Results_Table_Output.csv")


#### Table of Sensitivity Analysis Regression Results Summarized by Dataset (Table 2 of main text, Table A8 of Appendix ) ####

WCS_Table_Mean_Summary <- ddply(WCS_Table, .(WCS_analysis), summarise,
 mean_R_square = round(mean(R_squared), 2),
 mean_alpha_estimate = round(mean(alpha_estimate), 3),
 mean_alpha_ci_0.025 = round(mean(alpha_ci_0.025), 3),
 mean_alpha_ci_0.975 = round(mean(alpha_ci_0.975), 3),
 mean_beta_estimate = round(mean(beta_estimate), 2),
 mean_beta_ci_0.025 = round(mean(beta_ci_0.025), 2),
 mean_beta_ci_0.975 = round(mean(beta_ci_0.975), 2))
WCS_Table_Mean_Summary$Group <- c("Taxpayers", "Dyer (2000c)", "Dyer (2000c) Extended", "WCS1", "WCS1", "WCS1", "WCS1", "WCS1", "WCS2", "WCS2 Honor Data", "WCS3", "WCS3 Honor Data")
WCS_Table_Mean_Summary$Modifier_Ranges <- c("NA", "NA", "NA", "[4, 6]", "[5, 7]", "[5.5, 7.5]", "[6, 8]", "[7, 9]", "[5.94, 7.66]", "[5.5, 9.05]", "[4.52, 6.55]", "[4.22, 7.42]")
WCS_Table_Mean_Summary$b_a_ratio <- c(NA, NA, NA, 1.5, 1.4, 1.36, 1.33, 1.28, 1.29, 1.65, 1.45, 1.76)
WCS_Table_Mean_Summary$Estimation_Basis <- c("Raw Data", "Pop Estimates", "Pop Estimates", "Extended Modifier Ranges", "Extended Modifier Ranges", "Extended Modifier Ranges", "Extended Modifier Ranges", "Extended Modifier Ranges", "Taxpayers as Households (MHS)", "Taxpayers as Households (MHS)", "Taxpayers as Adult Males (Sex Ratios and Age Structure)", "Taxpayers as Adult Males (Sex Ratios and Age Structure)")
WCS_Table_Mean_Summary$n <- c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
View(WCS_Table_Mean_Summary)
#write.csv(WCS_Table_Mean_Summary, file = "Sensitivity_Analysis_Summary_Table_Averages.csv")


#### Statistical Summary of all WCS Regressions (n = 36) (Table 3 of Main Text and Table A5 of Appendix) ####

WCS_Table_Cropped <- WCS_Table[-c(1:12), ] 
Stat_Summary_WCS <- data.frame(row.names = c("Mean", "St.Dev.", "Min", "Q1", "Median", "Q3", "Max"))
Stat_Summary_WCS$R_square <- c(round(mean(WCS_Table_Cropped$R_squared), 2), round(sd(WCS_Table_Cropped$R_squared), 2), round(min(WCS_Table_Cropped$R_squared), 2), round(quantile(WCS_Table_Cropped$R_squared)[2], 2), round(median(WCS_Table_Cropped$R_squared), 2), round(quantile(WCS_Table_Cropped$R_squared)[4], 2), round(max(WCS_Table_Cropped$R_squared), 2))
Stat_Summary_WCS$beta <- c(round(mean(WCS_Table_Cropped$beta_estimate), 2), round(sd(WCS_Table_Cropped$beta_estimate), 2), round(min(WCS_Table_Cropped$beta_estimate), 2), round(quantile(WCS_Table_Cropped$beta_estimate)[2], 2), round(median(WCS_Table_Cropped$beta_estimate), 2), round(quantile(WCS_Table_Cropped$beta_estimate)[4], 2), round(max(WCS_Table_Cropped$beta_estimate), 2))
Stat_Summary_WCS$beta_ci_0.025 <- c(round(mean(WCS_Table_Cropped$beta_ci_0.025), 2), round(sd(WCS_Table_Cropped$beta_ci_0.025), 2), round(min(WCS_Table_Cropped$beta_ci_0.025), 2), round(quantile(WCS_Table_Cropped$beta_ci_0.025)[2], 2), round(median(WCS_Table_Cropped$beta_ci_0.025), 2), round(quantile(WCS_Table_Cropped$beta_ci_0.025)[4], 2), round(max(WCS_Table_Cropped$beta_ci_0.025), 2))
Stat_Summary_WCS$beta_ci_0.975 <- c(round(mean(WCS_Table_Cropped$beta_ci_0.975), 2), round(sd(WCS_Table_Cropped$beta_ci_0.975), 2), round(min(WCS_Table_Cropped$beta_ci_0.975), 2), round(quantile(WCS_Table_Cropped$beta_ci_0.975)[2], 2), round(median(WCS_Table_Cropped$beta_ci_0.975), 2), round(quantile(WCS_Table_Cropped$beta_ci_0.975)[4], 2), round(max(WCS_Table_Cropped$beta_ci_0.975), 2))
Stat_Summary_WCS$alpha <- c(round(mean(WCS_Table_Cropped$alpha_estimate), 3), round(sd(WCS_Table_Cropped$alpha_estimate), 3), round(min(WCS_Table_Cropped$alpha_estimate), 3), round(quantile(WCS_Table_Cropped$alpha_estimate)[2], 3), round(median(WCS_Table_Cropped$alpha_estimate), 3), round(quantile(WCS_Table_Cropped$alpha_estimate)[4], 3), round(max(WCS_Table_Cropped$alpha_estimate), 3))
Stat_Summary_WCS$alpha_ci_0.025 <- c(round(mean(WCS_Table_Cropped$alpha_ci_0.025), 3), round(sd(WCS_Table_Cropped$alpha_ci_0.025), 3), round(min(WCS_Table_Cropped$alpha_ci_0.025), 3), round(quantile(WCS_Table_Cropped$alpha_ci_0.025)[2], 3), round(median(WCS_Table_Cropped$alpha_ci_0.025), 3), round(quantile(WCS_Table_Cropped$alpha_ci_0.025)[4], 3), round(max(WCS_Table_Cropped$alpha_ci_0.025), 3))
Stat_Summary_WCS$alpha_ci_0.975 <- c(round(mean(WCS_Table_Cropped$alpha_ci_0.975), 3), round(sd(WCS_Table_Cropped$alpha_ci_0.975), 3), round(min(WCS_Table_Cropped$alpha_ci_0.975), 3), round(quantile(WCS_Table_Cropped$alpha_ci_0.975)[2], 3), round(median(WCS_Table_Cropped$alpha_ci_0.975), 3), round(quantile(WCS_Table_Cropped$alpha_ci_0.975)[4], 3), round(max(WCS_Table_Cropped$alpha_ci_0.975), 3))
View(Stat_Summary_WCS)
#write.csv(Stat_Summary_WCS, file = "SStat_Summary_WCS_Table.csv")


#### Sensitivity Analysis Histogram for all WCS β coefficients (n = 36) (Figure A5 of Appendix) ####

betas <- WCS_Table_Cropped$beta_estimate
betasNRNP <- betas[c(FALSE, FALSE, FALSE, TRUE)]

#tiff(filename = "Fig_2_Hist_betas.tif", width = 5, height = 4, units = "in", res=300,  compression = "lzw", bg = "white", family = "", restoreConsole = TRUE, type ="windows")

hist(betas, breaks=seq(1,1.2,0.02), main= "WCSs 1-3 (n = 36)", col='grey60', xlim=c(1,1.2), ylim=c(0,10), xlab="Estimated β")
hist(betasNRNP, col='Black', add=T)

#dev.off()


#####################################################################################################
################################# Analysis from 7. IRS and Town Size ################################
#####################################################################################################

##### Analyses for Table 3. Scaling Analysis of Town Size Ranges in 1524/5 Lay Subsidy #########

#### Analyses for Table 3A: Towns Split Into Thirds

fm <- lm(main_dataset$Log_Tax[1:31] ~ main_dataset$Log_Taxpayers[1:31])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[32:62] ~ main_dataset$Log_Taxpayers[32:62])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[63:93] ~ main_dataset$Log_Taxpayers[63:93])
summary(fm)
exp(fm$coefficients[1])

#### Analyses for Table 3A: Stepwise Removal of Larger Towns

fm <- lm(main_dataset$Log_Tax[1:93] ~ main_dataset$Log_Taxpayers[1:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[6:93] ~ main_dataset$Log_Taxpayers[6:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[11:93] ~ main_dataset$Log_Taxpayers[11:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[16:93] ~ main_dataset$Log_Taxpayers[16:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[21:93] ~ main_dataset$Log_Taxpayers[21:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[26:93] ~ main_dataset$Log_Taxpayers[26:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[31:93] ~ main_dataset$Log_Taxpayers[31:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[36:93] ~ main_dataset$Log_Taxpayers[36:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[41:93] ~ main_dataset$Log_Taxpayers[41:93])
summary(fm)
exp(fm$coefficients[1]) 

fm <- lm(main_dataset$Log_Tax[46:93] ~ main_dataset$Log_Taxpayers[46:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[51:93] ~ main_dataset$Log_Taxpayers[51:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[56:93] ~ main_dataset$Log_Taxpayers[56:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[61:93] ~ main_dataset$Log_Taxpayers[61:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[66:93] ~ main_dataset$Log_Taxpayers[66:93])
summary(fm)
exp(fm$coefficients[1])

fm <- lm(main_dataset$Log_Tax[71:93] ~ main_dataset$Log_Taxpayers[71:93])
summary(fm)
exp(fm$coefficients[1])

#####################################################################################################
##################### Analysis from 8. Scaling Residuals and Regional Demand ########################
#####################################################################################################

####### Global Moran's I for English Town Log Regression Residuals (n = 93) ###########

library(spdep)
library(ncf) 
library(maptools)

# create distance matrix for all towns
resid_coords_geog.d <- as.matrix(dist(cbind(resid_coords_geog$Lon, resid_coords_geog$Lat)))

# create vector of the average tax paid per taxpayer
ord <- order(resid_coords_geog$Resid)

# plot and color code sites, label by town number
plot(resid_coords_geog[ord, 10:9], pch = 16, col = colorRampPalette(c("yellow", "red"))(nrow(resid_coords_geog)),
xlab = "Lon", ylab = "Lat", add=T)
text(resid_coords_geog[ord, 10:9], labels = resid_coords_geog[ord, 2], cex = 0.7, pos = 1)

# create a matrix of weights based on the inverse distances between sites
w <- 1/resid_coords_geog.d
diag(w) <- 0

moran.test(resid_coords_geog$Resid, mat2listw(w))

####### Multiple OLS Regression for County-Level Regression Residuals on Demand Proxies (n = 31) ###########

fm <- lm(TC$SumLogResid ~ TC$Tax1524.5 + TC$Markets1588_km2 + TC$TownTax.TotTax)
summary(fm)

fm <- lm(TC$MaxLogResid ~ TC$Tax1524.5 + TC$Markets1588_km2 + TC$TownTax.TotTax)
summary(fm)




########################################################################### END ###############################################################################

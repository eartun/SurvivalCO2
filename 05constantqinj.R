library(randomForest)
library(iml)
library(tidyverse)
library(ggcorrplot)
library(GGally)
library(ggpubr)
library(survminer)
library(survival)

#Load the data set, assign it to co2seq and attach it for easy variable access
co2seq <- read.csv("datac.csv")
attach(co2seq)


#Defining normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#Defining cencoring variable C1
co2seq <- co2seq %>%mutate(C1 = ifelse(Gi < 1000000 | T1 > 40, 0, 1))
co2seqf <- co2seq %>% filter(C1==1) #filtering out cencored cases for correlation analysis
co2seqn <-as.data.frame(lapply(co2seqf,normalize)) #normalizing between 0 and 1
attach(co2seqn)
##PEARSON CORRELATION PLOTS
# cor1p<-ggcorr(as.data.frame(cbind(co2seqn[,1:13],T1)), geom = "blank", label = TRUE, hjust = 0.75,layout.exp = 0.2) +
#   geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
#   scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +ggtitle("a)")+
#   guides(color = "none", alpha =  "none")
# cor2p<-ggcorr(as.data.frame(cbind(co2seqn[,14:25],T1)), geom = "blank", label = TRUE, hjust = 0.75,layout.exp = 0.2) +
#   geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
#   scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +ggtitle("b)")+
#   guides(color = "none", alpha =  "none")
# 
# ggarrange(cor1p,cor2p,nrow=1, ncol=2)
# ggsave("cor1.png",width = 9,height =4)
# ggsave("cor1.pdf",width = 9,height =4)

#Analysis of SHAPley values for Variable Relationships 
#using random forests for modeling

X <- co2seq[, setdiff(names(co2seq), c("T1", "TINJ","Gi","C1"))]
y <- co2seq$T1
set.seed(123)
rf_model <- randomForest(x = X, y = y, ntree = 100)
prediction <- predict(rf_model, X)
#Train Error
SSE = sum((prediction - y)^2)
SST = sum( (mean(y) - y)^2)
R2 = 1 - SSE/SST
# Combine target and features for iml package
data <- cbind(X, T1 = y)
predictor <- Predictor$new(rf_model, data = X, y = y)
shapley <- Shapley$new(predictor, x.interest = X[1, ])  # For one example
feature_imp <- FeatureImp$new(predictor, loss = "mse")
theme_set(theme_bw())
plot(feature_imp)
ggsave("SHAPC.png",width = 7, height = 3.5,dpi=300)
ggsave("SHAPC.pdf",width = 7, height = 3.5,dpi=300)


km <- survfit(Surv(T1,C1) ~ 1, data = co2seq)
# Step 1: Call the pdf command to start the plot
pdf(file = "Plot.pdf", width = 6,  height = 4)
ggsurvplot(km,palette = "blue",risk.table = FALSE,surv.median.line = "hv",xlim=c(0,40),legend="none",break.time.by = 5,xlab="Time, years")
dev.off()




co2seq <- co2seq %>%   mutate(PRESc = case_when(
  PRES < quantile(PRES, probs = 0.30) ~ "Low",
  PRES > quantile(PRES, probs = 0.70) ~ "High",
  PRES < quantile(PRES, probs = 0.70) & PRES > quantile(PRES, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(TEMPc = case_when(
  TEMP < quantile(TEMP, probs = 0.30) ~ "Low",
  TEMP > quantile(TEMP, probs = 0.70) ~ "High",
  TEMP < quantile(TEMP, probs = 0.70) & TEMP > quantile(TEMP, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(THCKc = case_when(
  THCK < quantile(THCK, probs = 0.30) ~ "Low",
  THCK > quantile(THCK, probs = 0.70) ~ "High",
  THCK < quantile(THCK, probs = 0.70) & THCK > quantile(THCK, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(SWATc = case_when(
  SWAT < quantile(SWAT, probs = 0.30) ~ "Low",
  SWAT > quantile(SWAT, probs = 0.70) ~ "High",
  SWAT < quantile(SWAT, probs = 0.70) & SWAT > quantile(SWAT, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(POROMc = case_when(
  POROM < quantile(POROM, probs = 0.30) ~ "Low",
  POROM > quantile(POROM, probs = 0.70) ~ "High",
  POROM < quantile(POROM, probs = 0.70) & POROM > quantile(POROM, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(POROFc = case_when(
  POROF < quantile(POROF, probs = 0.30) ~ "Low",
  POROF > quantile(POROF, probs = 0.70) ~ "High",
  POROF < quantile(POROF, probs = 0.70) & POROF > quantile(POROF, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(PERMMc = case_when(
  PERMM < quantile(PERMM, probs = 0.30) ~ "Low",
  PERMM > quantile(PERMM, probs = 0.70) ~ "High",
  PERMM < quantile(PERMM, probs = 0.70) & PERMM > quantile(PERMM, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(PERMFc = case_when(
  PERMF < quantile(PERMF, probs = 0.30) ~ "Low",
  PERMF > quantile(PERMF, probs = 0.70) ~ "High",
  PERMF < quantile(PERMF, probs = 0.70) & PERMF > quantile(PERMF, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(PRESc = case_when(
  PRES < quantile(PRES, probs = 0.30) ~ "Low",
  PRES > quantile(PRES, probs = 0.70) ~ "High",
  PRES < quantile(PRES, probs = 0.70) & PRES > quantile(PRES, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(FRACSc = case_when(
  FRACS < quantile(FRACS, probs = 0.30) ~ "Low",
  FRACS > quantile(FRACS, probs = 0.70) ~ "High",
  FRACS < quantile(FRACS, probs = 0.70) & FRACS > quantile(FRACS, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(VLCH4c = case_when(
  VLCH4 < quantile(VLCH4, probs = 0.30) ~ "Low",
  VLCH4 > quantile(VLCH4, probs = 0.70) ~ "High",
  VLCH4 < quantile(VLCH4, probs = 0.70) & VLCH4 > quantile(VLCH4, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(PLCH4c = case_when(
  PLCH4 < quantile(PLCH4, probs = 0.30) ~ "Low",
  PLCH4 > quantile(PLCH4, probs = 0.70) ~ "High",
  PLCH4 < quantile(PLCH4, probs = 0.70) & PLCH4 > quantile(PLCH4, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(VLCO2c = case_when(
  VLCO2 < quantile(VLCO2, probs = 0.30) ~ "Low",
  VLCO2 > quantile(VLCO2, probs = 0.70) ~ "High",
  VLCO2 < quantile(VLCO2, probs = 0.70) & VLCO2 > quantile(VLCO2, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(PLCO2c = case_when(
  PLCO2 < quantile(PLCO2, probs = 0.30) ~ "Low",
  PLCO2 > quantile(PLCO2, probs = 0.70) ~ "High",
  PLCO2 < quantile(PLCO2, probs = 0.70) & PLCO2 > quantile(PLCO2, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(QINJc = case_when(
  QINJ < quantile(QINJ, probs = 0.30) ~ "Low",
  QINJ > quantile(QINJ, probs = 0.70) ~ "High",
  QINJ < quantile(QINJ, probs = 0.70) & QINJ > quantile(QINJ, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(TPRODc = case_when(
  TPROD < quantile(TPROD, probs = 0.30) ~ "Low",
  TPROD > quantile(TPROD, probs = 0.70) ~ "High",
  TPROD < quantile(TPROD, probs = 0.70) & TPROD > quantile(TPROD, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(PWFPRODc = case_when(
  PWFPROD < quantile(PWFPROD, probs = 0.30) ~ "Low",
  PWFPROD > quantile(PWFPROD, probs = 0.70) ~ "High",
  PWFPROD < quantile(PWFPROD, probs = 0.70) & PWFPROD > quantile(PWFPROD, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(PFRACc = case_when(
  PFRAC < quantile(PFRAC, probs = 0.30) ~ "Low",
  PFRAC > quantile(PFRAC, probs = 0.70) ~ "High",
  PFRAC < quantile(PFRAC, probs = 0.70) & PFRAC > quantile(PFRAC, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(LXc = case_when(
  LX < quantile(LX, probs = 0.30) ~ "Low",
  LX > quantile(LX, probs = 0.70) ~ "High",
  LX < quantile(LX, probs = 0.70) & LX > quantile(LX, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(LYc = case_when(
  LY < quantile(LY, probs = 0.30) ~ "Low",
  LY > quantile(LY, probs = 0.70) ~ "High",
  LY < quantile(LY, probs = 0.70) & LY > quantile(LY, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(LWELLc = case_when(
  LWELL < quantile(LWELL, probs = 0.30) ~ "Low",
  LWELL > quantile(LWELL, probs = 0.70) ~ "High",
  LWELL < quantile(LWELL, probs = 0.70) & LWELL > quantile(LWELL, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(LFRACc = case_when(
  LFRAC < quantile(LFRAC, probs = 0.30) ~ "Low",
  LFRAC > quantile(LFRAC, probs = 0.70) ~ "High",
  LFRAC < quantile(LFRAC, probs = 0.70) & LFRAC > quantile(LFRAC, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(SRVPERMc = case_when(
  SRVPERM < quantile(SRVPERM, probs = 0.30) ~ "Low",
  SRVPERM > quantile(SRVPERM, probs = 0.70) ~ "High",
  SRVPERM < quantile(SRVPERM, probs = 0.70) & SRVPERM > quantile(SRVPERM, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(SRVPOROc = case_when(
  SRVPORO < quantile(SRVPORO, probs = 0.30) ~ "Low",
  SRVPORO > quantile(SRVPORO, probs = 0.70) ~ "High",
  SRVPORO < quantile(SRVPORO, probs = 0.70) & SRVPORO > quantile(SRVPORO, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(SRVSPACc = case_when(
  SRVSPAC < quantile(SRVSPAC, probs = 0.30) ~ "Low",
  SRVSPAC > quantile(SRVSPAC, probs = 0.70) ~ "High",
  SRVSPAC < quantile(SRVSPAC, probs = 0.70) & SRVSPAC > quantile(SRVSPAC, probs = 0.30) ~ "Mid"))
co2seq <- co2seq %>%   mutate(QPRODc = case_when(
  QPROD < quantile(QPROD, probs = 0.30) ~ "Low",
  QPROD > quantile(QPROD, probs = 0.70) ~ "High",
  QPROD < quantile(QPROD, probs = 0.70) & QPROD > quantile(QPROD, probs = 0.30) ~ "Mid"))
attach(co2seq)

#Log-Rank Test (extracting p-values and plotting them as a variable ranking plot)
pv <- seq(1,24,1)
surv_object <- Surv(T1, C1)
logrank_test <- survdiff(surv_object ~ PRESc)
pv[1]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ TEMPc)
pv[2]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ THCKc)
pv[3]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ SWATc)
pv[4]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ POROMc)
pv[5]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ POROFc)
pv[6]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ PERMMc)
pv[7]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ PERMFc)
pv[8]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ FRACSc)
pv[9]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ VLCH4c)
pv[10]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ PLCH4c)
pv[11]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ VLCO2c)
pv[12]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ PLCO2c)
pv[13]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ QPRODc)
pv[14]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ TPRODc)
pv[15]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ PWFPRODc)
pv[16]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ PFRACc)
pv[17]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ LXc)
pv[18]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ LYc)
pv[19]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ LWELLc)
pv[20]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ LFRACc)
pv[21]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ SRVPOROc)
pv[22]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ SRVPERMc)
pv[23]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ SRVSPACc)
pv[24]<-logrank_test$pvalue

pv<-as.data.frame(pv)
pv$Variable<-c(colnames(co2seq[,1:13]),colnames(co2seq[,15:25]))
pv <- pv %>% arrange(desc(pv))
pvplot<- ggplot(pv, aes(x = reorder(Variable, pv), y = pv)) +
  geom_segment(
    aes(x = Variable, y = 0, xend = Variable, yend = pv), 
    color = "steelblue", 
    linewidth = 1.5
  ) +
  geom_point(aes(x = Variable, y = pv), color = "steelblue", size = 4) +
  geom_text(
    aes(label = round(pv,4)),
    hjust = -0.2, 
    vjust = 0.5, 
    color = "black",
    size = 3
  ) + 
  ylim(0,1)+
  ylab("p-value")+
  xlab("Variable")+
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) + 
  coord_flip() 

ggarrange(pvplot)
ggsave("LogRankC.png",width = 7, height = 4.5,dpi=300)
ggsave("LogRankC.pdf",width = 7, height = 4.5,dpi=300)



#Cox PH after log normaization
co2seq$T1 <- log(co2seq$T1)
co2seq$SRVPERM <- log(co2seq$SRVPERM)
cph<-coxph(Surv(T1, C1) ~ PRES+TEMP+THCK+SWAT+POROM+POROF+PERMM+PERMF+FRACS+VLCH4+PLCH4+VLCO2+PLCO2+TPROD+QPROD+PWFPROD+PFRAC+LX+LY+LWELL+LFRAC+SRVPORO+SRVPERM+SRVSPAC, data = co2seq)
cph

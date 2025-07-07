library(survminer)
library(survival)
library(tidyverse)
library(ggfortify)
library(GGally)

co2seq <- read.csv("co2_seq2.csv")
co2seq <- co2seq %>%mutate(C1 = ifelse(Gi < 1000000 | T1 > 40, 0, 1))
attach(co2seq)
km <- survfit(Surv(T1,C1) ~ 1, data = co2seq)



ggsurvplot(km,palette = "blue",risk.table = FALSE, surv.median.line = "hv",xlim=c(0,40),break.time.by = 5,xlab="Time, years")
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

splots <- list()
splots[[1]] <- ggsurvplot(survfit(Surv(T1, C1)~PRESc),data=co2seq,
                          surv.median.line = "hv", 
                          title = "a)",pval = TRUE,xlim=c(0,40),break.time.by = 5,xlab="Time, years",palette=c("black","grey","navy"))
splots[[2]] <- ggsurvplot(survfit(Surv(T1, C1)~TEMPc),data=co2seq,
                          surv.median.line = "hv",
                          title = "b)",pval = TRUE,xlim=c(0,40),break.time.by = 5,xlab="Time, years",palette=c("black","grey","navy"))
arrange_ggsurvplots(splots, print = TRUE,ncol = 1, nrow = 2)
splots <- list()
splots[[1]] <- ggsurvplot(survfit(Surv(T1, C1)~VLCO2c),data=co2seq,
                          surv.median.line = "hv",
                          title = "a)",pval = TRUE,xlim=c(0,40),break.time.by = 5,xlab="Time, years",palette=c("black","grey","navy"))
splots[[2]] <- ggsurvplot(survfit(Surv(T1, C1)~SRVPERMc),data=co2seq,
                          surv.median.line = "hv",
                          title = "b)",pval = TRUE,xlim=c(0,40),break.time.by = 5,xlab="Time, years",palette=c("black","grey","navy"))
splots[[3]] <- ggsurvplot(survfit(Surv(T1, C1)~PERMFc),data=co2seq,
                          surv.median.line = "hv",
                          title = "c)",pval = TRUE,xlim=c(0,40),break.time.by = 5,xlab="Time, years",palette=c("black","grey","navy"))
arrange_ggsurvplots(splots, print = TRUE,ncol = 1, nrow = 3)
splots <- list()
splots[[1]] <- ggsurvplot(survfit(Surv(T1, C1)~QINJc),data=co2seq,
                          surv.median.line = "hv",
                          title = "a)",pval = TRUE,xlim=c(0,40),break.time.by = 5,xlab="Time, years",palette=c("black","grey","navy"))
splots[[2]] <- ggsurvplot(survfit(Surv(T1, C1)~LWELLc),data=co2seq,
                          surv.median.line = "hv",
                          title = "b)",pval = TRUE,xlim=c(0,40),break.time.by = 5,xlab="Time, years",palette=c("black","grey","navy"))
splots[[3]] <- ggsurvplot(survfit(Surv(T1, C1)~PFRACc),data=co2seq,
                          surv.median.line = "hv",
                          title = "c)",pval = TRUE,xlim=c(0,40),break.time.by = 5,xlab="Time, years",palette=c("black","grey","navy"))
splots[[4]] <- ggsurvplot(survfit(Surv(T1, C1)~TPRODc),data=co2seq,
                          surv.median.line = "hv",
                          title = "d)",pval = TRUE,xlim=c(0,40),break.time.by = 5,xlab="Time, years",palette=c("black","grey","navy"))
arrange_ggsurvplots(splots, print = TRUE,ncol = 2, nrow = 2)

splots <- list()
splots[[1]] <- ggsurvplot(survfit(Surv(T1, C1)~SRVPERMc),data=co2seq,
                          surv.median.line = "hv",
                          title = "a)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[2]] <- ggsurvplot(survfit(Surv(T1, C1)~SRVPOROc),data=co2seq,
                          surv.median.line = "hv",
                          title = "b)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[3]] <- ggsurvplot(survfit(Surv(T1, C1)~SRVSPACc),data=co2seq,
                          surv.median.line = "hv",
                          title = "c)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[4]] <- ggsurvplot(survfit(Surv(T1, C1)~FRACSc),data=co2seq,
                          surv.median.line = "hv",
                          title = "d)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
arrange_ggsurvplots(splots, print = TRUE,ncol = 2, nrow = 2)
splots <- list()
splots[[1]] <- ggsurvplot(survfit(Surv(T1, C1)~VLCH4c),data=co2seq,
                          surv.median.line = "hv",
                          title = "a)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[2]] <- ggsurvplot(survfit(Surv(T1, C1)~PLCH4c),data=co2seq,
                          surv.median.line = "hv",
                          title = "b)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[3]] <- ggsurvplot(survfit(Surv(T1, C1)~VLCO2c),data=co2seq,
                          surv.median.line = "hv",
                          title = "c)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[4]] <- ggsurvplot(survfit(Surv(T1, C1)~PLCO2c),data=co2seq,
                          surv.median.line = "hv",
                          title = "c)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
arrange_ggsurvplots(splots, print = TRUE,ncol = 2, nrow = 2)
splots <- list()
splots[[1]] <- ggsurvplot(survfit(Surv(T1, C1)~LXc),data=co2seq,
                          surv.median.line = "hv",
                          title = "a)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[2]] <- ggsurvplot(survfit(Surv(T1, C1)~LYc),data=co2seq,
                          surv.median.line = "hv",
                          title = "b)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[3]] <- ggsurvplot(survfit(Surv(T1, C1)~LWELLc),data=co2seq,
                          surv.median.line = "hv",
                          title = "c)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[4]] <- ggsurvplot(survfit(Surv(T1, C1)~LFRACc),data=co2seq,
                          surv.median.line = "hv",
                          title = "c)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
arrange_ggsurvplots(splots, print = TRUE,ncol = 2, nrow = 2)
splots <- list()
splots[[1]] <- ggsurvplot(survfit(Surv(T1, C1)~POROMc),data=co2seq,
                          surv.median.line = "hv",
                          title = "a)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[2]] <- ggsurvplot(survfit(Surv(T1, C1)~POROFc),data=co2seq,
                          surv.median.line = "hv",
                          title = "b)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[3]] <- ggsurvplot(survfit(Surv(T1, C1)~PERMMc),data=co2seq,
                          surv.median.line = "hv",
                          title = "c)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[4]] <- ggsurvplot(survfit(Surv(T1, C1)~PERMFc),data=co2seq,
                          surv.median.line = "hv",
                          title = "c)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
arrange_ggsurvplots(splots, print = TRUE,ncol = 2, nrow = 2)
splots <- list()
splots[[1]] <- ggsurvplot(survfit(Surv(T1, C1)~QINJc),data=co2seq,
                          surv.median.line = "hv",
                          title = "a)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[2]] <- ggsurvplot(survfit(Surv(T1, C1)~TPRODc),data=co2seq,
                          surv.median.line = "hv",
                          title = "b)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[3]] <- ggsurvplot(survfit(Surv(T1, C1)~PWFPRODc),data=co2seq,
                          surv.median.line = "hv",
                          title = "c)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
splots[[4]] <- ggsurvplot(survfit(Surv(T1, C1)~PFRACc),data=co2seq,
                          surv.median.line = "hv",
                          title = "c)",pval = TRUE,xlim=c(0,40),xlab="Time, years")
arrange_ggsurvplots(splots, print = TRUE,ncol = 2, nrow = 2)


#Log-Rank Test (extracting p-values and plotting them as a variable ranking plot)
pv <- seq(1,25,1)
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
logrank_test <- survdiff(surv_object ~ QINJc)
pv[14]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ QPRODc)
pv[15]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ TPRODc)
pv[16]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ PWFPRODc)
pv[17]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ PFRACc)
pv[18]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ LXc)
pv[19]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ LYc)
pv[20]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ LWELLc)
pv[21]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ LFRACc)
pv[22]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ SRVPOROc)
pv[23]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ SRVPERMc)
pv[24]<-logrank_test$pvalue
logrank_test <- survdiff(surv_object ~ SRVSPACc)
pv[25]<-logrank_test$pvalue

pv<-as.data.frame(pv)
pv$Variable<-colnames(co2seq[,1:25])
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
ggsave("LogRank.png",width = 10, height = 4,dpi=300)
ggsave("LogRank.pdf",width = 10, height = 4,dpi=300)


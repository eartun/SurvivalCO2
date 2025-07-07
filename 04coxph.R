library(survminer)
#library(eha)
library(survival)
library(tidyverse)
library(ggfortify)
library(ggcorrplot)
library(GGally)

co2seq <- read.csv("co2_seq2.csv")
co2seq <- co2seq %>%mutate(C1 = ifelse(Gi < 1000000 | T1 > 40, 0, 1))
attach(co2seq)


#CoxPH 
cph<-coxph(Surv(T1, C1) ~ PRES+TEMP+THCK+SWAT+POROM+POROF+PERMM+PERMF+FRACS+VLCH4+PLCH4+VLCO2+PLCO2+QINJ+TPROD+QPROD+PWFPROD+PFRAC+LX+LY+LWELL+LFRAC+SRVPORO+SRVPERM+SRVSPAC, data = co2seq)
cph
co2seq$Residuals <- residuals(cph, type = "martingale")

cph1_res <- ggplot(data = co2seq, mapping = aes(x = T1, y = Residuals)) +
  geom_point() +
  geom_smooth() +
  theme_bw() + theme(legend.key = element_blank())

cph_noinj<-coxph(Surv(T1, C1) ~ PRES+TEMP+THCK+SWAT+POROM+POROF+PERMM+PERMF+FRACS+VLCH4+PLCH4+VLCO2+PLCO2+TPROD+QPROD+PWFPROD+PFRAC+LX+LY+LWELL+LFRAC+SRVPORO+SRVPERM+SRVSPAC, data = co2seq)
cph_noinj
co2seq$Residuals <- residuals(cph_noinj, type = "martingale")

cph1_res_noinj <-ggplot(data = co2seq, mapping = aes(x = T1, y = Residuals)) +
  geom_point() +
  geom_smooth() +ggtitle("a)")+
  theme_bw() + theme(legend.key = element_blank())



#Cox PH after log normaization
co2seq$T1 <- log(co2seq$T1)
co2seq$SRVPERM <- log(co2seq$SRVPERM)
#co2seq$QINJ <- log(co2seq$QINJ)
#co2seq$QPROD <- log(co2seq$QPROD)
#co2seq$VLCO2 <- log(co2seq$VLCO2)
cph<-coxph(Surv(T1, C1) ~ PRES+TEMP+THCK+SWAT+POROM+POROF+PERMM+PERMF+FRACS+VLCH4+PLCH4+VLCO2+PLCO2+QINJ+TPROD+QPROD+PWFPROD+PFRAC+LX+LY+LWELL+LFRAC+SRVPORO+SRVPERM+SRVSPAC, data = co2seq)
cph
co2seq$Residuals <- residuals(cph_noinj, type = "martingale")

cph2_res <-ggplot(data = co2seq, mapping = aes(x = T1, y = Residuals)) +
  geom_point() +
  geom_smooth() +
  theme_bw() + theme(legend.key = element_blank())
cph_noinj<-coxph(Surv(T1, C1) ~ PRES+TEMP+THCK+SWAT+POROM+POROF+PERMM+PERMF+FRACS+VLCH4+PLCH4+VLCO2+PLCO2+TPROD+QPROD+PWFPROD+PFRAC+LX+LY+LWELL+LFRAC+SRVPORO+SRVPERM+SRVSPAC, data = co2seq)
cph_noinj

co2seq$Residuals <- residuals(cph_noinj, type = "martingale")

cph2_res_noinj <-ggplot(data = co2seq, mapping = aes(x = T1, y = Residuals)) +
  geom_point() +
  geom_smooth() +ggtitle("b)")+
  theme_bw() + theme(legend.key = element_blank())

ggarrange(cph1_res_noinj,cph2_res_noinj,nrow=1, ncol=2)
ggsave("Res.png",width = 8,height =3)
ggsave("Res.pdf",width = 8,height =3)

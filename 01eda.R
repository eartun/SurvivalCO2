library(randomForest)
library(iml)
library(tidyverse)
library(ggcorrplot)
library(GGally)
library(ggpubr)
library(survminer)
library(survival)

#Load the data set, assign it to co2seq and attach it for easy variable access
co2seq <- read.csv("data.csv")
attach(co2seq)



#Boxplots of Operational parameters
box1 <- ggplot(co2seq, aes_string(x = LX)) +
  geom_boxplot() +xlab("Length of X-side, ft")+
  theme_bw()+ggtitle("a)")+theme(axis.text.y=element_blank())
box2 <- ggplot(co2seq, aes_string(x = LY)) +
  geom_boxplot() +xlab("Length of Y-side, ft")+
  theme_bw()+ggtitle("b)")+theme(axis.text.y=element_blank())
box3 <- ggplot(co2seq, aes_string(x = LFRAC)) +
  geom_boxplot() +xlab("Fracture Half-Length, ft")+
  theme_bw()+ggtitle("c)")+theme(axis.text.y=element_blank())
box4 <- ggplot(co2seq, aes_string(x = LWELL)) +
  geom_boxplot() +xlab("Lateral Length, ft")+
  theme_bw()+ggtitle("d)")+theme(axis.text.y=element_blank())
box5 <- ggplot(co2seq, aes_string(x = SRVPORO)) +
  geom_boxplot() +xlab("SRV Porosity")+
  theme_bw()+ggtitle("e)")+theme(axis.text.y=element_blank())
box6 <- ggplot(co2seq, aes_string(x = SRVPERM)) +
  geom_boxplot() +xlab("SRV Permeability, mD")+
  theme_bw()+ggtitle("f)")+theme(axis.text.y=element_blank())
box7 <- ggplot(co2seq, aes_string(x = SRVSPAC)) +
  geom_boxplot() +xlab("SRV Fracture Spacing, ft")+
  theme_bw()+ggtitle("g)")+theme(axis.text.y=element_blank())
box8 <- ggplot(co2seq, aes_string(x = QPROD)) +
  geom_boxplot() +xlab("Initial Production Rate, Mscf/d")+
  theme_bw()+ggtitle("h)")+theme(axis.text.y=element_blank())
box9 <- ggplot(co2seq, aes_string(x = TPROD)) +
  geom_boxplot() +xlab("Production Duration, days")+
  theme_bw()+ggtitle("i)")+theme(axis.text.y=element_blank())
box10 <- ggplot(co2seq, aes_string(x = PFRAC)) +
  geom_boxplot() +xlab("Fracturing Pressure, psia")+
  theme_bw()+ggtitle("j)")+theme(axis.text.y=element_blank())
box11 <- ggplot(co2seq, aes_string(x = QINJ)) +
  geom_boxplot() +xlab("Injection Rate, Mscf/d")+
  theme_bw()+ggtitle("k)")+theme(axis.text.y=element_blank())
box12 <- ggplot(co2seq, aes_string(x = PWFPROD)) +
  geom_boxplot() +xlab("Flowing Bottom-Hole Pressure, psia")+
  theme_bw()+ggtitle("l)")+theme(axis.text.y=element_blank())
ggarrange(box1,box2,box3,box4,
          box5,box6,box7,box8,
          box9,box10,box11,box12,
          nrow=4, ncol=3)
ggsave("box_op.png",width = 10,height =5)
ggsave("box_op.pdf",width = 10,height =5)
#Boxplots of Reservoir parameters
box1 <- ggplot(co2seq, aes_string(x = PRES)) +
  geom_boxplot() +xlab("Pressure, psia")+
  theme_bw()+ggtitle("a)")+theme(axis.text.y=element_blank())
box2 <- ggplot(co2seq, aes_string(x = TEMP)) +
  geom_boxplot() +xlab("Temperature, F")+
  theme_bw()+ggtitle("b)")+theme(axis.text.y=element_blank())
box3 <- ggplot(co2seq, aes_string(x = THCK)) +
  geom_boxplot() +xlab("Thickness, ft")+
  theme_bw()+ggtitle("c)")+theme(axis.text.y=element_blank())
box4 <- ggplot(co2seq, aes_string(x = SWAT)) +
  geom_boxplot() +xlab("Water Saturation, %")+
  theme_bw()+ggtitle("d)")+theme(axis.text.y=element_blank())
box5 <- ggplot(co2seq, aes_string(x = POROM)) +
  geom_boxplot() +xlab("Matrix Porosity, psia")+
  theme_bw()+ggtitle("e)")+theme(axis.text.y=element_blank())
box6 <- ggplot(co2seq, aes_string(x = POROF)) +
  geom_boxplot() +xlab("Fracture Porosity")+
  theme_bw()+ggtitle("f)")+theme(axis.text.y=element_blank())
box7 <- ggplot(co2seq, aes_string(x = PERMM)) +
  geom_boxplot() +xlab("Matrix Permeability, mD")+
  theme_bw()+ggtitle("g)")+theme(axis.text.y=element_blank())
box8 <- ggplot(co2seq, aes_string(x = PERMF)) +
  geom_boxplot() +xlab("Fracture Permeability, mD")+
  theme_bw()+ggtitle("h)")+theme(axis.text.y=element_blank())
box9 <- ggplot(co2seq, aes_string(x = VLCH4)) +
  geom_boxplot() +xlab(expression("Lang. Vol. Constant (CH"["4"]*"), SCF/ton"))+
  theme_bw()+ggtitle("i)")+theme(axis.text.y=element_blank())
box10 <- ggplot(co2seq, aes_string(x = 1/PLCH4)) +
  geom_boxplot() +xlab(expression("Lang. Pres. Constant (CH"["4"]*"), 1/psia"))+
  theme_bw()+ggtitle("j)")+theme(axis.text.y=element_blank())
box11 <- ggplot(co2seq, aes_string(x = VLCO2)) +
  geom_boxplot() +xlab(expression("Lang. Vol. Constant (CO"["2"]*"), SCF/ton"))+
  theme_bw()+ggtitle("k)")+theme(axis.text.y=element_blank())
box12 <- ggplot(co2seq, aes_string(x = 1/PLCO2)) +
  geom_boxplot() +xlab(expression("Lang. Pres. Constant (CO"["2"]*"), 1/psia"))+
  theme_bw()+ggtitle("l)")+theme(axis.text.y=element_blank())
ggarrange(box1,box2,box3,box4,
          box5,box6,box7,box8,
          box9,box10,box11,box12,
          nrow=4, ncol=3)
ggsave("box_res.png",width = 10,height =5)
ggsave("box_res.pdf",width = 10,height =5)

#Boxplots of Cumulative gas injected and time to reach 1 Bscf of injection
box1 <- ggplot(co2seq, aes_string(x = Gi/1000000)) +
  geom_boxplot() +xlab(expression("Cumulative CO"["2"]~"Injected, BSCF"))+
  theme_bw()+ggtitle("a)")+theme(axis.text.y=element_blank())
box2 <- ggplot(co2seq%>%filter(Gi > 1000000), aes(x = TINJ/365)) +
  geom_boxplot() +xlab(expression("Injection Duration, years"))+
  theme_bw() +ggtitle("b)")+theme(axis.text.y=element_blank())
ggarrange(box1,box2,nrow=1, ncol=2 )
ggsave("box.png",width = 8,height =2)
ggsave("box.pdf",width = 8,height =2)

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
cor1p<-ggcorr(as.data.frame(cbind(co2seqn[,1:13],T1)), geom = "blank", label = TRUE, hjust = 0.75,layout.exp = 0.2) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +ggtitle("a)")+
  guides(color = "none", alpha =  "none")
cor2p<-ggcorr(as.data.frame(cbind(co2seqn[,14:25],T1)), geom = "blank", label = TRUE, hjust = 0.75,layout.exp = 0.2) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +ggtitle("b)")+
  guides(color = "none", alpha =  "none")

ggarrange(cor1p,cor2p,nrow=1, ncol=2)
ggsave("cor1.png",width = 9,height =4)
ggsave("cor1.pdf",width = 9,height =4)

#Analysis of SHAPley values for Variable Relationships 
#using random forests for modeling

X <- co2seq[, setdiff(names(co2seq), c("T1", "TINJ","Gi","C1"))]
y <- co2seq$T1
set.seed(123)
rf_model <- randomForest(x = X, y = y, ntree = 50)
prediction <- predict(rf_model, X)
X2$T1 <- predict(rf_model, X2)
hist(X2$T1)
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
ggsave("SHAP.png",width = 7, height = 3.5,dpi=300)
ggsave("SHAP.pdf",width = 7, height = 3.5,dpi=300)


#Constant Injection Rate
X2 <- X
X2$QINJ <- 150
X2$T1 <- predict(rf_model, X2)
co2seqC <- X2 %>%mutate(C1 = ifelse(T1 > 40, 0, 1))
#Defining normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#Defining cencoring variable C1
co2seqfC <- co2seqC %>% filter(C1==1) #filtering out cencored cases for correlation analysis
co2seqnC <-as.data.frame(lapply(co2seqfC,normalize)) #normalizing between 0 and 1
attach(co2seqnC)
##PEARSON CORRELATION PLOTS
cor1pC<-ggcorr(as.data.frame(cbind(co2seqnC[,1:13],T1)), geom = "blank", label = TRUE, hjust = 0.75,layout.exp = 0.2) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +ggtitle("a)")+
  guides(color = "none", alpha =  "none")
cor2pC<-ggcorr(as.data.frame(cbind(co2seqnC[,14:25],T1)), geom = "blank", label = TRUE, hjust = 0.75,layout.exp = 0.2) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +ggtitle("b)")+
  guides(color = "none", alpha =  "none")

ggarrange(cor1pC,cor2pC,nrow=1, ncol=2)
ggsave("cor1C.png",width = 9,height =4)
ggsave("cor1C.pdf",width = 9,height =4)

X <- co2seqC[, setdiff(names(co2seqC), c("T1", "TINJ","Gi","C1"))]
y <- co2seqC$T1
set.seed(123)
rf_model <- randomForest(x = X, y = y, ntree = 50)
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

km <- survfit(Surv(T1,C1) ~ 1, data = co2seqC)
ggsurvplot(km,palette = "blue",risk.table = FALSE,surv.median.line = "hv",xlim=c(0,40),legend="none",break.time.by = 5,xlab="Time, years")

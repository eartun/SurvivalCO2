# Survival Analysis of CO$_2$ Sequestration in Depleted Shale Reservoirs
This repository was created for the peer-reviewed journal article submitted to _Petroleum_:

Artun, E., Al-Amri, A., Kulga, B. 2025. Survival Analysis of CO$_2$ Sequestration in Depleted Shale Reservoirs. _Petroleum_. _under review_

*Descriptions for the coding files:*

- 01eda.R: Exploratory data analysis and data preparation 

- 02km.R: Kaplan-meier analysis

- 03logrank.R: Log-rank test
  
- 04coxph.R: Cox proportional hazards model

- 05constantqinj.R: Fixed-injection rate analysis

*Descriptions for the data files:*

- data.csv: Data set that includes 2547 CO2 sequestration scenarios with variable reservoir and operational parameters, with corresponding cumulative gas injection, and time-to-event variable (T1) which indicates the time that would take until injecting 1 Bscf of CO$_2$

- datac.csv: Same data set with injection rate fixed at 150 Mscf/d in all scenarios. Other input parameters are the same with the other data set.

rm(list = ls(all = TRUE))
library(sqldf)
data=read.csv(file="C:\\Work\\sophie\\cleaner_smu.csv",header=TRUE)
data=sqldf("select q15_admitage, q16_gender, q9_ad_type, q10_ad_from,q18_edyr, q19_employ,q25_1_sx, q25_2_sz_dx, q25_3_quan_sz,q25_4_psycnee,q25_5_physnee,q25_6_optimize,q26_sxmon, q27_2_sps, q27_1_szpre,q27_2_freq_sps, q27_3_cps,q27_3_freq_cps, q27_4_gtc,q27_4_freq_gtc,q27_5_ab,q27_5_freq_abs,q27_6_tonic,q27_6_freq_tonic, q27_7_atonic,q27_7_freq_atonic, q27_8_myoclonic,q27_8_freq_myoclonic,q27_9_freq_psycnee, q27_9_psycnee, q27_10_phynee, q27_10_freq_phynee,q27_11_other,q27_11_freq_other, q27_12_unk,q27_12_freq_unk, seizure_freq,q30_1_sx_induc,q30_2_hypervent, q30_2_hyper_number, q30_3_photic_number, q30_3_photic, q30_4_sleep_dep_number,q30_4_sleep_dep, q30_5_othernumber, q34_aed_red, q34_a_redpread,q36a_1_tegad, q36a_2_clobad,q36a_3_clonad, q36a_4_gabaad, q36a_5_lamad, q36a_6_kepad, q36a_7_atiad, q36a_8_triad, q36a_9_phenad,q36a_11_dilad, q36a_11_lyrad,q36a_12_mysad, q36a_13_topad, q36a_14_epiad, q36a_15_sabad,q36a_16_vnsad,q36a_17_other,q37_psychad,q40_1_szevent,q40_2_sps, q40_3_cps,q40_4_gtc, q40_5_ab,q40_6_tonic,q40_7_atonic,q40_8_myo, q40_9_psycnee from data")
ad_type=0
reason_ref=0
seizure_type=0
med_count=0
ES_out=0
PNES_out=0
data=cbind(data,ad_type,reason_ref,seizure_type,med_count,ES_out,PNES_out)
data[which(data$q9_ad_type==1),]$ad_type=1
data[which(data$q10_ad_from==2),]$ad_type=2
#data[which(data$ad_type==0),]$ad_type=3
data$q16_gender=data$q16_gender-1
#table(data$ad_type,data$q40_1_szevent)
#table(data$q16_gender,data$q40_1_szevent)

data[which(data$q26_sxmon==1),]$q26_sxmon=1
data[which(data$q26_sxmon!=1),]$q26_sxmon=2
data$q26_sxmon=data$q26_sxmon-1
#table(data$q26_sxmon,data$q40_1_szevent)
# data[which(data$q30_2_hypervent==2),]$q30_2_hyper_number=0
# data[which(data$q30_3_photic==2),]$q30_3_photic_number=0
# data[which(data$q30_4_sleep_dep==2),]$q30_4_sleep_dep_number=0
# write.csv(data,file="C:\\Work\\sophie\\data.csv")
# data=read.csv(file="C:\\Work\\sophie\\data.csv",header=TRUE)
#define reason for referral, 1-Epileptic, 2-non epileptic, 3-both
data[which(data$q25_1_sx==1|data$q25_2_sz_dx==1|data$q25_3_quan_sz==1|data$q25_6_optimize==1),]$reason_ref=1
data[which(data$q25_4_psycnee==1|data$q25_5_physnee==1),]$reason_ref=2
data[which((data$q25_1_sx==1|data$q25_2_sz_dx==1|data$q25_3_quan_sz==1|data$q25_6_optimize==1)&(data$q25_4_psycnee==1|data$q25_5_physnee==1)),]$reason_ref=3
data[which(data$reason_ref==0),]$reason_ref=4
data$reason_ref=data$reason_ref-1
#define suizure type,1-Generalized, 2--Focal+-Generalized,  3-NEPS,4-other
data[which(data$q27_2_sps==1|data$q27_3_cps==1),]$seizure_type=2
data[which((data$q27_4_gtc==1|data$q27_5_ab==1|data$q27_6_tonic==1|data$q27_7_atonic==1|data$q27_8_myoclonic==1)& (data$seizure_type!=1)),]$seizure_type=1
data[which((data$q27_9_psycnee==1)& (data$seizure_type!=1)&(data$seizure_type!=2)),]$seizure_type=3
data[which(data$seizure_type==0),]$seizure_type=4
data$seizure_type=data$seizure_type-1
#data[which(data$seizure_type==1),]$seizure_freq=as.numeric(data[which(data$seizure_type==1),]$q27_2_freq_sps)+as.numeric(data[which(data$seizure_type==1),]$q27_3_freq_cps)+as.numeric(data[which(data$seizure_type==1),]$q27_4_freq_gtc)+as.numeric(data[which(data$seizure_type==1),]$q27_5_freq_abs)+as.numeric(data[which(data$seizure_type==1),]$q27_6_freq_tonic)+as.numeric(data[which(data$seizure_type==1),]$q27_7_freq_atonic)+as.numeric(data[which(data$seizure_type==1),]$q27_8_freq_myoclonic)
#data[which(data$seizure_type==2),]$seizure_freq=as.numeric(data[which(data$seizure_type==2),]$q27_4_freq_gtc)+as.numeric(data[which(data$seizure_type==2),]$q27_5_freq_abs)+as.numeric(data[which(data$seizure_type==2),]$q27_6_freq_tonic)+as.numeric(data[which(data$seizure_type==2),]$q27_7_freq_atonic)+as.numeric(data[which(data$seizure_type==2),]$q27_8_freq_myoclonic)
#data[which(data$seizure_type==3),]$seizure_freq=as.numeric(data[which(data$seizure_type==3),]$q27_9_freq_psycnee)+as.numeric(data[which(data$seizure_type==3),]$q27_10_freq_phynee)
#data[which(data$seizure_type==4),]$seizure_freq=as.numeric(data[which(data$seizure_type==4),]$q27_11_freq_other)+as.numeric(data[which(data$seizure_type==4),]$q27_12_freq_unk)
for (i in 1:nrow(data)){data$seizure_freq[i]=min(as.numeric(data$q27_2_freq_sps)[i],as.numeric(data$q27_3_freq_cps)[i],as.numeric(data$q27_4_freq_gtc)[i],as.numeric(data$q27_5_freq_abs)[i],as.numeric(data$q27_6_freq_tonic)[i],as.numeric(data$q27_7_freq_atonic)[i],as.numeric(data$q27_8_freq_myoclonic)[i],as.numeric(data$q27_9_freq_psycnee)[i],as.numeric(data$q27_10_freq_phynee)[i],as.numeric(data$q27_11_freq_other)[i],as.numeric(data$q27_12_freq_unk)[i],na.rm=TRUE)
} 
data[which(data$seizure_freq=="Inf"),]$seizure_freq='NA'
data[which(data$seizure_freq=="9"),]$seizure_freq='NA'

data$seizure_freq=as.numeric(data$seizure_freq)
data[which(data$seizure_freq==5),]$seizure_freq=4
data=data[-which(data$reason_ref==3),]
data$seizure_freq=4-data$seizure_freq

#data[which(data$q27_11_other==1|data$q27_12_unk==1),]$seizure_type=4
#table(data$reason_ref,data$q40_1_szevent)
#table(data$seizure_type,data$q40_1_szevent)
#data[which(data$q27_2_sps!=1&data$q27_3_cps!=1&data$q27_4_gtc!=1&data$q27_5_ab!=1&data$q27_6_tonic!=1&data$q27_7_atonic!=1&data$q27_8_myoclonic!=1&data$q27_9_psycnee!=1&data$q27_10_phynee!=1&data$q27_11_other!=1&data$q27_12_unk!=1),]$seizure_type=2
data[which(data$q34_aed_red==1),]$q34_aed_red=6
data[which(data$q34_aed_red==2),]$q34_aed_red=2
data[which(data$q34_aed_red==3|data$q34_aed_red==4),]$q34_aed_red=3

data[which(data$q34_aed_red==5),]$q34_aed_red=1
data[which(data$q34_aed_red==6),]$q34_aed_red=4
data$q34_aed_red=data$q34_aed_red-1
#table(data$q34_aed_red,data$q40_1_szevent)
data[which(data$q36a_1_tegad==2),]$q36a_1_tegad=0
data[which(data$q36a_2_clobad==2),]$q36a_2_clobad=0
data[which(data$q36a_3_clonad==2),]$q36a_3_clonad=0
data[which(data$q36a_4_gabaad==2),]$q36a_4_gabaad=0
data[which(data$q36a_5_lamad==2),]$q36a_5_lamad=0
data[which(data$q36a_6_kepad==2),]$q36a_6_kepad=0
data[which(data$q36a_7_atiad==2),]$q36a_7_atiad=0
data[which(data$q36a_8_triad==2),]$q36a_8_triad=0
data[which(data$q36a_9_phenad==2),]$q36a_9_phenad=0
data[which(data$q36a_11_dilad==2),]$q36a_11_dilad=0
data[which(data$q36a_11_lyrad==2),]$q36a_11_lyrad=0
data[which(data$q36a_12_mysad==2),]$q36a_12_mysad=0
data[which(data$q36a_13_topad==2),]$q36a_13_topad=0
data[which(data$q36a_14_epiad==2),]$q36a_14_epiad=0
data[which(data$q36a_15_sabad==2),]$q36a_15_sabad=0
data[which(data$q36a_16_vnsad==2),]$q36a_16_vnsad=0
data[which(data$q36a_17_other==2),]$q36a_17_other=0
data$med_count=data$q36a_1_tegad+data$q36a_2_clobad+data$q36a_3_clonad+data$q36a_4_gabaad+data$q36a_5_lamad+data$q36a_6_kepad+data$q36a_7_atiad+data$q36a_8_triad+data$q36a_9_phenad+data$q36a_11_dilad+data$q36a_11_lyrad+data$q36a_12_mysad+data$q36a_13_topad+data$q36a_14_epiad+data$q36a_15_sabad+data$q36a_16_vnsad+data$q36a_17_other
#table(data$med_count,data$q40_1_szevent)
data$q40_1_szevent=abs(data$q40_1_szevent-2)
data$q30_1_sx_induc=abs(data$q30_1_sx_induc-2)
data$q30_2_hypervent=abs(data$q30_2_hypervent-2)
data$q30_4_sleep_dep=abs(data$q30_4_sleep_dep-2)
data$q30_3_photic=abs(data$q30_3_photic-2)
data$q34_a_redpread=abs(data$q34_a_redpread-2)
data$q37_psychad=abs(data$q37_psychad-2)
data$q19_employ=data$q19_employ-1
####full model####
fullmodel=glm(factor(q40_1_szevent) ~ factor(ad_type) +q15_admitage +factor(q16_gender)+q18_edyr+factor(q19_employ)+factor(reason_ref)+factor(q26_sxmon)+factor(seizure_type)+factor(seizure_freq)+factor(q30_1_sx_induc)+factor(q30_4_sleep_dep)+factor(q30_2_hypervent)+factor(q30_3_photic)+factor(q34_aed_red)+factor(q34_a_redpread)+med_count+factor(q37_psychad), data = data, family = "binomial")
exp(cbind(OR = coef(fullmodel), confint(fullmodel)))

fullmodel=glm(factor(q27_1_szpre)~ factor(ad_type) +q15_admitage +factor(q16_gender)+q18_edyr+factor(q19_employ)+factor(reason_ref)+factor(q26_sxmon)+factor(seizure_type)+factor(seizure_freq)+factor(q30_1_sx_induc)+factor(q30_4_sleep_dep)+factor(q30_2_hypervent)+factor(q30_3_photic)+factor(q34_aed_red)+factor(q34_a_redpread)+med_count+factor(q37_psychad), data = data, family = "binomial")
exp(cbind(OR = coef(fullmodel), confint(fullmodel)))
# data[which(data$q40_2_sps==1|data$q40_3_cps==1|data$q40_4_gtc==1|data$q40_5_ab==1|data$q40_6_tonic==1|data$q40_7_atonic==1),]$ES_out=1
# data[which(data$q40_2_sps!=1&data$q40_3_cps!=1&data$q40_4_gtc!=1&data$q40_5_ab!=1&data$q40_6_tonic!=1&data$q40_7_atonic!=1),]$ES_out=2
# ESdata=data[which(data$ES_out==1 |data$ES_out==2),]
# data[which(data$q40_9_psycnee==1),]$PNES_out=1




####ES_model####

#nodata=data[which(data$q40_1_szevent==0 ),]
data[which(data$q40_2_sps==1|data$q40_3_cps==1|data$q40_4_gtc==1|data$q40_5_ab==1|data$q40_6_tonic==1|data$q40_7_atonic==1|data$q40_8_myo==1),]$ES_out=1
data[which(data$q40_2_sps!=1&data$q40_3_cps!=1&data$q40_4_gtc!=1&data$q40_5_ab!=1&data$q40_6_tonic!=1&data$q40_7_atonic!=1&data$q40_8_myo!=1),]$ES_out=0

#Define ES using Q27
# data[which(data$q27_2_sps==1|data$q27_3_cps==1|data$q27_4_gtc==1|data$q27_5_ab==1|data$q27_6_tonic==1|data$q27_7_atonic==1|data$q27_8_myoclonic==1),]$ES_out=1
# data[which(data$q27_2_sps!=1&data$q27_3_cps!=1&data$q27_4_gtc!=1&data$q27_5_ab!=1&data$q27_6_tonic!=1&data$q27_7_atonic!=1&data$q27_8_myoclonic!=1),]$ES_out=2
# 
data[which(data$q40_2_sps==1|data$q40_3_cps==1|data$q40_4_gtc==1|data$q40_5_ab==1|data$q40_6_tonic==1|data$q40_7_atonic==1|data$q40_8_myo==1),]$ES_out=1
data[which(data$q40_2_sps!=1&data$q40_3_cps!=1&data$q40_4_gtc!=1&data$q40_5_ab!=1&data$q40_6_tonic!=1&data$q40_7_atonic!=1&data$q40_8_myo!=1),]$ES_out=0

ES_out1=0
data=cbind(data,ES_out1)
data[which(data$q25_1_sx==1|data$q25_2_sz_dx==1|data$q25_3_quan_sz==1|data$q25_6_optimize==1),]$ES_out1=1

# data=data[-which(data$ES_out==0),]
# data[which(data$ES_out==2),]$ES_out=0
data1=sqldf("select ES_out,ES_out1,ad_type, q15_admitage,q16_gender,q18_edyr,q19_employ,reason_ref, q26_sxmon,seizure_type,seizure_freq, q30_4_sleep_dep,q30_2_hypervent,q30_1_sx_induc,q30_3_photic, q34_aed_red,q34_a_redpread, med_count,q37_psychad, q40_1_szevent from data ")
data1=na.omit(data1)

ESmodel=glm(factor(ES_out) ~ factor(ad_type) +q15_admitage +factor(q16_gender)+q18_edyr+factor(q19_employ)+factor(reason_ref)+factor(q26_sxmon)+factor(seizure_type)+factor(seizure_freq)+factor(q30_1_sx_induc)+factor(q30_4_sleep_dep)+factor(q30_2_hypervent)+factor(q30_3_photic)+factor(q34_aed_red)+factor(q34_a_redpread)+med_count+factor(q37_psychad), data = data, family = "binomial")
ESmodel=glm(factor(ES_out) ~ factor(ad_type) +q15_admitage +factor(q16_gender)+q18_edyr+factor(q19_employ)+factor(q26_sxmon)+factor(seizure_freq)+factor(q30_1_sx_induc)+factor(q30_4_sleep_dep)+factor(q30_2_hypervent)+factor(q30_3_photic)+factor(q34_aed_red)+factor(q34_a_redpread)+med_count+factor(q37_psychad), data = data1, family = "binomial")
summary(ESmodel)
exp(cbind(OR = coef(ESmodel), confint(ESmodel)))
bESmodel=glm(factor(ES_out) ~ factor(ad_type) +q15_admitage +factor(q16_gender)+q18_edyr+factor(q19_employ)+factor(reason_ref)+factor(q26_sxmon)+factor(seizure_type)+as.numeric(seizure_freq)+factor(q30_4_sleep_dep)+factor(q30_2_hypervent)+factor(q30_3_photic)+factor(q34_aed_red)+factor(q34_a_redpread)+med_count+factor(q37_psychad), data = data1, family = "binomial")
backwardsES=step(bESmodel)
summary(backwardsES)
exp(cbind(OR = coef(backwardsES), confint(backwardsES)))
####PNES_model####

data[which(data$q40_9_psycnee==1),]$PNES_out=1
data[which(data$q40_9_psycnee!=1),]$PNES_out=0


data[which(data$q27_9_psycnee==1),]$PNES_out=1
data[which(data$q27_9_psycnee!=1),]$PNES_out=0
data2=sqldf("select PNES_out,ad_type, q15_admitage,q16_gender,q18_edyr,q19_employ,reason_ref,seizure_type,seizure_freq, q30_1_sx_induc,q30_4_sleep_dep,q30_2_hypervent,q30_3_photic, q34_aed_red,q34_a_redpread,med_count,q37_psychad, q40_1_szevent from data ")
data2=na.omit(data2)

PNESmodel=glm(factor(PNES_out) ~ factor(ad_type) +q15_admitage +factor(q16_gender)+q18_edyr+factor(q19_employ)+factor(seizure_freq)+factor(q30_1_sx_induc)+factor(q30_4_sleep_dep)+factor(q30_2_hypervent)+factor(q30_3_photic)+factor(q34_aed_red)+factor(q34_a_redpread)+med_count+factor(q37_psychad), data = data, family = "binomial")
summary(PNESmodel)
exp(cbind(OR = coef(PNESmodel), confint(PNESmodel)))
bPNESmodel=glm(factor(PNES_out) ~ factor(ad_type) +q15_admitage +factor(q16_gender)+q18_edyr+factor(q19_employ)+factor(seizure_type)+as.numeric(seizure_freq)+factor(q30_4_sleep_dep)+factor(q30_2_hypervent)+factor(q30_3_photic)+factor(q34_aed_red)+factor(q34_a_redpread)+med_count+factor(q37_psychad), data = data2, family = "binomial")
backwardsPNES=step(bPNESmodel)
summary(backwardsPNES)
exp(cbind(OR = coef(backwardsPNES), confint(backwardsPNES)))
data2[which(data2$seizure_type==3|data2$seizure_type==4),]$seizure_type=3

table(data2$PNES_out,data2$seizure_type)
library(logistf)
bPNESmodel=logistf(factor(PNES_out) ~ factor(ad_type) +factor(q16_gender)+q18_edyr+factor(seizure_type)+factor(q34_aed_red)+factor(q34_a_redpread)+factor(q37_psychad), data = data2, family = "binomial")

data1=sqldf("select ES_out,ad_type, q15_admitage,q16_gender,q18_edyr,q19_employ,reason_ref, q26_sxmon,seizure_type,seizure_freq, q30_4_sleep_dep,q30_2_hypervent,q30_1_sx_induc,q30_3_photic, q34_aed_red,q34_a_redpread, med_count,q37_psychad, q40_1_szevent from data ")
data1=na.omit(data1)
fullmodel=glm(factor(q40_1_szevent) ~ factor(ad_type) +q15_admitage +factor(q16_gender)+q18_edyr+factor(q19_employ)+factor(reason_ref)+factor(q26_sxmon)+factor(seizure_type)+factor(seizure_freq)+factor(q30_1_sx_induc)+factor(q30_4_sleep_dep)+factor(q30_2_hypervent)+factor(q30_3_photic)+factor(q34_aed_red)+factor(q34_a_redpread)+med_count+factor(q37_psychad), data = data1, family = "binomial")
backwards=step(fullmodel)
exp(cbind(OR = coef(backwards), confint(backwards)))

#which(data$ES_out==1&data$PNES_out==1)
library(car)


polyserial(as.matrix(data1$seizure_freq), as.matrix(data1$q34_aed_red))

table(data$q40_1_szevent)
table(data$q40_1_szevent,data$q16_gender)
table(data$q40_1_szevent,data$ad_type)
table(data$q40_1_szevent,data$q26_sxmon)
table(data$q40_1_szevent,data$reason_ref)
table(data$q40_1_szevent,data$seizure_type)
table(data$q40_1_szevent,data$q34_aed_red)
table(data$q40_1_szevent,data$q30_1_sx_induc)

table(data$ES_out)
table(data$ES_out,data$q16_gender)
table(data$ES_out,data$ad_type)
table(data$ES_out,data$q26_sxmon)
table(data$ES_out,data$reason_ref)
table(data$ES_out,data$seizure_type)
table(data$ES_out,data$q34_aed_red)

table(data1$ES_out)
table(data1$ES_out,data1$q16_gender)
table(data1$ES_out,data1$ad_type)
table(data1$ES_out,data1$q26_sxmon)
table(data1$ES_out,data1$reason_ref)
table(data1$ES_out,data1$seizure_type)
table(data1$ES_out,data1$q34_aed_red)
 table(data$ES_out,data$q30_1_sx_induc)


table(data$PNES_out)
table(data$PNES_out,data$q16_gender)
table(data$PNES_out,data$ad_type)
table(data$PNES_out,data$reason_ref)
table(data$PNES_out,data$seizure_type)
table(data$PNES_out,data$q34_aed_red)
table(data$PNES_out,data$q30_1_sx_induc)

table(data2$PNES_out)
table(data2$PNES_out,data2$q16_gender)
table(data2$PNES_out,data2$ad_type)
table(data2$PNES_out,data2$reason_ref)
table(data2$PNES_out,data2$seizure_type)
table(data2$PNES_out,data2$q34_aed_red)


table(data$q27_2_sps==1&data$q40_2_sps==1)
table(data$q27_3_cps==1&data$q40_3_cps==1)
table(data$q27_4_gtc==1&data$q40_4_gtc==1)
table(data$q27_5_ab==1&data$q40_5_ab==1)
table(data$q27_6_tonic==1&data$q40_6_tonic==1)
table(data$q27_7_atonic==1&data$q40_7_atonic==1)
table(data$q27_8_myoclonic==1&data$q40_8_myo==1)
table(data$q27_9_psycnee==1&data$q40_9_psycnee==1)

library(irr)
kappa2(cbind(data$q27_2_sps,data$q40_2_sps), "unweighted")
kappa2(cbind(data$q27_3_cps,data$q40_3_cps), "unweighted")
kappa2(cbind(data$q27_4_gtc,data$q40_4_gtc),"unweighted")
kappa2(cbind(data$q27_5_ab,data$q40_5_ab),"unweighted")
kappa2(cbind(data$q27_6_tonic,data$q40_6_tonic),"unweighted")
kappa2(cbind(data$q27_7_atonic,data$q40_7_atonic),"unweighted")
kappa2(cbind(data$q27_8_myoclonic,data$q40_8_myo),"unweighted")
kappa2(cbind(data$q27_9_psycnee,data$q40_9_psycnee),"unweighted")

library(psych)
cohen.kappa(cbind(data$q27_2_sps,data$q40_2_sps))
cohen.kappa(cbind(data$q27_3_cps,data$q40_3_cps))
cohen.kappa(cbind(data$q27_4_gtc,data$q40_4_gtc))
cohen.kappa(cbind(data$q27_5_ab,data$q40_5_ab))
cohen.kappa(cbind(data$q27_6_tonic,data$q40_6_tonic))

cohen.kappa(cbind(data$q27_7_atonic,data$q40_7_atonic))
cohen.kappa(cbind(data$q27_8_myoclonic,data$q40_8_myo))

cohen.kappa(cbind(data$q27_9_psycnee,data$q40_9_psycnee))

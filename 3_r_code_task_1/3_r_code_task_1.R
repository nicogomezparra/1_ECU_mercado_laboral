# Author: NICOLAS GOMEZ PARRA
# Labor market analysis for Ecuador in Q4-2013
# Date: March 31st, 2021

# Change code to the folder of the script if not defined autmoatically
getwd()
setwd("/Users/nicolasgomezparra/Dropbox/1_ECU_mercado_laboral/3_r_code_task_1")

# Libraries
library(haven)
library(dplyr)
library(survey)
library(openxlsx)
#Import data
ECU <- read_dta("../datastore/Ecuador_panel_test.dta")
summary(ECU)
#Transform strings to num 
ECU <- as.data.frame(apply(ECU,2,as.numeric))
sapply(ECU,class)

#recode missing values
ECU$p47b[ECU$p47b==99] <-NA
ECU$p33[ECU$p33==999] <-NA
ECU$p51a[ECU$p51a==999] <-NA
ECU$p51b[ECU$p51b==999] <-NA
ECU$p51c[ECU$p51c==999] <-NA
ECU$p63[ECU$p63==999999] <-NA
ECU$p64b[ECU$p64b==999999] <-NA
ECU$p65[ECU$p65==999999] <-NA
ECU$p66[ECU$p66==999999] <-NA
ECU$p67[ECU$p67==999999] <-NA
ECU$p69[ECU$p69==999999] <-NA
ECU$p72b[ECU$p72b==999999] <-NA
#define samples
ECU$sample1 <- (!is.na(ECU$sexo) & ECU$edad>14 & ECU$edad<65 & ECU$ano==2013 & ECU$trim==4)
ECU$sample2 <- (ECU$sexo==1 & ECU$edad>14 & ECU$edad<65 & ECU$ano==2013 & ECU$trim==4)
ECU$sample3 <- (ECU$sexo==2 & ECU$edad>14 & ECU$edad<65 & ECU$ano==2013 & ECU$trim==4)
ECU$sample4 <- (!is.na(ECU$sexo) & ECU$edad>64 & ECU$ano==2013 & ECU$trim==4)
ECU$sample5 <- (ECU$sexo==1 & ECU$edad>64 & ECU$ano==2013 & ECU$trim==4)
ECU$sample6 <- (ECU$sexo==2 & ECU$edad>64 & ECU$ano==2013 & ECU$trim==4)
ECU$sample7 <- (!is.na(ECU$sexo) & ECU$edad>14 & ECU$edad<25 & ECU$ano==2013 & ECU$trim==4)
ECU$sample8 <- (!is.na(ECU$sexo) & ECU$edad>24 & ECU$edad<65 & ECU$ano==2013 & ECU$trim==4)

#prepare data for aggregation at the national level
ECU$pea[ECU$condact==7] <- 0 
ECU$po <- as.integer(ECU$condact>=0 & ECU$condact<=3)
ECU$pd <- as.integer(ECU$condact>=5 & ECU$condact<=7)
ECU$pd[ECU$pea==0] <- NA
ECU$pd_ld <- as.integer(ECU$p33>=52 & ECU$p33<=100000000)*ECU$pd
ECU$pd_ld[is.na(ECU$pd_ld) & !is.na(ECU$pd)] <- 0
ECU$po_form1[ECU$po<1] <- ECU$socsec[ECU$po!=1]
ECU$po_form2[ECU$p42>0 & ECU$p42<4 & !is.na(ECU$p42)] <- ECU$socsec[ECU$p42>0 & ECU$p42<4 & !is.na(ECU$p42)]
ECU$po_auto <- as.integer(ECU$p42==6)
ECU$po_auto[ECU$po<1] <- NA
ECU$po_sub <- as.integer(ECU$condact>=2 & ECU$condact<=3)
ECU$po_sub[ECU$pea<1] <- NA
ECU$ing_princ <- ECU$p66
ECU$ing_princ[!(ECU$p42>=1 & ECU$p42<=3 & ECU$po>0)] <- NA
ECU$ing_princ[ECU$p42>=5 & ECU$p42<=6 & ECU$po>0 & !is.na(ECU$p63)] <- ECU$p63[ECU$p42>=5 & ECU$p42<=6 & ECU$po>0 & !is.na(ECU$p63)]
ECU$ing_total <- ECU$ing_prin
ECU$ing_total[ECU$po<1] <- NA
ECU$ing_total[ECU$p51b>=1 & !is.na(ECU$p51b) & !is.na(ECU$ing_total) & !is.na(ECU$p69)] <- ECU$ing_total[ECU$p51b>=1 & !is.na(ECU$p51b) & !is.na(ECU$ing_total) & !is.na(ECU$p69)] + ECU$p69[ECU$p51b>=1 & !is.na(ECU$p51b) & !is.na(ECU$ing_total) & !is.na(ECU$p69)]
ECU$ing_salmm[!is.na(ECU$ing_total)] <- as.integer(ECU$ing_tota[!is.na(ECU$ing_total)]<ECU$salmm_ci[!is.na(ECU$ing_total)])
ECU$educano[ECU$nivinst==0] <- 0
ECU$educano[ECU$nivinst==1 & !is.na(ECU$nivinst)] <- ECU$anoinst[ECU$nivinst==1 & !is.na(ECU$nivinst)]
ECU$educano[ECU$nivinst==2 & !is.na(ECU$nivinst)] <- 4 + ECU$anoinst[ECU$nivinst==2 & !is.na(ECU$nivinst)]
ECU$educano[ECU$nivinst==4 & !is.na(ECU$nivinst)] <- 4 + ECU$anoinst[ECU$nivinst==4 & !is.na(ECU$nivinst)]
ECU$educano[ECU$nivinst==5 & !is.na(ECU$nivinst)] <- 4 + ECU$anoinst[ECU$nivinst==5 & !is.na(ECU$nivinst)]
ECU$educano[ECU$nivinst==6 & !is.na(ECU$nivinst)] <- 10 + ECU$anoinst[ECU$nivinst==6 & !is.na(ECU$nivinst)]
ECU$educano[ECU$nivinst==7 & !is.na(ECU$nivinst)] <- 13 + ECU$anoinst[ECU$nivinst==7 & !is.na(ECU$nivinst)]
ECU$educano[ECU$nivinst==8 & !is.na(ECU$nivinst)] <- 16 + ECU$anoinst[ECU$nivinst==8 & !is.na(ECU$nivinst)]
ECU$educano[ECU$nivinst==9 & !is.na(ECU$nivinst)] <- 16 + ECU$anoinst[ECU$nivinst==9 & !is.na(ECU$nivinst)]
ECU$educano[ECU$nivinst==10 & !is.na(ECU$nivinst)] <- 21 + ECU$anoinst[ECU$nivinst==10 & !is.na(ECU$nivinst)]
ECU$emp5less[ECU$p42>=1 & ECU$p42<=3 & !is.na(ECU$p47b)] <- as.integer(ECU$p47b[ECU$p42>=1 & ECU$p42<=3 & !is.na(ECU$p47b)]>=0 & ECU$p47b[ECU$p42>=1 & ECU$p42<=3 & !is.na(ECU$p47b)]<=5.5)
ECU$p65_pens[ECU$edad>64 & !is.na(ECU$p72a)] <- as.integer(ECU$p72a[ECU$edad>64 & !is.na(ECU$p72a)]==1)
summary(ECU)
# set survey settings
ECUsrvy <- svydesign(id = ~id_pers,weights = ~fexp,data = ECU)

# Calculate statistics - first columns
t1 <- svyby(~pea,~sample1,ECUsrvy,svytotal,na.rm = T)
t2 <- svyby(~pea,~sample1,ECUsrvy,svymean,na.rm = T)
t3 <- svyby(~po,~sample1,ECUsrvy,svytotal,na.rm = T)
t4 <- svyby(~po,~sample1,ECUsrvy,svymean,na.rm = T)
t5 <- svyby(~pd,~sample1,ECUsrvy,svytotal,na.rm = T)
t6 <- svyby(~pd,~sample1,ECUsrvy,svymean,na.rm = T)
t7 <- svyby(~pd_ld,~sample1,ECUsrvy,svymean,na.rm = T)
t8 <- svyby(~po_form1,~sample1,ECUsrvy,svymean,na.rm = T)
t9 <- svyby(~po_form2,~sample1,ECUsrvy,svymean,na.rm = T)
t10 <- svyby(~po_auto,~sample1,ECUsrvy,svymean,na.rm = T)
t11 <- svyby(~po_sub,~sample1,ECUsrvy,svymean,na.rm = T)
t12 <- svyby(~ing_princ,~sample1,ECUsrvy,svymean,na.rm = T)
t13 <- svyby(~ing_total,~sample1,ECUsrvy,svymean,na.rm = T)
t14 <- svyby(~ing_salmm,~sample1,ECUsrvy,svymean,na.rm = T)
t15 <- svyby(~educano,~sample1,ECUsrvy,svymean,na.rm = T)
t16 <- svyby(~emp5less,~sample1,ECUsrvy,svymean,na.rm = T)
t17 <- svyby(~p65_pens,~sample4,ECUsrvy,svymean,na.rm = T)
t18 <- svyby(~p72b,~sample4,ECUsrvy,svymean,na.rm = T)
t19 <- svyby(~salmm_ci,~sample4,ECUsrvy,svymean,na.rm = T)
t22 <- t18[2,2]/t19[2,2]

# Export values
results1 <- c(t1[2,2],t2[2,2],t3[2,2],t4[2,2],t5[2,2],t6[2,2],t7[2,2],t8[2,2],t9[2,2],t10[2,2],t11[2,2],t12[2,2],t13[2,2],t14[2,2],t15[2,2],t16[2,2],t17[2,2],t22)
table <- cbind(results1)

#Load Excel template
exc <-loadWorkbook("Tabla de Indicadores in R.xlsx")
writeData(exc,sheet = "1.Descriptivos",table,startCol = 2,startRow = 5,colNames = F)

# Calculate statistics - second column
ECU$sample1 <- ECU$sample2
ECU$sample4 <- ECU$sample5
ECUsrvy <- svydesign(id = ~id_pers,weights = ~fexp,data = ECU)
t1 <- svyby(~pea,~sample1,ECUsrvy,svytotal,na.rm = T)
t2 <- svyby(~pea,~sample1,ECUsrvy,svymean,na.rm = T)
t3 <- svyby(~po,~sample1,ECUsrvy,svytotal,na.rm = T)
t4 <- svyby(~po,~sample1,ECUsrvy,svymean,na.rm = T)
t5 <- svyby(~pd,~sample1,ECUsrvy,svytotal,na.rm = T)
t6 <- svyby(~pd,~sample1,ECUsrvy,svymean,na.rm = T)
t7 <- svyby(~pd_ld,~sample1,ECUsrvy,svymean,na.rm = T)
t8 <- svyby(~po_form1,~sample1,ECUsrvy,svymean,na.rm = T)
t9 <- svyby(~po_form2,~sample1,ECUsrvy,svymean,na.rm = T)
t10 <- svyby(~po_auto,~sample1,ECUsrvy,svymean,na.rm = T)
t11 <- svyby(~po_sub,~sample1,ECUsrvy,svymean,na.rm = T)
t12 <- svyby(~ing_princ,~sample1,ECUsrvy,svymean,na.rm = T)
t13 <- svyby(~ing_total,~sample1,ECUsrvy,svymean,na.rm = T)
t14 <- svyby(~ing_salmm,~sample1,ECUsrvy,svymean,na.rm = T)
t15 <- svyby(~educano,~sample1,ECUsrvy,svymean,na.rm = T)
t16 <- svyby(~emp5less,~sample1,ECUsrvy,svymean,na.rm = T)
t17 <- svyby(~p65_pens,~sample4,ECUsrvy,svymean,na.rm = T)
t18 <- svyby(~p72b,~sample4,ECUsrvy,svymean,na.rm = T)
t19 <- svyby(~salmm_ci,~sample4,ECUsrvy,svymean,na.rm = T)
t22 <- t18[2,2]/t19[2,2]

# Export values
results1 <- c(t1[2,2],t2[2,2],t3[2,2],t4[2,2],t5[2,2],t6[2,2],t7[2,2],t8[2,2],t9[2,2],t10[2,2],t11[2,2],t12[2,2],t13[2,2],t14[2,2],t15[2,2],t16[2,2],t17[2,2],t22)
table <- cbind(results1)

#Load Excel template
writeData(exc,sheet = "1.Descriptivos",table,startCol = 3,startRow = 5,colNames = F)

# Calculate statistics - third column
ECU$sample1 <- ECU$sample3
ECU$sample4 <- ECU$sample6
ECUsrvy <- svydesign(id = ~id_pers,weights = ~fexp,data = ECU)
t1 <- svyby(~pea,~sample1,ECUsrvy,svytotal,na.rm = T)
t2 <- svyby(~pea,~sample1,ECUsrvy,svymean,na.rm = T)
t3 <- svyby(~po,~sample1,ECUsrvy,svytotal,na.rm = T)
t4 <- svyby(~po,~sample1,ECUsrvy,svymean,na.rm = T)
t5 <- svyby(~pd,~sample1,ECUsrvy,svytotal,na.rm = T)
t6 <- svyby(~pd,~sample1,ECUsrvy,svymean,na.rm = T)
t7 <- svyby(~pd_ld,~sample1,ECUsrvy,svymean,na.rm = T)
t8 <- svyby(~po_form1,~sample1,ECUsrvy,svymean,na.rm = T)
t9 <- svyby(~po_form2,~sample1,ECUsrvy,svymean,na.rm = T)
t10 <- svyby(~po_auto,~sample1,ECUsrvy,svymean,na.rm = T)
t11 <- svyby(~po_sub,~sample1,ECUsrvy,svymean,na.rm = T)
t12 <- svyby(~ing_princ,~sample1,ECUsrvy,svymean,na.rm = T)
t13 <- svyby(~ing_total,~sample1,ECUsrvy,svymean,na.rm = T)
t14 <- svyby(~ing_salmm,~sample1,ECUsrvy,svymean,na.rm = T)
t15 <- svyby(~educano,~sample1,ECUsrvy,svymean,na.rm = T)
t16 <- svyby(~emp5less,~sample1,ECUsrvy,svymean,na.rm = T)
t17 <- svyby(~p65_pens,~sample4,ECUsrvy,svymean,na.rm = T)
t18 <- svyby(~p72b,~sample4,ECUsrvy,svymean,na.rm = T)
t19 <- svyby(~salmm_ci,~sample4,ECUsrvy,svymean,na.rm = T)
t22 <- t18[2,2]/t19[2,2]

# Export values
results1 <- c(t1[2,2],t2[2,2],t3[2,2],t4[2,2],t5[2,2],t6[2,2],t7[2,2],t8[2,2],t9[2,2],t10[2,2],t11[2,2],t12[2,2],t13[2,2],t14[2,2],t15[2,2],t16[2,2],t17[2,2],t22)
table <- cbind(results1)

#Load Excel template
writeData(exc,sheet = "1.Descriptivos",table,startCol = 4,startRow = 5,colNames = F)

# Calculate statistics - fourth column
ECU$sample1 <- ECU$sample7
ECUsrvy <- svydesign(id = ~id_pers,weights = ~fexp,data = ECU)
t1 <- svyby(~pea,~sample1,ECUsrvy,svytotal,na.rm = T)
t2 <- svyby(~pea,~sample1,ECUsrvy,svymean,na.rm = T)
t3 <- svyby(~po,~sample1,ECUsrvy,svytotal,na.rm = T)
t4 <- svyby(~po,~sample1,ECUsrvy,svymean,na.rm = T)
t5 <- svyby(~pd,~sample1,ECUsrvy,svytotal,na.rm = T)
t6 <- svyby(~pd,~sample1,ECUsrvy,svymean,na.rm = T)
t7 <- svyby(~pd_ld,~sample1,ECUsrvy,svymean,na.rm = T)
t8 <- svyby(~po_form1,~sample1,ECUsrvy,svymean,na.rm = T)
t9 <- svyby(~po_form2,~sample1,ECUsrvy,svymean,na.rm = T)
t10 <- svyby(~po_auto,~sample1,ECUsrvy,svymean,na.rm = T)
t11 <- svyby(~po_sub,~sample1,ECUsrvy,svymean,na.rm = T)
t12 <- svyby(~ing_princ,~sample1,ECUsrvy,svymean,na.rm = T)
t13 <- svyby(~ing_total,~sample1,ECUsrvy,svymean,na.rm = T)
t14 <- svyby(~ing_salmm,~sample1,ECUsrvy,svymean,na.rm = T)
t15 <- svyby(~educano,~sample1,ECUsrvy,svymean,na.rm = T)
t16 <- svyby(~emp5less,~sample1,ECUsrvy,svymean,na.rm = T)

# Export values
results1 <- c(t1[2,2],t2[2,2],t3[2,2],t4[2,2],t5[2,2],t6[2,2],t7[2,2],t8[2,2],t9[2,2],t10[2,2],t11[2,2],t12[2,2],t13[2,2],t14[2,2],t15[2,2],t16[2,2])
table <- cbind(results1)

#Load Excel template
writeData(exc,sheet = "1.Descriptivos",table,startCol = 5,startRow = 5,colNames = F)

# Calculate statistics - fifth column
ECU$sample1 <- ECU$sample8
ECUsrvy <- svydesign(id = ~id_pers,weights = ~fexp,data = ECU)
t1 <- svyby(~pea,~sample1,ECUsrvy,svytotal,na.rm = T)
t2 <- svyby(~pea,~sample1,ECUsrvy,svymean,na.rm = T)
t3 <- svyby(~po,~sample1,ECUsrvy,svytotal,na.rm = T)
t4 <- svyby(~po,~sample1,ECUsrvy,svymean,na.rm = T)
t5 <- svyby(~pd,~sample1,ECUsrvy,svytotal,na.rm = T)
t6 <- svyby(~pd,~sample1,ECUsrvy,svymean,na.rm = T)
t7 <- svyby(~pd_ld,~sample1,ECUsrvy,svymean,na.rm = T)
t8 <- svyby(~po_form1,~sample1,ECUsrvy,svymean,na.rm = T)
t9 <- svyby(~po_form2,~sample1,ECUsrvy,svymean,na.rm = T)
t10 <- svyby(~po_auto,~sample1,ECUsrvy,svymean,na.rm = T)
t11 <- svyby(~po_sub,~sample1,ECUsrvy,svymean,na.rm = T)
t12 <- svyby(~ing_princ,~sample1,ECUsrvy,svymean,na.rm = T)
t13 <- svyby(~ing_total,~sample1,ECUsrvy,svymean,na.rm = T)
t14 <- svyby(~ing_salmm,~sample1,ECUsrvy,svymean,na.rm = T)
t15 <- svyby(~educano,~sample1,ECUsrvy,svymean,na.rm = T)
t16 <- svyby(~emp5less,~sample1,ECUsrvy,svymean,na.rm = T)

# Export values
results1 <- c(t1[2,2],t2[2,2],t3[2,2],t4[2,2],t5[2,2],t6[2,2],t7[2,2],t8[2,2],t9[2,2],t10[2,2],t11[2,2],t12[2,2],t13[2,2],t14[2,2],t15[2,2],t16[2,2])
table <- cbind(results1)

#Load Excel template
writeData(exc,sheet = "1.Descriptivos",table,startCol = 6,startRow = 5,colNames = F)
saveWorkbook(exc,"Tabla de Indicadores in R.xlsx",overwrite = T)
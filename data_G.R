#---- PCA data 분석 준비하기 ----

# 원본 데이터 (인턴 수집) 2957명 이었음

# 1. 데이터 준비하기

setwd("C:/Users/jaege/Desktop/PCA/Work2")

# 2. 패키지 설치 및 로드하기

library(tidyverse)
library(caret)

raw_data <- read.csv("PCA_data_0812.csv", header=TRUE)

head(raw_data)
str(raw_data)

# categorical variable
# continuous variable

# 전체 2680명 data 수집됨

data <- filter(raw_data, age > 18)  # 19세 미만 23명 제거 후 2680명에서 2657명 남음
data <- data %>% filter(type_an == 1) # total 2657명 => G/A 2149명

# 24. PONV
# 0. 없음 1. 있음

class(data$PONV)
data$PONV <- factor(data$PONV, levels=c(0,1), labels=c("no PONV","PONV"))
table(data$PONV)
table(is.na(data$PONV))

data_noPONV <- data %>% filter(PONV=="no PONV")
data_PONV <- data %>% filter(PONV=="PONV")

# 1. mon (Month)
table(data$mon)
table(is.na(data$mon))
data$mon <- factor(data$mon, levels=c("2019년 7월", "2019년 8월", "2019년 9월", "2019년 10월", "2019년 11월", "2019년 12월", "2020년 1월", "2020년 2월", "2020년 3월", "2020년 4월", "2020년 5월", "2020년 6월", "2020년 7월", order=T))

# 2. dept (Department)
class(data$dept)

table(data$dept)
table(is.na(data$dept))
summary(data$dept)

# 3. age
hist(data$age)
summary(data$age)
table(is.na(data$age))

table(is.na(data_noPONV$age))
hist(data_noPONV$age)
summary(data_noPONV$age)

table(is.na(data_PONV$age))
hist(data_PONV$age)
summary(data_PONV$age)

boxplot(age ~ PONV, data = data, col = c("brown2", "deepskyblue"))
var.test(age ~ PONV, data = data)
t.test(age ~ PONV, data = data, var.equal=TRUE)

# 4. sex
# F = 0, M = 1
class(data$sex)

str(data)
table(data$sex)
table(is.na(data$sex))

qplot(data$sex0)
df_sex <- data %>% group_by(sex) %>% summarise(total = n())
df_sex.1 <- df_sex %>% mutate(percent = total / sum(total))
df_sex.1

table(data_noPONV$sex)
qplot(data_noPONV$sex)
df_sex<- data_noPONV %>% group_by(sex) %>% summarise(total = n())
df_sex.1 <- df_sex %>% mutate(percent = total / sum(total))
df_sex.1

table(data_PONV$sex)
qplot(data_PONV$sex)
df_sex<- data_PONV %>% group_by(sex) %>% summarise(total = n())
df_sex.1 <- df_sex %>% mutate(percent = total / sum(total))
df_sex.1

sex_PONV_cross <- xtabs(~ sex + PONV, data = data)
sex_PONV_cross
chisq.test(sex_PONV_cross)

# 5. bmi
hist(data$bmi)
summary(data$bmi)
table(is.na(data$bmi))

table(is.na(data_noPONV$bmi))
hist(data_noPONV$bmi)
summary(data_noPONV$bmi)

table(is.na(data_PONV$bmi))
hist(data_PONV$bmi)
summary(data_PONV$bmi)

boxplot(bmi ~ PONV, data = data, col = c("brown2", "deepskyblue"))
var.test(bmi ~ PONV, data = data)
t.test(bmi ~ PONV, data = data, var.equal=TRUE)

# 6. ht
hist(data$ht)
summary(data$ht)
table(is.na(data$ht))

# 6. wt
hist(data$wt)
summary(data$wt)
table(is.na(data$wt))

# 7. smoking
class(data$smoking)

data$smoking <- factor(data$smoking, levels=c(0,1), labels=c("no","yes"))
table(data$smoking)
table(is.na(data$smoking))

qplot(data$smoking)
df_smoking <- data %>% group_by(smoking) %>% summarise(total = n())
df_smoking.1 <- df_smoking %>% mutate(percent = total / sum(total))
df_smoking.1

table(data_noPONV$smoking)
qplot(data_noPONV$smoking)
df_smoking <- data_noPONV %>% group_by(smoking) %>% summarise(total = n())
df_smoking.1 <- df_smoking %>% mutate(percent = total / sum(total))
df_smoking.1

table(data_PONV$smoking)
qplot(data_PONV$smoking)
df_smoking <- data_PONV %>% group_by(smoking) %>% summarise(total = n())
df_smoking.1 <- df_smoking %>% mutate(percent = total / sum(total))
df_smoking.1

smoking_PONV_cross <- xtabs(~ smoking + PONV, data = data)
smoking_PONV_cross
chisq.test(smoking_PONV_cross)

# 8. motion_sickness
class(data$motion_sickness)
data$motion_sickness <- factor(data$motion_sickness, levels=c(0,1), labels=c("no","yes"))
table(data$motion_sickness)
table(is.na(data$motion_sickness))

qplot(data$motion_sickness)
df_motion_sickness <- data %>% group_by(motion_sickness) %>% summarise(total = n())
df_motion_sickness.1 <- df_motion_sickness %>% mutate(percent = total / sum(total))
df_motion_sickness.1

table(data_noPONV$motion_sickness)
qplot(data_noPONV$motion_sickness, binwidth = 0.5)
df_motion_sickness <- data_noPONV %>% group_by(motion_sickness) %>% summarise(total = n())
df_motion_sickness.1 <- df_motion_sickness %>% mutate(percent = total / sum(total))
df_motion_sickness.1

table(data_PONV$motion_sickness)
qplot(data_PONV$motion_sickness,  binwidth = 0.5)
df_motion_sickness <- data_PONV %>% group_by(motion_sickness) %>% summarise(total = n())
df_motion_sickness.1 <- df_motion_sickness %>% mutate(percent = total / sum(total))
df_motion_sickness.1

motion_sickness_PONV_cross <- xtabs(~ motion_sickness + PONV, data = data)
motion_sickness_PONV_cross
chisq.test(motion_sickness_PONV_cross)

# 9. ponv
class(data$ponv)
data$ponv <- factor(data$ponv, levels=c(0,1), labels=c("no","yes"))
table(data$ponv)
table(is.na(data$ponv))

qplot(data$ponv)
df_ponv <- data %>% group_by(ponv) %>% summarise(total = n())
df_ponv.1 <- df_ponv %>% mutate(percent = total / sum(total))
df_ponv.1

table(data_noPONV$ponv)
qplot(data_noPONV$ponv, binwidth = 0.5)
df_ponv <- data_noPONV %>% group_by(ponv) %>% summarise(total = n())
df_ponvs.1 <- df_ponv %>% mutate(percent = total / sum(total))
df_ponvs.1

table(data_PONV$ponv)
qplot(data_PONV$ponv,  binwidth = 0.5)
df_ponv <- data_PONV %>% group_by(ponv) %>% summarise(total = n())
df_ponv.1 <- df_ponv %>% mutate(percent = total / sum(total))
df_ponv.1

ponv_PONV_cross <- xtabs(~ ponv + PONV, data = data)
ponv_PONV_cross
chisq.test(ponv_PONV_cross)

# 9. asa
class(data$asa)
data$asa <- ifelse(data$asa == 3, 1, 0)
data$asa <- as.factor(data$asa)
table(data$asa)
table(is.na(data$asa))

qplot(data$asa)
df_asa <- data %>% group_by(asa) %>% summarise(total = n())
df_asa.1 <- df_asa %>% mutate(percent = total / sum(total))
df_asa.1

table(data_noPONV$asa)
qplot(data_noPONV$asa)
df_asa <- data_noPONV %>% group_by(asa) %>% summarise(total = n())
df_asa.1 <- df_asa %>% mutate(percent = total / sum(total))
df_asa.1

table(data_PONV$asa)
qplot(data_PONV$asa)
df_asa <- data_PONV %>% group_by(asa) %>% summarise(total = n())
df_asa.1 <- df_asa %>% mutate(percent = total / sum(total))
df_asa.1

asa_PONV_cross <- xtabs(~ asa + PONV, data = data)
asa_PONV_cross
chisq.test(asa_PONV_cross)

# 10. dm
class(data$dm)
data$dm <- factor(data$dm, levels=c(0,1), labels=c("no","yes"))
table(data$dm)
table(is.na(data$dm))

qplot(data$dm)
df_dm <- data %>% group_by(dm) %>% summarise(total = n())
df_dm.1 <- df_dm %>% mutate(percent = total / sum(total))
df_dm.1

table(data_noPONV$dm)
qplot(data_noPONV$dm)
df_dm<- data_noPONV %>% group_by(dm) %>% summarise(total = n())
df_dm.1 <- df_dm %>% mutate(percent = total / sum(total))
df_dm.1

table(data_PONV$dm)
qplot(data_PONV$dm)
df_dm <- data_PONV %>% group_by(dm) %>% summarise(total = n())
df_dm.1 <- df_dm %>% mutate(percent = total / sum(total))
df_dm.1

dm_PONV_cross <- xtabs(~ dm + PONV, data = data)
dm_PONV_cross
chisq.test(dm_PONV_cross)

# 11. htn
class(data$htn)
data$htn <- factor(data$htn, levels=c(0,1), labels=c("no","yes"))
table(data$htn)
table(is.na(data$htn))

qplot(data$htn)
df_htn <- data %>% group_by(htn) %>% summarise(total = n())
df_htn.1 <- df_htn %>% mutate(percent = total / sum(total))
df_htn.1

table(data_noPONV$htn)
qplot(data_noPONV$htn)
df_htn <- data_noPONV %>% group_by(htn) %>% summarise(total = n())
df_htn.1 <- df_htn %>% mutate(percent = total / sum(total))
df_htn.1

table(data_PONV$htn)
qplot(data_PONV$htn)
df_htn <- data_PONV %>% group_by(htn) %>% summarise(total = n())
df_htn.1 <- df_htn %>% mutate(percent = total / sum(total))
df_htn.1

htn_PONV_cross <- xtabs(~ htn + PONV, data = data)
htn_PONV_cross
chisq.test(htn_PONV_cross)

# 10. type_an
class(data$type_an)
table(data$type_an)
table(is.na(data$type_an))

qplot(data$type_an)
df_type_an <- data %>% group_by(type_an) %>% summarise(total = n())
df_type_an.1 <- df_type_an %>% mutate(percent = total / sum(total))
df_type_an.1

table(data_noPONV$type_an)
qplot(data_noPONV$type_an)
df_type_an<- data_noPONV %>% group_by(type_an) %>% summarise(total = n())
df_type_an.1 <- df_type_an %>% mutate(percent = total / sum(total))
df_type_an.1

table(data_PONV$type_an)
qplot(data_PONV$type_an)
df_type_an<- data_PONV %>% group_by(type_an) %>% summarise(total = n())
df_type_an.1 <- df_type_an %>% mutate(percent = total / sum(total))
df_type_an.1

type_an_PONV_cross <- xtabs(~ type_an + PONV, data = data)
type_an_PONV_cross
chisq.test(type_an_PONV_cross)

# 11. duration_an
hist(data$duration_an)
summary(data$duration_an)
table(is.na(data$duration_an))

table(is.na(data_noPONV$duration_an))
hist(data_noPONV$duration_an)
summary(data_noPONV$duration_an)

table(is.na(data_PONV$duration_an))
hist(data_PONV$duration_an)
summary(data_PONV$duration_an)

boxplot(duration_an ~ PONV, data = data, col = c("brown2", "deepskyblue"))
var.test(duration_an ~ PONV, data = data)
t.test(duration_an ~ PONV, data = data, var.equal=TRUE)

# 12. premedi
class(data$premedi)
data$premedi <- factor(data$premedi, levels=c(0,1), labels=c("no","yes"))
table(data$premedi)
table(is.na(data$premedi))

qplot(data$premedi)
df_premedi <- data %>% group_by(premedi) %>% summarise(total = n())
df_premedi.1 <- df_premedi %>% mutate(percent = total / sum(total))
df_premedi.1

table(data_noPONV$premedi)
qplot(data_noPONV$premedi)
df_premedi <- data_noPONV %>% group_by(premedi) %>% summarise(total = n())
df_premedi.1 <- df_premedi %>% mutate(percent = total / sum(total))
df_premedi.1

table(data_PONV$premedi)
qplot(data_PONV$premedi)
df_premedi <- data_PONV %>% group_by(premedi) %>% summarise(total = n())
df_premedi.1 <- df_premedi %>% mutate(percent = total / sum(total))
df_premedi.1

premedi_PONV_cross <- xtabs(~ premedi + PONV, data = data)
premedi_PONV_cross
chisq.test(premedi_PONV_cross)

# 13. pre_op
class(data$pre_op)
data$pre_op <- factor(data$pre_op, levels=c(0,1), labels=c("no","yes"))
table(data$pre_op)
table(is.na(data$pre_op))

qplot(data$pre_op)
df_pre_op <- data %>% group_by(pre_op) %>% summarise(total = n())
df_pre_op.1 <- df_pre_op %>% mutate(percent = total / sum(total))
df_pre_op.1

table(data_noPONV$pre_op)
qplot(data_noPONV$pre_op)
df_pre_op <- data_noPONV %>% group_by(pre_op) %>% summarise(total = n())
df_pre_op.1 <- df_pre_op %>% mutate(percent = total / sum(total))
df_pre_op.1

table(data_PONV$pre_op)
qplot(data_PONV$pre_op)
df_pre_op <- data_PONV %>% group_by(pre_op) %>% summarise(total = n())
df_pre_op.1 <- df_pre_op %>% mutate(percent = total / sum(total))
df_pre_op.1

pre_op_PONV_cross <- xtabs(~ pre_op + PONV, data = data)
pre_op_PONV_cross
chisq.test(pre_op_PONV_cross)

# 14. agent_an
class(data$agent_an)
table(data$agent_an)
table(is.na(data$agent_an))

class(data$agent_an)
table(data$agent_an)
table(is.na(data$agent_an))

qplot(data$agent_an)
df_agent_an <- data %>% group_by(agent_an) %>% summarise(total = n())
df_agent_an<- df_agent_an %>% mutate(percent = total / sum(total))
df_agent_an

table(data_noPONV$agent_an)
qplot(data_noPONV$agent_an)
df_data_noPONV <- data_noPONV %>% group_by(agent_an) %>% summarise(total = n())
df_data_noPONV.1 <- df_data_noPONV %>% mutate(percent = total / sum(total))
df_data_noPONV.1

table(data_G_PONV$agent_an)
qplot(data_G_PONV$agent_an)
df_data_G_PONV <- data_G_PONV %>% group_by(agent_an) %>% summarise(total = n())
df_data_G_PONV.1 <- df_data_G_PONV %>% mutate(percent = total / sum(total))
df_data_G_PONV.1

agent_an_PONV_cross <- xtabs(~ agent_an + PONV, data = data)
agent_an_PONV_cross
chisq.test(agent_an_PONV_cross)

# 15. intra_remi
class(data$intra_remi)
data$intra_remi <- factor(data$intra_remi, levels=c(0,1), labels=c("no","yes"))
table(data$intra_remi)
table(is.na(data$intra_remi))

qplot(data$intra_remi)
df_intra_remi <- data %>% group_by(intra_remi) %>% summarise(total = n())
df_intra_remi.1 <- df_intra_remi %>% mutate(percent = total / sum(total))
df_intra_remi.1

table(data_noPONV$intra_remi)
qplot(data_noPONV$intra_remi)
df_intra_remi <- data_noPONV %>% group_by(intra_remi) %>% summarise(total = n())
df_intra_remi.1 <- df_intra_remi %>% mutate(percent = total / sum(total))
df_intra_remi.1

table(data_PONV$intra_remi)
qplot(data_PONV$intra_remi)
df_intra_remi<- data_PONV %>% group_by(intra_remi) %>% summarise(total = n())
df_intra_remi.1 <- df_intra_remi %>% mutate(percent = total / sum(total))
df_intra_remi.1

intra_remi_PONV_cross <- xtabs(~ intra_remi + PONV, data = data)
intra_remi_PONV_cross
chisq.test(intra_remi_PONV_cross)

# 15. intra_op
class(data$intra_op)
data$intra_op <- factor(data$intra_op, levels=c(0,1), labels=c("no","yes"))
table(data$intra_op)
table(is.na(data$intra_op))

qplot(data$intra_op)
df_intra_op <- data %>% group_by(intra_op) %>% summarise(total = n())
df_intra_op.1 <- df_intra_op %>% mutate(percent = total / sum(total))
df_intra_op.1

table(data_noPONV$intra_op)
qplot(data_noPONV$intra_op)
df_intra_op <- data_noPONV %>% group_by(intra_op) %>% summarise(total = n())
df_intra_op.1 <- df_intra_op %>% mutate(percent = total / sum(total))
df_intra_op.1

table(data_PONV$intra_op)
qplot(data_PONV$intra_op)
df_intra_op <- data_PONV %>% group_by(intra_op) %>% summarise(total = n())
df_intra_op.1 <- df_intra_op %>% mutate(percent = total / sum(total))
df_intra_op.1

intra_op_PONV_cross <- xtabs(~ intra_op + PONV, data = data)
intra_op_PONV_cross
chisq.test(intra_op_PONV_cross)

# 15. em
class(data$em)
data$em <- factor(data$em, levels=c(0,1), labels=c("no","yes"))
table(data$em)
table(is.na(data$em))

qplot(data$em)
df_em <- data %>% group_by(em) %>% summarise(total = n())
df_em.1 <- df_em %>% mutate(percent = total / sum(total))
df_em.1

table(data_noPONV$em)
qplot(data_noPONV$em)
df_em <- data_noPONV %>% group_by(em) %>% summarise(total = n())
df_em.1 <- df_em %>% mutate(percent = total / sum(total))
df_em.1

table(data_PONV$em)
qplot(data_PONV$em)
df_em <- data_PONV %>% group_by(em) %>% summarise(total = n())
df_em.1 <- df_em %>% mutate(percent = total / sum(total))
df_em.1

em_PONV_cross <- xtabs(~ em + PONV, data = data)
em_PONV_cross
chisq.test(em_PONV_cross)

# 16. type_op
class(data$type_op)
data$type_op <- factor(data$type_op, levels=c(0,1,2,3,4,5,6,7,8,9,10,11), labels=c("Abdominal","Thoracic","Obsteric","Gynecological","Urology","Brain","Spine","Shoulder","Hip","Upper and lower extremities","Skin, soft tissue","Others"))
table(data$type_op)
table(is.na(data$type_op))

qplot(data$type_op)
df_type_op <- data %>% group_by(type_op) %>% summarise(total = n())
df_type_op.1 <- df_type_op %>% mutate(percent = total / sum(total))
df_type_op.1

table(data_noPONV$type_op)
qplot(data_noPONV$type_op)
df_type_op <- data_noPONV %>% group_by(type_op) %>% summarise(total = n())
df_type_op.1 <- df_type_op %>% mutate(percent = total / sum(total))
df_type_op.1

table(data_PONV$type_op)
qplot(data_PONV$type_op)
df_type_op <- data_PONV %>% group_by(type_op) %>% summarise(total = n())
df_type_op.1 <- df_type_op %>% mutate(percent = total / sum(total))
df_type_op.1

type_op_PONV_cross <- xtabs(~ type_op + PONV, data = data)
type_op_PONV_cross
chisq.test(type_op_PONV_cross)

# 17. lapa
class(data$lapa)
data$lapa <- factor(data$lapa, levels=c(0,1), labels=c("no","yes"))
table(data$lapa)
table(is.na(data$lapa))

qplot(data$lapa)
df_lapa <- data %>% group_by(lapa) %>% summarise(total = n())
df_lapa.1 <- df_lapa %>% mutate(percent = total / sum(total))
df_lapa.1

table(data_noPONV$lapa)
qplot(data_noPONV$lapa)
df_lapa <- data_noPONV %>% group_by(lapa) %>% summarise(total = n())
df_lapa.1 <- df_lapa %>% mutate(percent = total / sum(total))
df_lapa.1

table(data_PONV$lapa)
qplot(data_PONV$lapa)
df_lapa <- data_PONV %>% group_by(lapa) %>% summarise(total = n())
df_lapa.1 <- df_lapa %>% mutate(percent = total / sum(total))
df_lapa.1

lapa_PONV_cross <- xtabs(~ lapa + PONV, data = data)
lapa_PONV_cross
chisq.test(lapa_PONV_cross)

# 18. main_fentanyl
hist(data$main_fentanyl)
summary(data$main_fentanyl)
table(is.na(data$main_fentanyl))

table(is.na(data_noPONV$main_fentanyl))
hist(data_noPONV$main_fentanyl)
summary(data_noPONV$main_fentanyl)

table(is.na(data_PONV$main_fentanyl))
hist(data_PONV$main_fentanyl)
summary(data_PONV$main_fentanyl)

boxplot(main_fentanyl ~ PONV, data = data, col = c("brown2", "deepskyblue"))
var.test(main_fentanyl ~ PONV, data = data)
t.test(main_fentanyl ~ PONV, data = data, var.equal=TRUE)

# 19. nefopam
class(data$nefopam)
data$nefopam <- factor(data$nefopam, levels=c(0,1), labels=c("no","yes"))
table(data$nefopam)
table(is.na(data$nefopam))

qplot(data$nefopam)
df_nefopam <- data %>% group_by(nefopam) %>% summarise(total = n())
df_nefopam.1 <- df_nefopam %>% mutate(percent = total / sum(total))
df_nefopam.1

table(data_noPONV$nefopam)
qplot(data_noPONV$nefopam)
df_nefopam <- data_noPONV %>% group_by(nefopam) %>% summarise(total = n())
df_nefopam.1 <- df_nefopam %>% mutate(percent = total / sum(total))
df_nefopam.1

table(data_PONV$nefopam)
qplot(data_PONV$nefopam)
df_nefopam<- data_PONV %>% group_by(nefopam) %>% summarise(total = n())
df_nefopam.1 <- df_nefopam %>% mutate(percent = total / sum(total))
df_nefopam.1

nefopam_PONV_cross <- xtabs(~ nefopam + PONV, data = data)
nefopam_PONV_cross
chisq.test(nefopam_PONV_cross)

# 20. antiemetic
class(data$antiemetic)
data$antiemetic <- factor(data$antiemetic, levels=c(0,1), labels=c("no","yes"))
table(data$antiemetic)
table(is.na(data$antiemetic))

qplot(data$antiemetic)
df_antiemetic <- data %>% group_by(antiemetic) %>% summarise(total = n())
df_antiemetic.1 <- df_antiemetic %>% mutate(percent = total / sum(total))
df_antiemetic.1

table(data_noPONV$antiemetic)
qplot(data_noPONV$antiemetic)
df_antiemetic <- data_noPONV %>% group_by(antiemetic) %>% summarise(total = n())
df_antiemetic.1 <- df_antiemetic %>% mutate(percent = total / sum(total))
df_antiemetic.1

table(data_PONV$antiemetic)
qplot(data_PONV$antiemetic)
df_antiemetic <- data_PONV %>% group_by(antiemetic) %>% summarise(total = n())
df_antiemetic.1 <- df_antiemetic %>% mutate(percent = total / sum(total))
df_antiemetic.1

antiemetic_PONV_cross <- xtabs(~ antiemetic + PONV, data = data)
antiemetic_PONV_cross
chisq.test(antiemetic_PONV_cross)

# 21. rescue_anti
class(data$rescue_anti)
data$rescue_anti <- factor(data$rescue_anti, levels=c(0,1), labels=c("no","yes"))
table(data$rescue_anti)
table(is.na(data$rescue_anti))

# 22. vnrs_max
hist(data$vnrs_max)
summary(data$vnrs_max)
table(is.na(data$vnrs_max))

# 23. rescue_analgesic
table(!is.na(data$rescue_analgesic))
data$rescue_analgesic <- ifelse(data$rescue_analgesic == 0, "no", "yes")
table(data$rescue_analgesic)


#### HA 13명, dizziness 137명, sedation 22명, pruritis 5명, hypotension 29명, discontinuation 173명, 

#---- 

raw_data <- read.csv("PCA_data_0812.csv", header=TRUE)
table(raw_data$asa)
raw_data$asa <- ifelse(raw_data$asa == 3, 1, 0)
raw_data$PONV <- factor(raw_data$PONV, labels=c("no", "yes"))
head(raw_data)
str(raw_data)
class(raw_data$PONV)

# 전체 2680명 data 수집됨

data <- filter(raw_data, age > 18)  # 19세 미만 23명 제거 후 2680명에서 2657명 남음

# agent_an, type_op 변수는 one-hot encoding
data$agent_an <- as.factor(data$agent_an)
data$type_op <- as.factor(data$type_op)

dummy <- dummyVars( ~ agent_an + type_op, data = data)
dummy_data <- data.frame(predict(dummy, newdata = data))

dummy_data <- select(dummy_data, -agent_an.0)
data_reduced <- select(data, -agent_an, -type_op)

data_temp <- cbind(data_reduced, dummy_data)

###########################################
## !!! Data scaling & 범주특성의 변환 ## ----
###########################################

# 1. PCA 데이터 범주(categorical data)/연속(continuous data)/label 분류

data_cat <- data_temp %>% 
  select(-age, -bmi, -duration_an, -main_fentanyl, -PONV)
data_num <- data_temp %>% 
  select(age, bmi, duration_an, main_fentanyl)
data_class <- data_temp %>% 
  select(PONV)

# 2. 연속형 특성의 Scaling

# 2-1. 표준화(평균 0, 표준편차 1) scaling

StandardScale <- preProcess(data_num, method=c("center", "scale"))
print(StandardScale)
data_standard <- predict(StandardScale, data_num)
head(data_standard)

# 2-2. min-max scaling

MinMaxScale <- preProcess(data_num, method=c("range"))
print(MinMaxScale)
data_minmax <- predict(MinMaxScale, data_num)
head(data_minmax)


# 3. 데이터 통합 및 저장

# cbind로 column 데이터를 추가해준다. cbind 외에도 여러가지 방법으로 같은 작업이 가능하다.
PCA_data = cbind(data_cat, data_minmax, data_class)

write.csv(PCA_data, file="PCA_data_p2.csv", row.names = TRUE)

#---- Demographic 통계 분석

sex_PONV <- data %>% 
  group_by(sex, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

sex_PONV

sex_PONV.1 <- data %>% 
  group_by(PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1))

sex_PONV.1

PONV <- sex_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(sex, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = sex, y = pct)) + geom_col()      

# type_op, create a basic ggplot2 pie chart
plotdata <- data %>%
  count(type_op) %>%
  arrange(desc(type_op)) %>%
  mutate(prop = round(n * 100 / sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5  *prop)

ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = type_op)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void()

# create a pie chart with slice labels
plotdata <- data %>%
  count(type_op) %>%
  arrange(desc(type_op)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

plotdata$label <- paste0(plotdata$type_op, "\n",
                         round(plotdata$prop), "%")

ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = type_op)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Type of surgery")


# age, plot the histogram with percentages on the y-axis
library(scales)
ggplot(data, 
       aes(x = age, 
           y= ..count.. / sum(..count..))) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 10) + 
  labs(title="Participants by age", 
       y = "Percent",
       x = "Age") +
  scale_y_continuous(labels = percent)

# Create a kernel density plot of age
ggplot(data, aes(x = age)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Participants by age")


# Bar chart with numeric labels 
plotdata <- data %>%
  count(sex) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

# plot the bars as percentages, 
# in decending order with bar labels
ggplot(plotdata, 
       aes(x = reorder(sex, -pct),
           y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Sex", 
       y = "Percent", 
       title  = "Participants by sex")

# 월별 PONV 변화, 4월 7일부터 PCA regimen 변화
mon_PONV <- data %>% 
  group_by(mon, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

mon_PONV

PONV <- mon_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(mon, pct, pctlabel)
PONV

# line plot with points
# and improved labeling
ggplot(PONV, 
       aes(x = mon, 
           y = pct, group = 1)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "steelblue") +
  ylim(0, 30) +
  labs(y = "PONV (%)", 
       x = "Time",
       title = "PONV changes over time") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(PONV, 
       aes(x = mon, 
           y = pct, group = 1)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "steelblue") +
  ylim(0, 30) +
  geom_smooth() +
  labs(y = "PONV (%)", 
       x = "Time",
       title = "PONV changes over time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# department, horizontal bar chart
data2 <- transform(data,
                   dept = factor(dept, levels = c("OS","OB","GS","CS",
                                                  "NS","URO","PS","OPH","ENT")))
attributes(data2$dept)

data2 %>% 
  count(dept) %>% 
  mutate(perc = n / nrow(data)) -> tips

ggplot(tips, aes(x = reorder(dept, perc), y = perc, fill = dept)) + 
  geom_bar(stat = "identity") +
  labs(x = "",
       y = "Frequency",
       title = "Surgery by department") +
  coord_flip()

# type of surgery

data %>% 
  count(dept) %>% 
  mutate(perc = n / nrow(data)) -> tips

ggplot(tips, aes(x = reorder(dept, perc), y = perc, fill = dept)) + 
  geom_bar(stat = "identity") +
  labs(x = "",
       y = "Frequency",
       title = "Type of surgery") +
  coord_flip()

# plot the distribution of age by PONV using boxplots
ggplot(data2, aes(x = PONV, y = age, fill = PONV)) +
  geom_boxplot(notch = TRUE, 
               alpha = .7) +
  labs(title = "Age distribution by PONV")

# plot sex by PONV
# grouped bar plot preserving zero count bars
ggplot(data2, 
       aes(x = PONV, 
           fill = sex)) + 
  geom_bar(position = position_dodge(preserve = "single"))

# bmi 
library(scales)
ggplot(data, 
       aes(x = bmi, 
           y= ..count.. / sum(..count..))) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 5) + 
  labs(title="Participants by BMI", 
       y = "Percent",
       x = "BMI") +
  scale_y_continuous(labels = percent)

# Create a kernel density plot of bmi
ggplot(data, aes(x = bmi)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Participants by BMI")

# PONV, pie chart
# create a pie chart with slice labels
plotdata <- data %>%
  count(PONV) %>%
  arrange(desc(PONV)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

plotdata$label <- paste0(plotdata$PONV, "\n",
                         round(plotdata$prop), "%")

ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = PONV)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Participants by PONV")


# ASA class, pie chart
# create a pie chart with slice labels
plotdata <- data %>%
  count(asa) %>%
  arrange(desc(asa)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

plotdata$label <- paste0(plotdata$asa, "\n",
                         round(plotdata$prop), "%")

ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = asa)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(y = lab.ypos, label = label), 
            color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Participants by ASA class")


# plot the distribution of bmi by PONV using boxplots
ggplot(data, aes(x = PONV, y = bmi, fill = PONV)) +
  geom_boxplot(notch = TRUE, 
               alpha = .7) +
  labs(title = "BMI distribution by PONV")

# plot smoking by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = smoking)) + 
  geom_bar(position = position_dodge(preserve = "single"))

smoking_PONV <- data %>% 
  group_by(smoking, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

smoking_PONV

PONV <- smoking_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(smoking, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = smoking, y = pct, fill = smoking)) +
  geom_col() +  coord_cartesian(ylim = c(0, 30))

# plot type of anesthesia by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = type_an)) + 
  geom_bar(position = position_dodge(preserve = "single"))

smoking_PONV <- data %>% 
  group_by(type_an, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

smoking_PONV

PONV <- smoking_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(type_an, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = type_an, y = pct, fill = type_an)) +
  geom_col(show.legend = FALSE) +  coord_cartesian(ylim = c(0, 20))

# plot preoperative opioid by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = pre_op)) + 
  geom_bar(position = position_dodge(preserve = "single"))

pre_op_PONV <- data %>% 
  group_by(pre_op, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

pre_op_PONV

PONV <- pre_op_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(pre_op, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = pre_op, y = pct, fill = pre_op)) +
  geom_col(show.legend = FALSE) +  coord_cartesian(ylim = c(0, 30))

# plot anesthetic agent by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = agent_an)) + 
  geom_bar(position = position_dodge(preserve = "single"))

agent_an_PONV <- data %>% 
  group_by(agent_an, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

agent_an_PONV

PONV <- agent_an_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(agent_an, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = agent_an, y = pct, fill = agent_an)) +
  geom_col(show.legend = FALSE) +  coord_cartesian(ylim = c(0, 30))

# plot intraopertive remifentanil by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = intra_remi)) + 
  geom_bar(position = position_dodge(preserve = "single"))

intra_remi_PONV <- data %>% 
  group_by(intra_remi, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

intra_remi_PONV

PONV <- intra_remi_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(intra_remi, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = intra_remi, y = pct, fill = intra_remi)) +
  geom_col(show.legend = FALSE) +  coord_cartesian(ylim = c(0, 30))

# plot intraopertive opioid by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = intra_op)) + 
  geom_bar(position = position_dodge(preserve = "single"))

intra_op_PONV <- data %>% 
  group_by(intra_op, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

intra_op_PONV

PONV <- intra_op_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(intra_op, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = intra_op, y = pct, fill = intra_op)) +
  geom_col(show.legend = FALSE) +  coord_cartesian(ylim = c(0, 30))

# plot emergency by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = em)) + 
  geom_bar(position = position_dodge(preserve = "single"))

em_PONV <- data %>% 
  group_by(em, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

em_PONV

PONV <- em_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(em, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = em, y = pct, fill = em)) +
  geom_col(show.legend = FALSE) +  coord_cartesian(ylim = c(0, 30))

# plot premedication by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = premedi)) + 
  geom_bar(position = position_dodge(preserve = "single"))

premedi_PONV <- data %>% 
  group_by(premedi, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

premedi_PONV

PONV <- premedi_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(premedi, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = premedi, y = pct, fill = premedi)) +
  geom_col(show.legend = FALSE) +  coord_cartesian(ylim = c(0, 20))

# plot type of surgery by PONV
table(data$type_op)
ggplot(data, 
       aes(x = PONV, 
           fill = type_op)) + 
  geom_bar(position = position_dodge(preserve = "single"))

type_op_PONV <- data %>% 
  group_by(type_op, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

type_op_PONV

PONV <- type_op_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(type_op, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = reorder(type_op, pct), y = pct, fill = type_op)) +
  geom_bar(stat = "identity") + 
  labs(x = "",
       y = "PONV, %",
       title = "Surgery by department") +
  ylim(0, 30) +
  coord_flip()

# plot laparoscopy by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = lapa)) + 
  geom_bar(position = position_dodge(preserve = "single"))

lapa_PONV <- data %>% 
  group_by(lapa, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

lapa_PONV

PONV <- lapa_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(lapa, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = lapa, y = pct, fill = lapa)) +
  geom_col(show.legend = FALSE) +  coord_cartesian(ylim = c(0, 30))

# plot acupan by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = nefopam)) + 
  geom_bar(position = position_dodge(preserve = "single"))

nefopam_PONV <- data %>% 
  group_by(nefopam, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

nefopam_PONV

PONV <- nefopam_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(nefopam, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = nefopam, y = pct, fill = nefopam)) +
  geom_col(show.legend = FALSE) +  coord_cartesian(ylim = c(0, 20))

# plot naseron by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = antiemetic)) + 
  geom_bar(position = position_dodge(preserve = "single"))

antiemetic_PONV <- data %>% 
  group_by(antiemetic, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

antiemetic_PONV

PONV <- antiemetic_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(antiemetic, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = antiemetic, y = pct, fill = antiemetic)) +
  geom_col(show.legend = FALSE) +  coord_cartesian(ylim = c(0, 20))


# main_fentanyl 
ggplot(data, 
       aes(x = main_fentanyl, 
           y= ..count.. / sum(..count..))) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white", 
                 binwidth = 0.05) + 
  labs(title="Participants by main_PCA", 
       y = "Percent",
       x = "main_PCA") +
  scale_y_continuous(labels = percent)

# Create a kernel density plot of main_fentanyl
ggplot(data, aes(x = main_fentanyl)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Participants by main_PCA")

# plot the distribution of main_PCA by PONV using boxplots
ggplot(data, aes(x = PONV, y = main_fentanyl, fill = PONV)) +
  geom_boxplot(notch = TRUE, 
               alpha = .7) +
  labs(title = "maintenance_PCA distribution by PONV")


# 월별 # PCA maintenance dose changes over time
PCA_dose <- data %>%
  group_by(mon) %>%
  dplyr::summarise(Mean   = mean(main_fentanyl),
                   Median = median(main_fentanyl))

ggplot(PCA_dose, 
       aes(x = mon, 
           y = Mean, group = 1)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "steelblue") +
  ylim(0.3, 0.4) +
  labs(y = "PCA maintenance dose", 
       x = "Time",
       title = "PCA maintenance dose changes over time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot asa by PONV
ggplot(data, 
       aes(x = PONV, 
           fill = asa)) + 
  geom_bar(position = position_dodge(preserve = "single"))

asa_PONV <- data %>% 
  group_by(asa, PONV) %>% 
  summarise(n = n()) %>% 
  mutate(tot_PONV = sum(n)) %>% 
  mutate(pct = round(n/tot_PONV*100, 1), 
         pctlabel = paste0(round(pct, 2), "%"))

asa_PONV

PONV <- asa_PONV %>% 
  filter(PONV == "PONV") %>% 
  select(asa, pct, pctlabel)
PONV

ggplot(data = PONV, aes(x = asa, y = pct, fill = asa)) +
  geom_col(show.legend = FALSE) +  coord_cartesian(ylim = c(0, 20))

# 

raw_data <- read.csv("PCA_data1.csv", header=TRUE)

PCA_cor <- cor(raw_data)
round(PCA_cor, 2)

library(corrplot)
corrplot(PCA_cor)
col <- colorRampPalette(c("#EC592E", "#E8E800", "#289BF0", "#BA1EFF", "#1EA316"))
corrplot(PCA_cor,
         method = "color",
         type = "lower",
         order = "original",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.col = "black",
         tl.srt = 45,
         diag = F)

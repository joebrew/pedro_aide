##########################################################################################################
#################################Tese de Mestrado da Custodia Paulo#######################################
#Criado por: Mussagy Mahomed, Instituto Nacional de Saude
#Tema:       Avalia??o de factores de risco associados a ocorr?ncia de epis?dios m?ltiplos de mal?ria cl?nica 
#            em crian?as dos 6 meses a  9  anos residentes na ?rea de vigil?ncia de morbilidade do Distrito 
#            de Manhi?a,  Agosto a dezembro de 2011
#Dados:      CISM Junho-Dezembro de 2011
#Datasets:   merged_data_sec.csv;master_file_cleaned_final_2.csv e master_file_cleaned_final_3.csv
#Supervisor: Prof. Pedro Aide
#Inicio:     Outubro de 2017
#Revis?o: 
#email:      mmahomed@hotmail.com
#Fim:
##########################################################################################################
######################################Importa??o de dados ################################################
library(readr)
library(epitools)
merged_data_sec <- read_csv("merged_data_sec.csv")
dt=merged_data_sec
str(dt)
library(readr)
merged_df <- read_csv("master_file_cleaned_final_3.csv")
dm=merged_df
#master_custodia_merged <- read_excel("C:/Users/INS2015/Downloads/Custodia Paulo/Base_de_dados_revista/master_custodia_merged.xlsx")
#df=master_custodia_merged
str(dm)
###################################codifica??o de variaveis###############################################
tab1 <- function(x = NA, miss = T, desc = F) {
  data.frame(t <- table(x, useNA = ifelse(miss, "always", "no")), 
             perc = tx <- as.numeric(100*prop.table(t)),
             cumperc = cumsum(tx)
  ) -> dm
  if (desc) {
    dm <- dff[order(dm$perc, decreasing = T),]
    dm$cumperc <- cumsum(dm$perc)
    ##rownames(df07) <- NULL
  }
  dm
}
######
## Age: 0 - "< 1"; 1 - "1 - 4"; 2 - "5 - 9"; 3 - "10 - 15"
dm$agecat <- ifelse(dm$age_years < 1, 0, NA)
dm$agecat <- ifelse(dm$age_years >= 1 & dm$age_years< 5, 1, dm$agecat)
dm$agecat <- ifelse(dm$age_years >= 5 & dm$age_years < 10, 2, dm$agecat)
#dm$agecat <- ifelse(dm$age_years>= 10 & dm$age_years >= 15, 3, dm$agecat)
tab1(dm$agecat)

#### Quantas pessoas dormiram neste agregado ontem?
## people_sleeping24h: 0 - "0 - 4"; 1 - "5 - 9"; 2 - "10 - 14"; 3 - "15 - 19"; 4 - "20 - 24"; 5 - "25 - 30"
#dm$peop24hcat <- ifelse(dm$people_sleeping24h < 4, 0, NA)
#dm$peop24hcat <- ifelse(dm$people_sleeping24h >= 5 & dm$people_sleeping24h < 9, 1, dm$peop24hcat)
#dm$peop24hcat <- ifelse(dm$people_sleeping24h >= 10 & dm$people_sleeping24h < 14, 2, dm$peop24hcat)
#dm$peop24hcat <- ifelse(dm$people_sleeping24h >= 15 & dm$people_sleeping24h < 19, 3, dm$peop24hcat)
#dm$peop24hcat <- ifelse(dm$people_sleeping24h >= 20 & dm$people_sleeping24h < 24, 4, dm$peop24hcat)
#dm$peop24hcat <- ifelse(dm$people_sleeping24h >= 25 & dm$people_sleeping24h >= 30, 5, dm$peop24hcat)
#tab1(dm$peop24hcat)
tab1(dm$`nr pregn_slept24h`)

####Quantas crian?as menores de 5 anos dormiram neste agregado ontem?
## child_sleeping: 0 - "0"; 1 - "1"; 2 - "2"; 3 - "3"; 4 - "4"; 5 - "5"
#dm$chisleep <- ifelse(dm$child_sleeping < 0, 0, NA)
#dm$chisleep <- (dm=dm[which (dm$child_sleeping<6),])

#dm$chisleep <- ifelse(dm$child_sleeping < 1 & dm$child_sleeping > 1, 1, dm$chisleep)
#dm$chisleep <- ifelse(dm$child_sleeping < 2 & dm$child_sleeping > 2, 2, dm$chisleep)
#dm$chisleep <- ifelse(dm$child_sleeping < 3 & dm$child_sleeping > 3, 3, dm$chisleep)
#dm$chisleep <- ifelse(dm$child_sleeping < 4 & dm$child_sleeping > 4, 4, dm$chisleep)
#dm$chisleep <- ifelse(dm$child_sleeping < 5 & dm$child_sleeping > 5, 5, dm$chisleep)
#tab1(dm$chisleep)
tab1(dm$`under5 slept_24hrs`)


#####numero total de episodios de malaria durante 
###case_def: "1 = 0; 2:12 = 1", 0=unico, 1=multiplos
#dm$epidmal <- recode(dm$case_def, "1 = 0; 2:12 = 1")

#####numero total de episodios de malaria durante 
###case_def: "1 = 0; 2:12 = 1", 0=unico, 1=multiplos
#dm$epidmal <- recode(dm$case_def, "1 = 0; 2:12 = 1")
dm$epimal[dm$case_def=="mult_epi"] <- 1
dm$epimal[dm$case_def=="single_epi"] <- 0
table(dm$case_def)
table(dm$head_educ, dm$epimal)
names(dm)
vars <- c("nr people slept_24hrs", "under5 slept_24hrs", "nr pregn_slept24h", "house_irs_12mont",
         "reason_no_fum", "slept_net", "nr_bednet", "nr_llins", "h_net", "h_llins", "episodes_of_malaria", "case_def", "head_sex", 
         "head_educ", "head_ocup", "head_resid", "n_const", "t_const", "kitchen", "bath", "fuel", "divhouse", "phone", "radio", "television",
         "salaried", "chd_gender", "age_years", "administrative_post", "agecat", "epimal")
for (i in 1:length(vars)){
  cat(vars[i])
  print(table(dm[,vars[i]]))
}

# variable cont: "people_sleeping24h", "child_sleeping", "preg_sleep", "n_const", "n_badr" 
# variable cat : "head_educ", "head_ocup", "head_resid", "t_const", "bath", "fuel"
# variable cat bin: "irs_12mon", "slept_net", "kitchen", "h_fumig"

write.csv(dm,"df.csv")
##########################################################################################################
#####################################Tabela descritiva do agregado familiar###############################
#Caracteristicas sociodemograficas do agregado familiar
tab1(dm$head_sex)
tab1(dm$head_educ)
tab1(dm$head_ocup)
tab1(dm$head_resid)
tab1(dm$salaried)
#tab1(dm$village_name)
tab1(dm$administrative_post)
#tab1(dm$locality)

#condicoes da habitacao do agregado familiar
tab1(dm$n_const)
tab1(dm$n_const)
tab1(dm$kitchen)
tab1(dm$bath)
tab1(dm$fuel)
tab1(dm$divhouse)
tab1(dm$phone)
tab1(dm$radio)
tab1(dm$television)

#Condicoes de dormida no agregado familiar
tab1(dm$nr_bednet)
tab1(dm$`nr people slept_24hrs`)
tab1(dm$`under5 slept_24hrs`)
tab1(dm$`nr pregn_slept24h`)

#Mecanismos de prevencao adotadas no agregado familiar
tab1(dm$house_irs_12mont)
tab1(dm$slept_net)
tab1(dm$reason_no_fum)
tab1(dm$nr_bednet)
tab1(dm$nr_llins)
tab1(dm$h_net)
tab1(dm$h_llins)

#Episodios de malaria das criancas 
tab1(dm$chd_gender)
tab1(dm$age_years)
summary(dm$age_years)
sd(dm$age_years)
tab1(dm$agecat)
tab1(dm$episodes_of_malaria)
tab1(dm$case_def)
tab1(dm$epimal)

###############Tabela descritiva das variaveis sociodemograficas ao nivel sem missings####################
#Caracteristicas sociodemograficas do agregado familiar
tab1(dm$head_sex, miss = F)
tab1(dm$head_educ, miss = F)
tab1(dm$head_ocup, miss = F)
tab1(dm$head_resid, miss = F)
tab1(dm$salaried, miss = F)
#tab1(dm$village_name)
tab1(dm$administrative_post, miss = F)
#tab1(dm$locality)

#condicoes da habitacao do agregado familiar
tab1(dm$n_const, miss = F)
tab1(dm$n_const, miss = F)
tab1(dm$kitchen, miss = F)
tab1(dm$bath, miss = F)
tab1(dm$fuel, miss = F)
tab1(dm$divhouse, miss = F)
tab1(dm$phone, miss = F)
tab1(dm$radio, miss = F)
tab1(dm$television, miss = F)

#Condicoes de dormida no agregado familiar
tab1(dm$`nr people slept_24hrs`, miss = F)
tab1(dm$`under5 slept_24hrs`, miss = F)
tab1(dm$`nr pregn_slept24h`, miss = F)

#Mecanismos de prevencao adotadas no agregado familiar
tab1(dm$house_irs_12mont, miss = F)
tab1(dm$reason_no_fum, miss = F)
tab1(dm$slept_net, miss = F)
tab1(dm$nr_bednet, miss = F)
tab1(dm$nr_llins, miss = F)
tab1(dm$h_net, miss = F)
tab1(dm$h_llins, miss = F)

#Episodios de malaria das criancas 
tab1(dm$episodes_of_malaria, miss = F)
tab1(dm$case_def, miss = F)
tab1(dm$chd_gender, miss = F)
tab1(dm$agecat, miss = F)
tab1(dm$epimal, miss = F)

#############ignorar
#tab1(dm$agecat, miss = F)
#tab1(dm$head_sex, miss = F)
#tab1(dm$head_educ, miss = F)
#tab1(dm$head_ocup, miss = F)
#tab1(dm$head_resid, miss = F)
#tab1(dm$village_name, miss = F)
#tab1(dm$gender, miss = F)
#tab1(dm$kitchen, miss = F)
#tab1(dm$`nr people slept_24hrs`, miss = F)
#tab1(dm$`under5 slept_24hrs`, miss = F)
#tab1(dm$case_def, miss = F)
#tab1(dm$epimal, miss = F)

tab1(dm$chd_gender, miss = F)


#######Caracterizar  os factores socio-demogr?ficos associados a m?ltiplos epis?dios de mal?ria########### 
#######em crian?as dos 6 meses a 9 anos de idade                                               
###Sexo: sexo da crian?a (1=Masculino; 2= Feminino)
table(dm$chd_gender, dm$epimal)
addmargins(table(dm$chd_gender, dm$epimal))
addmargins(prop.table((tbsex<-with(dm, table(chd_gender,epimal))),margin=2))

chisq.test(table(dm$chd_gender, dm$epimal))

mo = glm( epimal ~ as.factor(chd_gender), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)

summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI

oddsratio.wald(dm$chd_gender, dm$epimal)

epitab(dm$chd_gender, dm$epimal, pvalue = "chi2")
round(epitab(dm$chd_gender, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$chd_gender, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

### Idade
tapply(dm$age_years, dm$epimal,mean)
tapply(dm$age_years, dm$epimal,sd)
tapply(dm$age_years, dm$epimal,median)
tapply(dm$age_years, dm$epimal,summary)
tapply(dm$age_years, dm$epimal,quantile)
###Categoria de idade da crian?a: 0 - "< 1"; 1 - "1 - 4"; 2 - "5 - 9"
table(dm$agecat, dm$epimal)
addmargins(table(dm$agecat, dm$epimal))
addmargins(prop.table((tbcat<-with(dm, table(agecat,epimal))),margin=2))

chisq.test(table(dm$agecat, dm$epimal))

mo = glm( epimal ~ as.factor(agecat), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$agecat, dm$epimal)

epitab(dm$agecat, dm$epimal, pvalue = "chi2")
round(epitab(dm$agecat, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$agecat, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

###Sexo: sexo do chfe do agregado familiar (1=Masculino; 2= Feminino)
table(dm$head_sex, dm$epimal)
prop.table((tbsex<-with(dm, table(head_sex,epimal))),margin=2)
addmargins(table(dm$head_sex, dm$epimal))
addmargins(prop.table((tbsex<-with(dm, table(head_sex,epimal))),margin=2))

chisq.test(table(dm$head_sex, dm$epimal))

mo = glm( epimal ~ as.factor(head_sex), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$head_sex, dm$epimal)

epitab(dm$head_sex, dm$epimal, pvalue = "chi2")
round(epitab(dm$head_sex, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$head_sex, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)


#head_educ=nivel de educacao do chefe do agregado
tbedu<-with(dm, table(head_educ,epimal))
tbedu
tbedu<-with(dm, table(head_educ,epimal),margin=2)
tbedu
prop.table((tbedu<-with(dm, table(head_educ,epimal))),margin=2)
addmargins(table(dm$head_educ, dm$epimal))
addmargins(prop.table((tbedu<-with(dm, table(head_educ,epimal))),margin=2))

chisq.test(table(dm$head_educ, dm$epimal))

mo = glm( epimal ~ as.factor(head_educ), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$head_educ, dm$epimal)

epitab(dm$head_educ, dm$epimal, pvalue = "chi2")
round(epitab(dm$head_educ, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$head_educ, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

#head_ocup=Ocupa??o do chefe do agregado
tbocup<-with(dm, table(head_ocup, epimal))
tbocup
prop.table((tbocup),margin=2)

addmargins(table(dm$head_ocup, dm$epimal))
addmargins(prop.table((tbocup<-with(dm, table(head_ocup,epimal))),margin=2))

chisq.test(table(dm$head_ocup, dm$epimal))

mo = glm( epimal ~ as.factor(head_ocup), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$head_ocup, dm$epimal)

epitab(dm$head_ocup, dm$epimal, pvalue = "chi2")
round(epitab(dm$head_ocup, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$head_ocup, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)
#Mas uma coisa, vc pode mudar o grupo de referencia e fazer o inverso usando rev="rows" depois do pvalue="...  
#rev significa reverse e rows as linhas. entao vc esta dizendo para fazer o reverso das rows.
#'arg' should be one of "neither", "rows", "columns", "both"


epitab(relevel(as.factor(dm$head_educ), ref =2), dm$epimal, pvalue = "chi2")
#round(epitab(relevel(as.factor(dm$head_educ), ref =6), dm$epimal, pvalue = "chi2")$tab,4)
#round(epitab(relevel(as.factor(dm$head_educ), ref =6), dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

#nr de pessoas assalariadas no agregado
tab1(dm$salaried, miss = F)
tbsal<-with(dm, table(salaried, epimal))
tbsal
prop.table((tbsal),margin=2)

addmargins(table(dm$salaried, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(salaried,epimal))),margin=2))

chisq.test(table(dm$salaried, dm$epimal))

mo = glm( epimal ~ as.factor(salaried), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$salaried, dm$epimal)

epitab(dm$salaried, dm$epimal, pvalue = "chi2")
round(epitab(dm$salaried, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$salaried, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$salaried), ref =2), dm$epimal, pvalue = "chi2")
#round(epitab(relevel(as.factor(dm$head_educ), ref =6), dm$epimal, pvalue = "chi2")$tab,4)
#round(epitab(relevel(as.factor(dm$head_educ), ref =6), dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

#head_resid=nivel de educacao do chefe do agregado
tbresid<-with(dm, table(head_resid, epimal))
tbresid
prop.table((tbresid),margin=2)
tab1(dm$head_resid, miss = F)
tbsal<-with(dm, table(head_resid, epimal))
tbsal
prop.table((tbsal),margin=2)

addmargins(table(dm$head_resid, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(head_resid,epimal))),margin=2))

chisq.test(table(dm$head_resid, dm$epimal))

mo = glm( epimal ~ as.factor(head_resid), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$head_resid, dm$epimal)

epitab(dm$head_resid, dm$epimal, pvalue = "chi2")
round(epitab(dm$head_resid, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$head_resid, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$head_resid), ref =2), dm$epimal, pvalue = "chi2")

##Localizacao nos Postos administrativos dos agregados familiares
tab1(dm$administrative_post)
tbresid<-with(dm, table(administrative_post, epimal))
tbresid
prop.table((tbresid),margin=2)
tab1(dm$administrative_post, miss = F)
tbsal<-with(dm, table(administrative_post, epimal))
tbsal
prop.table((tbsal),margin=2)

addmargins(table(dm$administrative_post, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(administrative_post,epimal))),margin=2))

chisq.test(table(dm$administrative_post, dm$epimal))

mo = glm( epimal ~ as.factor(administrative_post), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$administrative_post, dm$epimal)

epitab(dm$administrative_post, dm$epimal, pvalue = "chi2")
round(epitab(dm$administrative_post, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$administrative_post, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$administrative_post), ref =2), dm$epimal, pvalue = "chi2")


#condicoes da habitacao do agregado familiar
#n_const=total de construcoes no agregado
tab1(dm$n_const)
tb_n_const<-with(dm, table(n_const, epimal))
tb_n_const
prop.table((tb_n_const),margin=2)
tab1(dm$n_const, miss = F)
tb_n_const<-with(dm, table(n_const, epimal))
tb_n_const
prop.table((tb_n_const),margin=2)

addmargins(table(dm$n_const, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(n_const,epimal))),margin=2))

chisq.test(table(dm$n_const, dm$epimal))

mo = glm( epimal ~ as.factor(n_const), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$n_const, dm$epimal)

epitab(dm$n_const, dm$epimal, pvalue = "chi2")
round(epitab(dm$n_const, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$n_const, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$n_const), ref =1), dm$epimal, pvalue = "chi2")

#t_const=tipo de construcao (1=todas material precario; 2=uma convencional outras precario; 
#3=todas de material convencional; 4= outro)
tb_t_const<-with(dm, table(t_const, epimal))
tb_t_const
prop.table((tb_t_const),margin=2)
tab1(dm$t_const, miss = F)
tb_t_const<-with(dm, table(t_const, epimal))
tb_t_const
prop.table((tb_t_const),margin=2)

addmargins(table(dm$t_const, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(t_const,epimal))),margin=2))

chisq.test(table(dm$t_const, dm$epimal))

mo = glm( epimal ~ as.factor(t_const), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$t_const, dm$epimal)

epitab(dm$t_const, dm$epimal, pvalue = "chi2")
round(epitab(dm$t_const, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$t_const, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$t_const), ref =2), dm$epimal, pvalue = "chi2")

#kitchen=tem cozinha no agregado (1=Sim, 2=N?o)
tbkt<-with(dm, table(kitchen, epimal))
tbkt
prop.table((tbkt),margin=2)

tab1(dm$kitchen, miss = F)
tbsal<-with(dm, table(kitchen, epimal))
tbsal
prop.table((tbsal),margin=2)

addmargins(table(dm$kitchen, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(kitchen,epimal))),margin=2))

chisq.test(table(dm$kitchen, dm$epimal))

mo = glm( epimal ~ as.factor(kitchen), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$kitchen, dm$epimal)

epitab(dm$kitchen, dm$epimal, pvalue = "chi2")
round(epitab(dm$kitchen, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$kitchen, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$kitchen), ref =2), dm$epimal, pvalue = "chi2")


#fuel=tipo de combustivel usado para cozinha
tbfuel<-with(dm, table(fuel, epimal))
tbfuel
prop.table((tbfuel),margin=2)
tab1(dm$fuel, miss = F)
tbfuel<-with(dm, table(fuel, epimal))
tbfuel
prop.table((tbfuel),margin=2)

addmargins(table(dm$fuel, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(fuel,epimal))),margin=2))

chisq.test(table(dm$fuel, dm$epimal))

mo = glm( epimal ~ as.factor(fuel), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$fuel, dm$epimal)

epitab(dm$fuel, dm$epimal, pvalue = "chi2")
round(epitab(dm$fuel, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$fuel, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$fuel), ref =2), dm$epimal, pvalue = "chi2")

##tipo de latrina (1=melhorada; 2=Tradicional; 3=autoclismo; 4=nao tem)
tab1(dm$bath)
tbfuel<-with(dm, table(bath, epimal))
tbbath
prop.table((tbbath),margin=2)
tab1(dm$bath, miss = F)
tbbath<-with(dm, table(bath, epimal))
tbbath
prop.table((tbbath),margin=2)

addmargins(table(dm$bath, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(bath,epimal))),margin=2))

chisq.test(table(dm$bath, dm$epimal))

mo = glm( epimal ~ as.factor(bath), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$bath, dm$epimal)

epitab(dm$bath, dm$epimal, pvalue = "chi2")
round(epitab(dm$bath, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$bath, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$bath), ref =2), dm$epimal, pvalue = "chi2")

##Divhouse
tab1(dm$divhouse)
tbhou<-with(dm, table(divhouse, case_def))
tbhou
prop.table((tbhou),margin=2)
tab1(dm$divhouse, miss = F)
tbhou<-with(dm, table(divhouse, epimal))
tbhou
prop.table((tbhou),margin=2)

addmargins(table(dm$divhouse, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(divhouse,epimal))),margin=2))

chisq.test(table(dm$divhouse, dm$epimal))

mo = glm( epimal ~ as.factor(divhouse), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$divhouse, dm$epimal)

#Phone
tab1(dm$phone)
tbph<-with(dm, table(phone, epimal))
tbph
prop.table((tbph),margin=2)
tab1(dm$phone, miss = F)
prop.table(tbph<-with(dm, table(phone, epimal)),margin=2)
tbph

addmargins(table(dm$phone, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(phone,epimal))),margin=2))

chisq.test(table(dm$phone, dm$epimal))

mo = glm( epimal ~ as.factor(phone), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$phone, dm$epimal)

#Radio

chisq.test(table(dm$radio, dm$epimal))

oddsratio.wald(dm$radio, dm$epimal)

mo = glm( epimal ~ as.factor(radio), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI

#Television

chisq.test(table(dm$television, dm$epimal))
oddsratio.wald(dm$television, dm$epimal)

mo = glm( epimal ~ as.factor(television), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$television, dm$epimal)



#####Condicoes de dormida no agregado familiar
#n_badr=nr_bednet:numero de quartos usados para dormir
tab1(dm$nr_bednet)
tab1(dm$nr_bednet, miss = F)

prop.table(tbbdr<-with(dm, table(nr_bednet, epimal)),margin=2)
tbbdr


addmargins(table(dm$nr_bednet, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(nr_bednet,epimal))),margin=2))

chisq.test(table(dm$nr_bednet, dm$epimal))

mo = glm( epimal ~ as.factor(nr_bednet), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$nr_bednet, dm$epimal)

epitab(dm$nr_bednet, dm$epimal, pvalue = "chi2")
round(epitab(dm$nr_bednet, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$nr_bednet, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$nr_bednet), ref =7), dm$epimal, pvalue = "chi2")

##Quantas pessoas dormiram neste agregado ontem?
tab1(dm$`nr people slept_24hrs`)
tbbdr<-with(dm, table(`nr people slept_24hrs`, case_def))
tbbdr
prop.table((tbbdr),margin=2)
tab1(dm$`nr people slept_24hrs`, miss = F)
tbsal<-with(dm, table(`nr people slept_24hrs`, epimal))
tbsal
prop.table((tbsal),margin=2)

addmargins(table(dm$`nr people slept_24hrs`, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(`nr people slept_24hrs`,epimal))),margin=2))

chisq.test(table(dm$`nr people slept_24hrs`, dm$epimal))

mo = glm( epimal ~ as.factor(`nr people slept_24hrs`), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$`nr people slept_24hrs`, dm$epimal)

epitab(dm$`nr people slept_24hrs`, dm$epimal, pvalue = "chi2")
round(epitab(dm$`nr people slept_24hrs`, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$`nr people slept_24hrs`, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)


###Quantas crian?as menores de 5 anos dormiram neste agregado ontem?
tab1(dm$`under5 slept_24hrs`)
prop.table(tbunder<-with(dm, table(`under5 slept_24hrs`, case_def)),margin=2)
tbunder

addmargins(table(dm$`under5 slept_24hrs`, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(`under5 slept_24hrs`,epimal))),margin=2))

chisq.test(table(dm$`under5 slept_24hrs`, dm$epimal))

mo = glm( epimal ~ as.factor(`under5 slept_24hrs`), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$`under5 slept_24hrs`, dm$epimal)

epitab(dm$`under5 slept_24hrs`, dm$epimal, pvalue = "chi2")
round(epitab(dm$`under5 slept_24hrs`, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$`under5 slept_24hrs`, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

####Quantas mulheres gr?vidas dormiram neste agregado ontem?
tab1(dm$`nr pregn_slept24h`)
prop.table(tbpreg<-with(dm, table(`nr pregn_slept24h`, case_def)),margin=2)
tbpreg

addmargins(table(dm$`nr pregn_slept24h`, dm$epimal))
addmargins(prop.table((tbpreg<-with(dm, table(`nr pregn_slept24h`,epimal))),margin=2))

chisq.test(table(dm$`nr pregn_slept24h`, dm$epimal))

mo = glm( epimal ~ as.factor(`nr pregn_slept24h`), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$`nr pregn_slept24h`, dm$epimal)

epitab(dm$`nr pregn_slept24h`, dm$epimal, pvalue = "chi2")
round(epitab(dm$`nr pregn_slept24h`, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$`nr pregn_slept24h`, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

########Mecanismos de prevencao adotadas no agregado familiar
####h_fumig=house_irs_12mont: A casa foi fumigada durante os ?ltimos 12 meses? (1=Sim, 2=N?o, 3=N?o sabe)
tab1(dm$house_irs_12mont)
prop.table(tbhou<-with(dm, table(house_irs_12mont,epimal)),margin=2)
tbhou

addmargins(table(dm$house_irs_12mont, dm$epimal))
addmargins(prop.table((tbhou<-with(dm, table(house_irs_12mont,epimal))),margin=2))

chisq.test(table(dm$house_irs_12mont, dm$epimal))

mo = glm( epimal ~ as.factor(house_irs_12mont), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$house_irs_12mont, dm$epimal)

epitab(dm$house_irs_12mont, dm$epimal, pvalue = "chi2")
round(epitab(dm$house_irs_12mont, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$house_irs_12mont, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$house_irs_12mont), ref =1), dm$epimal, pvalue = "chi2")

###Dormiu numa rede ontem? (0=n?o usou rede; 1=rede n?o impregnada, 2=rede impregnada, 3=n?o sabe)
tab1(dm$slept_net)
tab1(dm$slept_net, miss = F)
prop.table(tbbdr<-with(dm, table(slept_net, epimal)),margin=2)
tbbdr

addmargins(table(dm$slept_net, dm$epimal))
addmargins(prop.table((tbsal<-with(dm, table(slept_net,epimal))),margin=2))

chisq.test(table(dm$slept_net, dm$epimal))

mo = glm( epimal ~ as.factor(slept_net), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$slept_net, dm$epimal)

epitab(dm$slept_net, dm$epimal, pvalue = "chi2")
round(epitab(dm$slept_net, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$slept_net, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$slept_net), ref =2), dm$epimal, pvalue = "chi2")

###razao d nao fumigacao (1= Equipa n?o veio, 2=n?o estavam em casa, 3=n?o sabe, 4=outros, 99=recusaram)
tab1(dm$reason_no_fum)
prop.table(tbreas<-with(dm, table(reason_no_fum, epimal)),margin=2)
tbreas

addmargins(table(dm$reason_no_fum, dm$epimal))
addmargins(prop.table((tbreas<-with(dm, table(reason_no_fum,epimal))),margin=2))

chisq.test(table(dm$reason_no_fum, dm$epimal))

mo = glm( epimal ~ as.factor(reason_no_fum), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI
oddsratio.wald(dm$reason_no_fum, dm$epimal)

epitab(dm$reason_no_fum, dm$epimal, pvalue = "chi2")
round(epitab(dm$reason_no_fum, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$reason_no_fum, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$reason_no_fum), ref =2), dm$epimal, pvalue = "chi2")

###numero de redes mosquiteiras existentes no agregado
#Sair
tab1(dm$nr_bednet)

chisq.test(table(dm$nr_bednet, dm$epimal))
mo = glm( epimal ~ as.factor(reason_no_fum), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI

oddsratio.wald(dm$nr_bednet, dm$epimal)

epitab(dm$nr_bednet, dm$epimal, pvalue = "chi2")
round(epitab(dm$nr_bednet, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$nr_bednet, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$nr_bednet), ref =2), dm$epimal, pvalue = "chi2")


###Numero de redes mosquiteiras tratadas com insecticidas de longa duracao existentes no agregado
tab1(dm$nr_llins)
chisq.test(table(dm$nr_llins, dm$epimal))

mo = glm( epimal ~ as.factor(nr_llins), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI

oddsratio.wald(dm$nr_llins, dm$epimal)

epitab(dm$nr_llins, dm$epimal, pvalue = "chi2")
round(epitab(dm$nr_llins, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$nr_llins, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$nr_llins), ref =4), dm$epimal, pvalue = "chi2")
###Numero de redes penduradas
tab1(dm$h_net)
chisq.test(table(dm$h_net, dm$epimal))

mo = glm( epimal ~ as.factor(h_net), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI

oddsratio.wald(dm$h_net, dm$epimal)

epitab(dm$h_net, dm$epimal, pvalue = "chi2")
round(epitab(dm$h_net, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$h_net, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$h_net), ref =4), dm$epimal, pvalue = "chi2")
###Numero de redes impregnadas penduradas
tab1(dm$h_llins)
chisq.test(table(dm$h_llins, dm$epimal))

mo = glm( epimal ~ as.factor(h_llins), data=dm, family=binomial)
data.frame(OR  = exp(coefficients(mo)))
#logistic.display(mo)
summary(mo)
p.value <- round(summary(mo)$coef[, "Pr(>|z|)"],3)
cbind(round(confint(mo, level=0.95, type="LR"),2),p.value)
MULTI <- cbind(round(exp(cbind(OR=coef(mo),confint(mo))),2),p.value)
MULTI

oddsratio.wald(dm$h_llins, dm$epimal)

epitab(dm$nr_llins, dm$epimal, pvalue = "chi2")
round(epitab(dm$nr_llins, dm$epimal, pvalue = "chi2")$tab,4)
round(epitab(dm$nr_llinst, dm$epimal, oddsratio = "fisher", pvalue="fisher.exact")$tab, 4)

epitab(relevel(as.factor(dm$h_llins), ref =4), dm$epimal, pvalue = "chi2")



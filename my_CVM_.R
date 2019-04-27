#예비조사
pre_cvm<-presurvey_1
View(pre_cvm)
summary(pre_cvm,na.rm=TRUE)

#본조사
real_cvm<-realsurvey_1
View(real_cvm)
summary(real_cvm,na.rm=TRUE)


#frequency analysis
table2_c_1<-table(real_cvm$C_1)
table2_c_1

table2_D_2<-table(real_cvm$D_2)
table2_D_2

table2_D_3<-table(real_cvm$D_3)
table2_D_3

table2_H_1<-table(real_cvm$H_1)
table2_H_1

table2_P_1<-table(real_cvm$P_1)
table2_P_1

table2_P_3<-table(real_cvm$P_3)
table2_P_3

table2_P_4<-table(real_cvm$P_4)
table2_P_4

table2_P_5<-table(real_cvm$P_5)
table2_P_5


table2_P_6<-table(real_cvm$P_6)
table2_P_6


table2_P_7<-table(real_cvm$P_7)
table2_P_7

#income자료추가,housewife, etc==> based on sex, age average income
income_1<-real_cvm$P_5
income_1
income_2<-replace(income_1,income_1=="2",3392916)
income_2

income_3<-replace(income_2,income_2=="3",2461000)
income_3

income_4<-replace(income_3,income_3=="4",3733000)
income_4

income_5<-replace(income_4,income_4=="5",6216166)
income_5

income_6<-replace(income_5,income_5=="6",3026041)
income_6

income_7<-replace(income_6,income_6=="7",4246416)
income_7

income_8<-replace(income_7,income_7=="10",4465250)
income_8


#income_8리스트를 real_cvm에 추가하기
n_real_cvm_1<-cbind(real_cvm,income_8)
n_real_cvm_1
write.csv(n_real_cvm_1,file = "D:/CVM_data/n_real_cvm_1.csv")

#Probit model, D_3 NA--> special value transform
is.na(n_real_cvm_1$D_3)
n_real_cvm_2<-n_real_cvm_1
is.na(n_real_cvm_2$D_3)
n_real_cvm_2$D_3[is.na(n_real_cvm_2$D_3)]<-0
n_real_cvm_2$D_3
n_real_cvm_2
write.csv(n_real_cvm_2,file = "D:/CVM_data/n_real_cvm_2.csv",row.names = FALSE)

#Probit model,http://r-statistics.co/Probit-Regression-With-R.html
attach(n_real_cvm_2)
my_AV_result<-aov(D_3~D_4)
summary(my_AV_result)

#더미변수는 factor()함수use
my_regression_1<-lm(D_3~D_1+D_4+factor(P_1)+P_2+factor(P_3)+factor(P_4)+factor(P_6)+factor(P_7)+income_8)
summary(my_regression_1)
coef(my_regression_1) 
#coefficient
##just coefficient--> female, not living attached housing(연립) compared detached housing(단독), 
##travel mode-->bike, taxi


#Probit model https://www.youtube.com/watch?v=A9P888Lxde8
D2_probit_3<-glm(D_3~D_1+D_4+factor(P_1)+P_2+factor(P_3)+factor(P_4)+factor(P_6)+factor(P_7)+income_8,family = binomial(link = "probit"),data = n_real_cvm_2)
summary(D2_probit_3)

#Probit model based on considering correlation
D2_probit_4<-glm(D_3~D_1+D_4+factor(P_1)+P_2+factor(P_3)+factor(P_6)+income_8,family = binomial(link = "probit"),data = n_real_cvm_2)
summary(D2_probit_4)

D2_probit_5<-glm(D_3~factor(P_1)+factor(P_3)+factor(P_6)+income_8,family = binomial(link = "probit"),data = n_real_cvm_2)
summary(D2_probit_5)

D2_probit_6<-glm(D_3~factor(P_1)+factor(P_6)+income_8,family = binomial(link = "probit"),data = n_real_cvm_2)
summary(D2_probit_6)

D2_probit_7<-glm(D_3~factor(P_6)+income_8,family = binomial(link = "probit"),data = n_real_cvm_2)
summary(D2_probit_7)

D2_probit_8<-glm(D_3~factor(P_6),family = binomial(link = "probit"),data = n_real_cvm_2)
summary(D2_probit_8)

D2_probit_9<-glm(D_3~income_8,family = binomial(link = "probit"),data = n_real_cvm_2)
summary(D2_probit_9)

D2_probit_10<-glm(D_3~C_1+income_8,family = binomial(link = "probit"),data = n_real_cvm_2)
summary(D2_probit_10)

D2_probit_11<-glm(D_3~C_1+factor(P_6)+income_8,family = binomial(link = "probit"),data = n_real_cvm_2)
summary(D2_probit_11)

D2_probit_12<-glm(D_3~C_1+factor(P_6),family = binomial(link = "probit"),data = n_real_cvm_2)
summary(D2_probit_12)

D2_probit_13<-glm(D_3~D_1,family = binomial(link = "probit"),data = n_real_cvm_2)
summary(D2_probit_13)

D2_probit_14<-glm(D_3~D_1,family = binomial(link = "logit"),data = n_real_cvm_2)
summary(D2_probit_14)


#C_1 추가, All variables-->NA control--> mean, 
#C_1 deal with value "1"-->5000, "2"-->3000, "3"-->2000, "4"-->1000, "5"-->500, "6"-->843.3("1"~"5"mean)
price_e_1<-n_real_cvm_2$C_1
price_e_2<-replace(price_e_1,price_e_1=="1",5000)
price_e_2

price_e_3<-replace(price_e_2,price_e_2=="2",3000)
price_e_3

price_e_4<-replace(price_e_3,price_e_3=="3",2000)
price_e_4

price_e_5<-replace(price_e_4,price_e_4=="4",1000)
price_e_5

price_e_6<-replace(price_e_5,price_e_5=="5",500)
price_e_6

mean(price_e_6)
summary(price_e_6)

price_e_7<-replace(price_e_6,price_e_6=="6",843.3)
price_e_7

#price_e_7리스트를 n_real_cvm_2에 추가하기
n_real_cvm_3<-cbind(n_real_cvm_2,price_e_7)
n_real_cvm_3

#n_real_cvm_3_correlation
my_regression_20<-lm(D_3~C_1)
summary(my_regression_20)
coef(my_regression_20)

my_regression_21<-lm(D_3~C_1+D_1+D_4+factor(P_1)+P_2+factor(P_3)+factor(P_4)+factor(P_6)+factor(P_7)+income_8)
summary(my_regression_21)
coef(my_regression_21)

#n_real_cvm_3_probit
D2_probit_21<-glm(D_3~C_1+D_1+D_4+factor(P_1)+P_2+factor(P_3)+factor(P_4)+factor(P_6)+factor(P_7)+income_8,family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_21)

D2_probit_22<-glm(D_3~C_1+D_1,family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_22)

D2_probit_23<-glm(D_3~C_1+D_1+D_4+factor(P_1),family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_23)

D2_probit_24<-glm(D_3~C_1+D_1+D_4+factor(P_1)+P_2,family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_24)

D2_probit_25<-glm(D_3~P_2+factor(P_6),family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_25)

D2_probit_26<-glm(D_3~P_2,family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_26)

write.csv(n_real_cvm_3,file = "D:/CVM_data/n_real_cvm_3.csv",row.names = FALSE)


#n_real_cvm_4 analysis D_4-->alternative value include
D2_probit_31<-glm(D_3~C_1+D_1+D_4+factor(P_1)+P_2+factor(P_3)+factor(P_4)+factor(P_6)+factor(P_7)+income_8,family = binomial(link = "probit"), data = n_real_cvm_4)
summary(D2_probit_31)

D2_probit_32<-glm(D_3~C_1+D_1,family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_32)

D2_probit_33<-glm(D_3~C_1+D_1+D_4+factor(P_1),family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_33)

D2_probit_34<-glm(D_3~C_1+D_1+D_4+factor(P_1)+P_2,family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_34)

D2_probit_35<-glm(D_3~P_2+factor(P_6),family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_35)

D2_probit_36<-glm(D_3~P_2,family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_36)



D2_probit_41<-glm(D_3~D_1+D_4+factor(P_1)+P_2+factor(P_3)+factor(P_4)+factor(P_6)+factor(P_7)+income_8,family = binomial(link = "probit"), data = n_real_cvm_4)
summary(D2_probit_41)

D2_probit_42<-glm(D_3~D_1,family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_42)

D2_probit_43<-glm(D_3~D_1+D_4+factor(P_1),family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_43)

D2_probit_44<-glm(D_3~D_1+D_4+factor(P_1)+P_2,family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_44)

D2_probit_45<-glm(D_3~P_2+factor(P_6),family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_45)

D2_probit_46<-glm(D_3~P_2,family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_46)

D2_probit_47<-glm(D_3~factor(P_6),family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_47)


D2_probit_48<-glm(D_3~factor(P_6)+factor(P_7),family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_48)

D2_probit_49<-glm(D_3~factor(P_7),family = binomial(link = "probit"), data = n_real_cvm_3)
summary(D2_probit_49)

plot(D_3~income_8)
plot(D_3~factor(P_6))

plot(D_3~factor(P_1))
plot(D_3~C_1)

#Ver02_anal##################
ver02_cvm_1<-ver02_total
View(ver02_cvm_1)

#C_1 value change, 6etc==mean
n_C_1<-ver02_cvm_1$C_1
n_C_1

n_C_2<-replace(n_C_1,n_C_1=="1",5000)
n_C_2

n_C_3<-replace(n_C_2,n_C_2=="2",3000)
n_C_3

n_C_4<-replace(n_C_3,n_C_3=="3",2000)
n_C_4

n_C_5<-replace(n_C_4,n_C_4=="4",1000)
n_C_5

n_C_6<-replace(n_C_5,n_C_5=="5",500)
n_C_6

n_C1<-replace(n_C_6,n_C_6=="6",1452)
n_C1

#ver02_cvm colume add, C_1 and S column delete
ver02_cvm_2<-cbind(ver02_cvm_1,n_C1)
ver02_cvm_2

ver02_cvm_3<-subset(ver02_cvm_2,select = -c(C_1,S))
ver02_cvm_3
write.csv(ver02_cvm_3,file = "D:/CVM_data/ver02_cvm_3.csv",row.names = FALSE)

summary(ver02_cvm_3)

#frequency analysis
cvm_table_1<-table(ver02_cvm_3$n_C_7)
cvm_table_1

cvm_table_2<-table(ver02_cvm_3$D_2)
cvm_table_2

cvm_table_3<-table(ver02_cvm_3$D_3)
cvm_table_3

cvm_table_4<-table(ver02_cvm_3$P_1)
cvm_table_4

cvm_table_5<-table(ver02_cvm_3$P_3)
cvm_table_5

cvm_table_6<-table(ver02_cvm_3$P_4)
cvm_table_6

cvm_table_7<-table(ver02_cvm_3$P_5)
cvm_table_7

cvm_table_8<-table(ver02_cvm_3$P_6)
cvm_table_8

cvm_table_9<-table(ver02_cvm_3$P_7)
cvm_table_9

cvm_table_10<-table(ver02_cvm_3$H_1)
cvm_table_10

#ver02_cvm_3_correlation
attach(ver02_cvm_3)
my_reg_ver02<-lm(H_1~D_1)
summary(my_reg_ver02)
coef(my_reg_ver02)

#P_3_residence/others devided
n_v02_resid_1<-subset(ver02_cvm_3,P_3=="1",select = A_1:n_C_7)
n_v02_resid_1

n_v02_ot_1<-subset(ver02_cvm_3,P_3=="2",select = A_1:n_C_7)
n_v02_ot_1

n_v02_ot_2<-subset(ver02_cvm_3,P_3=="3",select = A_1:n_C_7)
n_v02_ot_2

n_v02_ot_3<-rbind(n_v02_ot_1,n_v02_ot_2)
n_v02_ot_3

attach(n_v02_resid_1)
my_reg_ver03<-lm(H_1~D_1)
summary(my_reg_ver03)
coef(my_reg_ver03)

attach(n_v02_resid_1)
my_reg_ver04<-lm(D_2~D_1)
summary(my_reg_ver04)
coef(my_reg_ver04)

attach(n_v02_ot_3)
my_reg_ot_3<-lm(H_1~D_1)
summary(my_reg_ot_3)
coef(my_reg_ot_3)

attach(n_v02_ot_3)
my_reg_ot_4<-lm(D_2~D_1)
summary(my_reg_ot_4)
coef(my_reg_ot_4)

boxplot(n_v02_resid_1$D_1)
plot(H_1,D_1)
plot(D_2,D_1)

#travel mode_group
ver02_cvm_3
tm_car_cvm_1<-subset(ver02_cvm_3,P_6=="1",select = A_1:n_C1)
tm_car_cvm_1
summary(tm_car_cvm_1)
write.csv(tm_car_cvm_1,file = "D:/CVM_data/tm_car_cvm_1.csv",row.names = FALSE)

#WTP~variables between car group and pubilc group
attach(tm_pub_cvm_1)
my_reg_pub_1<-lm(D_4~factor(P_7))
summary(my_reg_pub_1)
coef(my_reg_pub_1)

my_reg_pub_2<-lm(D_4~n_C1+factor(P_7))
summary(my_reg_pub_2)
coef(my_reg_pub_2)

my_reg_pub_3<-lm(D_4~n_C1+P_2+factor(P_7)+factor(P_3)+factor(P_4))
summary(my_reg_pub_3)
coef(my_reg_pub_3)

attach(tm_car_cvm_1)
my_reg_car_1<-lm(D_4~factor(P_7))
summary(my_reg_car_1)
coef(my_reg_car_1)

my_reg_car_2<-lm(D_4~n_C1+factor(P_7))
summary(my_reg_car_2)
coef(my_reg_car_2)

my_reg_car_3<-lm(D_4~n_C1+P_2+factor(P_7)+factor(P_3)+factor(P_4))
summary(my_reg_car_3)
coef(my_reg_car_3)


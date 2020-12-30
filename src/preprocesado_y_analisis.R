train<-read.csv(file = "../csv/train.csv")
test<-read.csv(file = "../csv/test.csv")

#### informacion de las variables
str(train)
summary(train)


#### valores nulos
colSums(is.na(train))
colSums(is.na(test))


#### valores extremos
for(i in colnames(train)){
  if(length(boxplot.stats(train[,i])$out) > 0)
  {
    boxplot(train[i], main = i, xlab = i)
    print(boxplot.stats(train[,i])$out)
  }
}

#### preprocesado
train$ram = 2^trunc(log(train$ram,2))
test$ram = 2^trunc(log(test$ram,2))

# Discretizamos las variables con pocas clases
cols = c("blue", "dual_sim", "four_g", "n_cores", "ram", "three_g", "touch_screen",
         "wifi", "price_range")
for (i in cols){
  train[,i] = as.factor(train[,i])
}

cols = c("blue", "dual_sim", "four_g", "n_cores", "ram", "three_g", "touch_screen",
         "wifi")
for (i in cols){
  test[,i] = as.factor(test[,i])
}

# Discretizamos las demas variables en 5 grupos
# Generamos nuevas variables en lugar de remplazar las originales para poder usar ambas

rows = dim(train)[1]
rowsTest = dim(test)[1]


max = max(train$battery_power)
min = min(train$battery_power)
inter = (max - min) / 5
for (i in 1:rows){
  train$battery_power_group[i] = trunc((train$battery_power[i] - min) / inter)
}
train$battery_power_group = as.factor(train$battery_power_group)
levels(train$battery_power_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$battery_power_group[i] = trunc((test$battery_power[i] - min) / inter)
}
test$battery_power_group = as.factor(test$battery_power_group)
levels(test$battery_power_group) = c("0", "1", "2", "3", "4", "4")


max = max(train$clock_speed)
min = min(train$clock_speed)
inter = (max - min) / 5
for (i in 1:rows){
  train$clock_speed_group[i] = trunc((train$clock_speed[i] - min) / inter)
}
train$clock_speed_group = as.factor(train$clock_speed_group)
levels(train$clock_speed_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$clock_speed_group[i] = trunc((test$clock_speed[i] - min) / inter)
}
test$clock_speed_group = as.factor(test$clock_speed_group)
levels(test$clock_speed_group) = c("0", "1", "2", "3", "4", "4")


max = max(train$fc)
min = min(train$fc)
inter = (max - min) / 5
for (i in 1:rows){
  train$fc_group[i] = trunc((train$fc[i] - min) / inter)
}
train$fc_group = as.factor(train$fc_group)
levels(train$fc_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$fc_group[i] = trunc((test$fc[i] - min) / inter)
}
test$fc_group = as.factor(test$fc_group)
levels(test$fc_group) = c("0", "1", "2", "3", "4", "4")


max = max(train$int_memory)
min = min(train$int_memory)
inter = (max - min) / 5
for (i in 1:rows){
  train$int_memory_group[i] = trunc((train$int_memory[i] - min) / inter)
}
train$int_memory_group = as.factor(train$int_memory_group)
levels(train$int_memory_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$int_memory_group[i] = trunc((test$int_memory[i] - min) / inter)
}
test$int_memory_group = as.factor(test$int_memory_group)
levels(test$int_memory_group) = c("0", "1", "2", "3", "4", "4")


max = max(train$m_dep)
min = min(train$m_dep)
inter = (max - min) / 5
for (i in 1:rows){
  train$m_dep_group[i] = trunc((train$m_dep[i] - min) / inter)
}
train$m_dep_group = as.factor(train$m_dep_group)
levels(train$m_dep_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$m_dep_group[i] = trunc((test$m_dep[i] - min) / inter)
}
test$m_dep_group = as.factor(test$m_dep_group)
levels(test$m_dep_group) = c("0", "1", "2", "3", "4", "4")


max = max(train$mobile_wt)
min = min(train$mobile_wt)
inter = (max - min) / 5
for (i in 1:rows){
  train$mobile_wt_group[i] = trunc((train$mobile_wt[i] - min) / inter)
}
train$mobile_wt_group = as.factor(train$mobile_wt_group)
levels(train$mobile_wt_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$mobile_wt_group[i] = trunc((test$mobile_wt[i] - min) / inter)
}
test$mobile_wt_group = as.factor(test$mobile_wt_group)
levels(test$mobile_wt_group) = c("0", "1", "2", "3", "4", "4")


max = max(train$pc)
min = min(train$pc)
inter = (max - min) / 5
for (i in 1:rows){
  train$pc_group[i] = trunc((train$pc[i] - min) / inter)
}
train$pc_group = as.factor(train$pc_group)
levels(train$pc_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$pc_group[i] = trunc((test$pc[i] - min) / inter)
}
test$pc_group = as.factor(test$pc_group)
levels(test$pc_group) = c("0", "1", "2", "3", "4", "4")


max = max(train$px_height)
min = min(train$px_height)
inter = (max - min) / 5
for (i in 1:rows){
  train$px_height_group[i] = trunc((train$px_height[i] - min) / inter)
}
train$px_height_group = as.factor(train$px_height_group)
levels(train$px_height_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$px_height_group[i] = trunc((test$px_height[i] - min) / inter)
}
test$px_height_group = as.factor(test$px_height_group)
levels(test$px_height_group) = c("0", "1", "2", "3", "4", "4")


max = max(train$px_width)
min = min(train$px_width)
inter = (max - min) / 5
for (i in 1:rows){
  train$px_width_group[i] = trunc((train$px_width[i] - min) / inter)
}
train$px_width_group = as.factor(train$px_width_group)
levels(train$px_width_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$px_width_group[i] = trunc((test$px_width[i] - min) / inter)
}
test$px_width_group = as.factor(test$px_width_group)
levels(test$px_width_group) = c("0", "1", "2", "3", "4", "4")


max = max(train$sc_h)
min = min(train$sc_h)
inter = (max - min) / 5
for (i in 1:rows){
  train$sc_h_group[i] = trunc((train$sc_h[i] - min) / inter)
}
train$sc_h_group = as.factor(train$sc_h_group)
levels(train$sc_h_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$sc_h_group[i] = trunc((test$sc_h[i] - min) / inter)
}
test$sc_h_group = as.factor(test$sc_h_group)
levels(test$sc_h_group) = c("0", "1", "2", "3", "4", "4")


max = max(train$sc_w)
min = min(train$sc_w)
inter = (max - min) / 5
for (i in 1:rows){
  train$sc_w_group[i] = trunc((train$sc_w[i] - min) / inter)
}
train$sc_w_group = as.factor(train$sc_w_group)
levels(train$sc_w_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$sc_w_group[i] = trunc((test$sc_w[i] - min) / inter)
}
test$sc_w_group = as.factor(test$sc_w_group)
levels(test$sc_w_group) = c("0", "1", "2", "3", "4", "4")


max = max(train$talk_time)
min = min(train$talk_time)
inter = (max - min) / 5
for (i in 1:rows){
  train$talk_time_group[i] = trunc((train$talk_time[i] - min) / inter)
}
train$talk_time_group = as.factor(train$talk_time_group)
levels(train$talk_time_group) = c("0", "1", "2", "3", "4", "4")
for (i in 1:rowsTest){
  test$talk_time_group[i] = trunc((test$talk_time[i] - min) / inter)
}
test$talk_time_group = as.factor(test$talk_time_group)
levels(test$talk_time_group) = c("0", "1", "2", "3", "4", "4")


str(train)

write.csv(train,"../csv/train_processed.csv", row.names = FALSE)
write.csv(test,"../csv/test_processed.csv", row.names = FALSE)


#### Dataframes a utilizar

vars1 = c("battery_power_group", "blue", "clock_speed_group", "dual_sim", "fc_group",
          "four_g", "int_memory_group", "m_dep_group", "mobile_wt_group", "n_cores",
          "pc_group", "px_height_group", "px_width_group", "ram", "sc_h_group", "sc_w_group",
          "talk_time_group", "three_g", "touch_screen", "wifi", "price_range")

vars2 = c("battery_power_group", "blue", "clock_speed_group", "dual_sim", "fc_group",
          "four_g", "int_memory_group", "m_dep_group", "mobile_wt_group", "n_cores",
          "pc_group", "px_height_group", "px_width_group", "ram", "sc_h_group", "sc_w_group",
          "talk_time_group", "three_g", "touch_screen", "wifi")

train_set = train[vars1]

train_set_num = train_set
for (i in vars1){
  train_set_num[,i] = as.numeric(train_set[,i])
}

train_set_var = train_set[vars2]

train_set_var_num = train_set_var
for (i in vars2){
  train_set_var_num[,i] = as.numeric(train_set_var[,i])
}

str(train_set)
str(train_set_num)
str(train_set_var)
str(train_set_var_num)


#### Comprobacion de normalidad

nor_table=matrix(nc=2, nr=0)
colnames(nor_table)=c("shapiro.test" ," ks.test")

for (i in colnames(train_set_var_num)){
  s_test = shapiro.test(train_set_var_num[,i])
  ks_test = ks.test(train_set_var_num[,i], pnorm, mean(train_set_var_num[,i]), sd(train_set_var_num[,i]))
  
  row=matrix(nc=2, nr=1)
  row[1][1]= s_test$p.value
  row[2][1]= ks_test$p.value
  
  nor_table=rbind(nor_table, row)
  rownames(nor_table)[nrow(nor_table)]=i
}

print(nor_table)


#### tabla de correlaciones

corr=matrix(nc=2, nr=0)
colnames(corr)=c("estimate" ," p-value")

for(i in colnames(train_set_num)){
  spearman_test=cor.test(train_set_num[,i],
                         train_set_num$price_range,
                         method="spearman")
  
  row=matrix(nc=2, nr=1)
  row[1][1]= spearman_test$estimate
  row[2][1]= spearman_test$p.value
  
  corr=rbind(corr, row)
  rownames(corr)[nrow(corr)]=i
  
}
print(corr)


#### Relacion entre poca RAM y bajo precio

# Hipotesis nula: las variables son independientes.
# Hipotesis alternativa: las variables no son independientes

train_set_num = transform(train_set_num, lowRAM= ifelse(ram<3, TRUE, FALSE))
train_set_num = transform(train_set_num, lowPrice= ifelse(price_range<3, TRUE, FALSE))

tablaContingencia = table(train_set_num$lowRAM, train_set_num$lowPrice)

tablaContingenciaMarg = addmargins(tablaContingencia)
tablaContingenciaMarg

test = chisq.test(tablaContingenciaMarg, correct=FALSE)

# Resultado con la funcion chisq.test
test$p.value


#### Modelo de regresion

regresion1 = lm(price_range ~ ram, data = train_set_num)

regresion2 = lm(price_range ~ ram + battery_power_group, data = train_set_num)

regresion3 = lm(price_range ~ ram + battery_power_group + px_height_group + four_g,
                data = train_set_num)

regresion4 = lm(price_range ~ ram + battery_power_group + px_height_group + four_g
                + n_cores, data = train_set_num)

# Tabla resultado
coef=matrix(c( 1, summary(regresion1)$r.squared,
               2, summary(regresion2)$r.squared,
               3, summary(regresion3)$r.squared,
               4, summary(regresion4)$r.squared),
            ncol=2, byrow=TRUE)

colnames(coef)=c("Modelo" ,"R^2")
coef
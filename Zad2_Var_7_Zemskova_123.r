#Земскова Ольга, 123 ПАЭ, 7 вариант - создайте модель множественной линейной регрессии дневных потоков углекислого газа за осенний период 2013 года по данным измерений методом турбулентной пульсации
#Проверяем деррикторию
getwd()
#Устанавливаем нужные пакеты
install.packages("tidyverse")
install.packages("readr")
install.packages("stringr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("tibble")
#Подключаем библиотеки
library("tidyr")
library("tibble")
library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")
#Читаем данные из файла, пропускаем первую строку, заменяем текстовые 'NA', пустые и сгенерированные пороговые значения на NA, игнорируем строки с "[" 
eddypro=read_csv("/Users/olgazemskova/Desktop/Novosibirsk/eddypro.csv", skip=1, na=c("","NA","-9999","-9999.0"), comment=c("["))
#Смотрим что получилось
eddypro=eddypro[-1, ]
eddypro
#Убираем переменную roll, т.к. она содержит только NA
eddypro=select(eddypro,-(roll))
#Преобразуем строковые значения в факторные
eddypro=eddypro %>% mutate_if(is.character, factor)
#Заменяем ненужные символы
names(eddypro)=names(eddypro) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")
#Смотрим на сами переменные, с помощью функции glimpse(), которая более наглядно представляет каждую отдельную переменну, жертвуя при этом представление строчек данных
glimpse(eddypro)
eddypro = drop_na(eddypro)
#Нужно выбрать все переменные типа numeric.
sapply(eddypro,is.numeric)
#Отфильруем данные по заданию - осенний период, дневное время
eddypro=filter(eddypro, DOY >= 243 & DOY < 334)
eddypro=filter(eddypro, daytime==TRUE)
#Переменные типов numeric и не numeric отдельно
eddypro_numeric=eddypro[,sapply(eddypro,is.numeric) ]
eddypro_non_numeric=eddypro[,!sapply(eddypro,is.numeric) ]
#Можем переходить к корелляционному анализу
cor_td = cor(eddypro_numeric)
cor_td
#Полученные результаты довольно тяжело интерпретировать т.к. они выдаются в виде матрицы, поэтому преобразуем матрицу в таблицу, выберем интересующий нас столбец, а из него возьмем только те имена строк(переменных) для которых значения коэффициента детерминации было больше 0,1 (личная прихоть)
cor_td = cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
#Создадим непересекающиеся подвыборки
row_numbers=1:length(eddypro_numeric$co2_flux)
teach=sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
test = row_numbers[-teach]
#Запишем данные из выборок в таблицы
teaching_tbl = eddypro_numeric[teach,]
testing_tbl = eddypro_numeric[test,]
#Модель 1 по обучающей выборке, добавив в нее все переменные с помощью "(.)" и получим информацию о моделе и коэффициенты
mod1 = lm(co2_flux~ (.) , data = teaching_tbl) 
#Коэффициенты
coef(mod1)
#Остатки
resid(mod1)
#Доверительный интервал
confint(mod1)
#P-значения по модели
summary(mod1)
#Дисперсионный анализ
anova(mod1)
#Графическое представление модели:
plot(mod1)

#Создадим модель 2 добавив в неё значимые переменные из результатов функции anova() (со значимостью до 0.1, соответственно ***,** и * пометки)
mod2 = lm(co2_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H 
          + rand_err_H + LE + qc_LE + rand_err_LE + rand_err_co2_flux
          + qc_co2_flux + h2o_flux + rand_err_h2o_flux + H_strg + h2o_v_minus_adv
          + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + co2_molar_density + co2_mole_fraction
          + co2_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature 
          + air_pressure + air_density + air_heat_capacity + air_molar_volume + water_vapor_density + e + es 
          + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot
          + w_rot + max_speed + wind_dir + yaw + pitch + u_star_ + TKE + L + `_z_minus_d__div_L` 
          + T_star_ + x_peak + x_offset + x_10_perc_ + x_30_perc_ + x_50_perc_ + x_70_perc_ + x_90_perc_ + un_Tau 
          + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + u_spikes + ts_var
          + co2_var + w_div_h2o_cov + co2_signal_strength_7200 + flowrate, data = teaching_tbl)
#Получим информацио о моделе и коэффициенты
coef(mod2)
resid(mod2)
confint(mod2)
summary(mod2)
anova(mod2)
#Сравним с предыдущей моделью, не ухудшилась ли она
anova(mod2, mod1)
plot(mod2)

#Создадим модель 3, повторив отбрасывание
mod3 = lm(co2_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H 
          + rand_err_H + LE + qc_LE + rand_err_LE + rand_err_co2_flux
          + qc_co2_flux + h2o_flux + rand_err_h2o_flux + H_strg + h2o_v_minus_adv
          + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + co2_molar_density + co2_mole_fraction
          + co2_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature 
          + air_pressure + air_density + air_heat_capacity + air_molar_volume + water_vapor_density + e + es 
          + specific_humidity + RH + VPD + Tdew + u_unrot + v_unrot + w_unrot + u_rot + v_rot
          + w_rot + max_speed + wind_dir + yaw + pitch + u_star_ + TKE + L + `_z_minus_d__div_L` 
          + T_star_ + x_peak + x_offset + x_10_perc_ + x_30_perc_ + x_50_perc_ + x_90_perc_ + un_Tau 
          + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + u_spikes + ts_var
          + co2_var + w_div_h2o_cov + flowrate, data = teaching_tbl)
#Получим информацио о модели и коэффициенты
coef(mod3)
resid(mod3)
confint(mod3)
summary(mod3)
anova(mod3)
anova(mod3, mod2)
plot(mod3)

#Т.к. по результатам дисперсионного анализа в Модели 3 все переменные больше 0,1 дальнейшее построение моделей не требуется, поэтому можем приступать к кореляционному анализу данных

#Проведем корреляционный анализ переменных
#Выберем из таблицы только участвующие в линейной модели переменные 
cor_teaching_tbl = select(teaching_tbl, co2_flux, DOY, Tau, qc_Tau, rand_err_Tau, H, qc_H, rand_err_H,
                          LE, qc_LE, rand_err_LE, rand_err_co2_flux, qc_co2_flux, h2o_flux, rand_err_h2o_flux,
                          H_strg, h2o_v_minus_adv, h2o_molar_density, h2o_mole_fraction, h2o_mixing_ratio, co2_molar_density, 
                          co2_mole_fraction, co2_mixing_ratio, h2o_time_lag, sonic_temperature, air_temperature, air_pressure, 
                          air_density, air_heat_capacity, air_molar_volume, water_vapor_density, e, es, 
                          specific_humidity, RH, VPD, Tdew, u_unrot, v_unrot, w_unrot, u_rot, v_rot,
                          w_rot, max_speed, wind_dir, yaw, pitch, u_star_, TKE, L, `_z_minus_d__div_L`, 
                          T_star_, x_peak, x_offset, x_10_perc_, x_30_perc_, x_50_perc_, x_90_perc_, un_Tau, 
                          Tau_scf, un_H, H_scf, un_LE, LE_scf, un_co2_flux, u_spikes, ts_var,
                          co2_var, w_div_h2o_cov, flowrate)
#Получаем таблицу коэффициентов корреляций. И подправляем модель 3, убирая из модели одну из двух коррелирующих между собой переменных (начиная от коэффициента >= 0.7)
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

#Графики по полученной моделе
#Построим точки co2_flux от co2_flux на значениях ОБУЧАЮЩЕЙ выборки. Наложим предсказанные значения по модели 3 на ОБУЧАЮЩЕЙ выборке сверху в виде линии
#В идеале линия должна  пройти через все точки
qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
#Теперь сделаем тоже самое на ТЕСТИРУЮЩЕЙ выборе
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
#Т.к. у нас модель зависит от множества переменных, мы можем вывести много графиков зависимостей сo2_flux от учитываемых в моделе параметров
#В идеале предсказанная линия должна пройти через все точки, или как можно ближе к ним на ТЕСТИРУЮЩЕЙ выборке
#Примеры
qplot(DOY, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(h2o_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

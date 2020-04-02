#Земскова Ольга, ПАЭ 123 – для региона 54 рассчитайте урожайность пшеницы в период с 2010 по 2013 год взяв для рассчета средние суммы активных температур за эти годы, с 25 ближайших метеостанций
#Проверка рабочей директории
getwd()
#Устанавливаем пакеты
install.packages("tidyverse")
install.packages("rnoaa")
install.packages("lubridate")
#Подключаем пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)
#Скачиваем список метеостанций
station_data = ghcnd_stations()
#Cохраняем результат
write.csv(station_data,"station_data2020.csv")
station_data = read.csv("station_data2020.csv")
#После получения списка всех станций, выбераем из него список станций ближайших к столице региона,создав таблицу с именем региона и координатами его столицы
novosibirsk=data.frame(id = "NOVOSIBIRSK", latitude = 55.0415000,  longitude = 82.9346000)
#Выбираем конечное число станций, которые имеют необходимые данные в заданный временной период, и выбраем переменные, которые обязательно должны быть в наличии
novosibirsk_around = meteo_nearby_stations(lat_lon_df = novosibirsk, station_data = station_data,limit = 25, var =c("PRCP", "TAVG"),year_min = 2010, year_max = 2013)
#Получение индентификатора метеостанций Новосибирска
novosibirsk_id = novosibirsk_around[["NOVOSIBIRSK"]][["id"]][1]
#Смотрим что внутри
summary (novosibirsk_id)
#Создаем таблицу всех метеостанций вокруг Новосибирска, выбрав целиком первый объект из списка
novosibirsk_table = novosibirsk_around[[1]] 
summary (novosibirsk_table)
#Т.к. по заданию не нужно отфильтровывать станции по расстоянию, то мы формируем список необходимых станций и смотрим, что он содержит
novosibirsk_stations=novosibirsk_table
str(novosibirsk_table)
novosibirsk_stations$id
#Получаем все данные с 1 метеостанции, зная ее идентификатор и смотрим что внутри
all_novosibirsk_data = meteo_tidy_ghcnd(stationid = novosibirsk_id)
summary(all_novosibirsk_data)
#Создадаем промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()
#Создадаем объект, куда скачаем все данные всех метеостанций
all_novosibirsk_meteodata = data.frame()
#Цикл для всех метеостанций
for(i in 1:25)
{
  novosibirsk_id=novosibirsk_around[["NOVOSIBIRSK"]][["id"]]
  print(i)
  print(novosibirsk_id)
  #Выберем нужные свойства 
all_i = meteo_tidy_ghcnd(stationid = novosibirsk_id [i], var="TAVG", date_min="2010-01-01", date_max="2013-12-31")
#Соединяем данные, полученные на предыдущих и данном этапах цикла
all_novosibirsk_meteodata=bind_rows(all_novosibirsk_meteodata, all_i  %>%
                                      mutate(year = year(date), month = month(date)) %>%
                                      group_by(month, year) %>%
                                      mutate(tavg=tavg/10) %>%
                                      filter(tavg>5) %>%
                                      summarise (sum = sum(tavg)))
}
#Записываем полученные результаты
write.csv(all_novosibirsk_meteodata,"all_novosibirsk_meteodata.csv")
#Смотрим как выглядят наши данные сгруппированные по месяцу, году и сумме активных температур
all_novosibirsk_meteodata
#Создание необходимых констант для рассчёта:
afi=c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bfi=c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
dfi=c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#Коэффициент использования ФАР:
Kf=300
#Калорийность урожая культуры:
Qj=1600
#Сумма частей основной и побочной продукции:
Lj=2.2
#Стандартная влажность культуры:
Ej=25
#Сохранение изменений таблицы в векторе now:
now = all_novosibirsk_meteodata %>%
  group_by(month) %>%
  #Рассчёт месячного d и cуммы активных температур для каждой станции:
  summarise(s = mean(sum, na.rm = TRUE)) %>%
  #Добавление данных из таблицы
  #Добавление колонок для вычислений:
  mutate(a = afi, b = bfi, d = dfi) %>%
  #Рассчёт урожайности для каждого месяца:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )
#Урожайность пшеницы с 2010 по 2013 год в Новосибирской области составила (ц/га):
Yield = sum(now$fert)
Yield

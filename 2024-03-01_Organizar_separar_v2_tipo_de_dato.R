#2024-03-01
library(tidyverse)
library(lubridate)

#antes de separar las columnas , verificar tipo de dato en columna dia de actividad
str(actividad_diaria)
str(calorias_por_hora)
str(pasos_por_minuto_largo)

#columna en tipo de dato chr, cambiar en todas las tablas
 

actividad_diaria <- actividad_diaria %>%
	mutate(Dia_de_actividad = mdy(Dia_de_actividad))


calorias_diarias <- calorias_diarias %>%
	mutate(Dia_de_actividad = mdy(Dia_de_actividad))

calorias_por_hora <- calorias_por_hora %>%
	mutate(Hora_de_actividad = ifelse(Hora_de_actividad == "12:00:00 AM", "00:00:00", Hora_de_actividad),
				 Hora_de_actividad = mdy_hms(Hora_de_actividad))


calorias_por_minuto_ancho <- calorias_por_minuto_ancho %>%
	mutate(Hora_de_actividad = ifelse(Hora_de_actividad == "12:00:00 AM", "00:00:00", Hora_de_actividad),
				 Hora_de_actividad = mdy_hms(Hora_de_actividad))


calorias_por_minuto_largo <- calorias_por_minuto_largo %>%
	mutate(Minuto_actividad = ifelse(Minuto_actividad == "12:00:00 AM", "00:00:00", Minuto_actividad),
				 Minuto_actividad = mdy_hms(Minuto_actividad))


dia_de_sueno <- dia_de_sueno %>%
	mutate(Dia_de_sueno = ifelse(Dia_de_sueno == "12:00:00 AM", "00:00:00", Dia_de_sueno),
				 Dia_de_sueno = mdy_hms(Dia_de_sueno))


frecuencia_cardiaca_segundos <- frecuencia_cardiaca_segundos %>%
	mutate(Tiempo = ifelse(Tiempo == "12:00:00 AM", "00:00:00", Tiempo),
				 Tiempo = mdy_hms(Tiempo))


intensidades_diarias <- intensidades_diarias %>%
	mutate(Dia_de_actividad = mdy(Dia_de_actividad))



intensidades_por_hora <- intensidades_por_hora %>%
	mutate(Hora_de_actividad = mdy_hms(ifelse(Hora_de_actividad == "12:00:00 AM", "00:00:00", Hora_de_actividad)))


intensidades_por_minuto_ancho <- intensidades_por_minuto_ancho %>%
	mutate(ActivityHour = mdy_hms(ifelse(ActivityHour == "12:00:00 AM", "00:00:00", ActivityHour)))



intensidades_por_minuto_largo <- intensidades_por_minuto_largo %>%
	mutate(Minuto_de_actividad = mdy_hms(ifelse(Minuto_actividad == "12:00:00 AM", "00:00:00", Minuto_actividad)))


mets_por_minuto_largo <- mets_por_minuto_largo %>%
	mutate(Minuto_actividad = mdy_hms(ifelse(Minuto_actividad == "12:00:00 AM", "00:00:00", Minuto_actividad)))


pasos_diarios <- pasos_diarios %>%
	mutate(Dia_de_actividad = mdy(Dia_de_actividad))


pasos_por_hora <- pasos_por_hora %>%
	mutate(Hora_de_actividad = mdy_hms(ifelse(Hora_de_actividad == "12:00:00 AM", "00:00:00", Hora_de_actividad)))


pasos_por_minuto_ancho <- pasos_por_minuto_ancho %>%
	mutate(Hora_de_actividad = mdy_hms(ifelse(Hora_de_actividad == "12:00:00 AM", "00:00:00", Hora_de_actividad)))


pasos_por_minuto_largo <- pasos_por_minuto_largo %>%
	mutate(Minuto_actividad = mdy_hms(ifelse(Minuto_actividad == "12:00:00 AM", "00:00:00", Minuto_actividad)))

registro_de_peso <- registro_de_peso %>%
	mutate(Fecha = mdy_hms(ifelse(Fecha == "12:00:00 AM", "00:00:00", Fecha)))

sueno_por_minuto <- sueno_por_minuto %>%
	mutate(Fecha = mdy_hms(ifelse(Fecha == "12:00:00 AM", "00:00:00", Fecha)))


#SEPARAR COLUMNAS EN FECHA Y HORA SEGUN CORRESPONDA

# separar en dos columnas la fecha y  hora, y nombrar fecha igual que las anteriores. 
#HAY COLUMNAS DE HR,  QUE TENDRAN NA PORQUE NO APARECE EL REGISTRO 
#SEGUN TABLAS APARECEN MISMMO NUMERO DE NA, 934 EN LAS DE HORA, QUIZAS ES UN USUARIO EL QUE INTRODUCE ASI LOS DATOS. (SON LAS MISMAS FILAS SEGUN TIPO DE REGISTRO)
#PODRIA SER POR LA HORA 24, ESTA REGISTRADA EN DATOS ORIGINALES COMO 12:00:00 AM 

#Con el codigo anterior las horas quedan de 1:00 a 23:00 en el lugar de 00:00 pone n/a .Para el analisis del user se rellenaran de manera manual los datos n/a con la hora adecuada. 


calorias_por_hora <- separate(
	calorias_por_hora,"Hora_de_actividad", into= c("Dia_de_actividad", "Hora"), sep = " ")

pasos_por_hora <- separate(
	pasos_por_hora, "Hora_de_actividad", into = c("Dia_de_actividad", "Hora"), sep = " ")

intensidades_por_hora <- separate(
	intensidades_por_hora, "Hora_de_actividad", into = c("Dia_de_actividad", "Hora"), sep = " ")

calorias_por_minuto_ancho <- separate(
	calorias_por_minuto_ancho, "Hora_de_actividad", into = c("Dia_de_actividad", "Hora"), sep = " ") 


intensidades_por_minuto_ancho <- separate(
	intensidades_por_minuto_ancho, "ActivityHour", into = c("Dia_de_actividad", "Hora"), sep = " ") 

pasos_por_minuto_ancho <- separate(
	pasos_por_minuto_ancho, "Hora_de_actividad", into = c("Dia_de_actividad", "Hora"), sep = " ") 


calorias_por_minuto_largo <- separate(
	calorias_por_minuto_largo, "Minuto_actividad",into = c("Dia_de_actividad", "Hora"), sep = " ") 

intensidades_por_minuto_largo <- separate(
	intensidades_por_minuto_largo, "Minuto_actividad",into = c("Dia_de_actividad", "Hora"), sep = " ")

mets_por_minuto_largo <- separate(
	mets_por_minuto_largo, "Minuto_actividad",into = c("Dia_de_actividad", "Hora"), sep = " ")

pasos_por_minuto_largo <- separate(
	pasos_por_minuto_largo, "Minuto_actividad",into = c("Dia_de_actividad", "Hora"), sep = " ")

dia_de_sueno <- separate(
	dia_de_sueno, "Dia_de_sueno", into = c("Dia_de_actividad", "Hora"), sep = " ")

sueno_por_minuto <- separate(
	sueno_por_minuto, "Fecha",  into = c("Dia_de_actividad", "Hora"), sep = " ")

frecuencia_cardiaca_segundos <- separate(
	frecuencia_cardiaca_segundos, "Tiempo", into = c("Dia_de_actividad", "Hora"), sep = " ")

registro_de_peso <- separate(
	registro_de_peso, "Fecha",  into = c("Dia_de_actividad", "Hora"), sep = " ")


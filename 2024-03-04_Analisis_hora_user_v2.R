library(tidyverse)
library(hms)


#dividir la columna Hora en fecha y hora, y convertirlas a tipo POSIXct para asegurar que cada hora se asocie correctamente a la fecha
frecuencia_cardiaca_segundos2 <- frecuencia_cardiaca_segundos %>%
	mutate(Fecha_Hora = as.POSIXct(paste(Dia_de_actividad, Hora), format = "%Y-%m-%d %H:%M:%S"))

#agrupar por hora y calcular la media de la frecuencia
frecuencia_cardiaca_hora <- frecuencia_cardiaca_segundos2 %>%
	mutate(Hora = floor_date(Fecha_Hora, unit = "hour")) %>%
	group_by(Id, Dia_de_actividad, Hora) %>%
	summarise(Media_Valor = mean(Valor))

frecuencia_cardiaca_hora <- frecuencia_cardiaca_hora %>%
	mutate(Hora = format(Hora, "%H:%M:%S"))


#convertir la columna Hora en tipo POSIXct para no perder datos
sueno_por_minuto2 <- sueno_por_minuto %>%
	mutate(Fecha_Hora = as.POSIXct(paste(Dia_de_actividad, Hora), format = "%Y-%m-%d %H:%M:%S"))

#agrupar por hora y calcular la media de la frecuencia
sueno_por_hr <- sueno_por_minuto2 %>%
	mutate(Hora = floor_date(Fecha_Hora, unit = "hour")) %>%
	group_by(Id, Dia_de_actividad, Hora) %>%
	summarise(Valor_promedio = mean(Valor))

#formatear la columna hr 
sueno_por_hr <- sueno_por_hr %>%
	mutate(Hora = format(Hora, "%H:%M:%S"))

#mets_por_minuto_largo

mets_por_minuto_largo2 <- mets_por_minuto_largo %>%
	mutate(Fecha_Hora = as.POSIXct(paste(Dia_de_actividad, Hora), format = "%Y-%m-%d %H:%M:%S"))

mets_por_hr <- mets_por_minuto_largo2 %>% 
	mutate(Hora = floor_date(Fecha_Hora, unit = "hour")) %>%
	group_by(Id, Dia_de_actividad, Hora) %>%
	summarise(Equivalente_metabolico = mean(Equivalente_metabolico))

mets_por_hr <- mets_por_hr %>% 
	mutate(Hora = format(Hora, "%H:%M:%S"))
	
	

# filtrar para las demas tablas a utilizar, usuario

frecuencia_hr_user <- frecuencia_cardiaca_hora %>%
	filter(Id == 6962181067)

#cambiar nombre a la columna media_valor
frecuencia_hr_user <- frecuencia_hr_user %>%
	rename(Frecuencia_cardiaca = Media_Valor)


calorias_hora_user <- calorias_por_hora %>% 
	filter(Id == 6962181067)


intensidades_hora_user <- intensidades_por_hora %>% 
	filter(Id == 6962181067)


pasos_hora_user  <- pasos_por_hora %>% 
	filter(Id == 6962181067)



sueno_hr_user <- sueno_por_hr %>% 
	filter(Id == 6962181067)

sueno_hr_user <- sueno_hr_user %>%
	rename(Valor_sueno = Valor_promedio)

mets_hr_user <- mets_por_hr %>% 
	filter(Id == 6962181067)
	

#error por duplicacion, salian varios valores repetidos, se hace el promedio para cada combinacion de hr id etc.#falto tambien meter mets




#valores n/a revisar tablas de user, para identificar las horas , la hora 00:00:00 tiene un error y se duplican haciendo que aparezcan n/a en las demas variables

View(frecuencia_hr_user)
View(calorias_hora_user)
View(intensidades_hora_user)
View(pasos_hora_user)
View(sueno_hr_user)

#los valores n/a en la columna hora de las tablas  los aplicare para solucion practica como si fueran 00:00:00, y hare la media entre el n/a y el 00:00:00

#reemplazar los valores NA en la columna "Hora" por "00:00:00"


frecuencia_hr_user <- frecuencia_hr_user %>%
	mutate(Hora = if_else(is.na(Hora), "00:00:00", Hora))

#hacer media de esos dos valore3 00:00:00 que ahora aparecen 

frecuencia_hr_user <- frecuencia_hr_user %>%
	group_by(Id, Dia_de_actividad, Hora) %>%
	summarise(Media_Frecuencia_cardiaca = mean(Frecuencia_cardiaca, na.rm = TRUE))

#lo mismo para las demas tablas

#tabla calorias no existe duplicado de 00, ya que noe staba en segundos, solo cambiar n/a por 00

calorias_hora_user <- calorias_hora_user %>% 
	mutate(Hora = if_else(is.na(Hora), "00:00:00", Hora))

intensidades_hora_user <- intensidades_hora_user %>% 
	mutate(Hora = if_else(is.na(Hora), "00:00:00", Hora))

pasos_hora_user <- pasos_hora_user %>% 
	mutate(Hora = if_else(is.na(Hora), "00:00:00", Hora))
	
# tabla sueño ocuree lo mismo que en la de frecuencias

sueno_hr_user <- sueno_hr_user %>% 
	mutate(Hora = if_else(is.na(Hora), "00:00:00", Hora))


sueno_hr_user <- sueno_hr_user %>%
	group_by(Id, Dia_de_actividad, Hora) %>%
	summarise(Valor_sueno = mean(Valor_sueno, na.rm = TRUE))

#mets 

mets_hr_user <- mets_hr_user %>% 
	mutate(Hora = if_else(is.na(Hora), "00:00:00", Hora))


mets_hr_user <- mets_hr_user %>% 
	group_by(Id, Dia_de_actividad, Hora) %>%
	summarise(Equivalente_metabolico = mean(Equivalente_metabolico, na.rm = TRUE))
	




#hacer Join con nuevos datos
#desaparecio el id de sueno y frecuencia, error al hacer el join

user_1067_por_hr_v2 <- frecuencia_hr_user %>%
	group_by(Id, Dia_de_actividad, Hora) %>%
	summarise(Media_Frecuencia_cardiaca = mean(Media_Frecuencia_cardiaca)) %>%
	full_join(
		calorias_hora_user %>%
			group_by(Id, Dia_de_actividad, Hora) %>%
			summarise(Calorias = mean(Calorias)),
		by = c("Id", "Dia_de_actividad", "Hora")
	) %>%
	full_join(
		intensidades_hora_user %>%
			group_by(Id, Dia_de_actividad, Hora) %>%
			summarise(Intensidad_total = mean(Intensidad_total), Promedio_intensidad = mean(Promedio_intensidad)),
		by = c("Id", "Dia_de_actividad", "Hora")
	) %>%
	full_join(
		pasos_hora_user %>%
			group_by(Id, Dia_de_actividad, Hora) %>%
			summarise(Total_pasos = mean(Total_pasos)),
		by = c("Id", "Dia_de_actividad", "Hora")
	) %>%
	full_join(
		sueno_hr_user %>%
			group_by(Id, Dia_de_actividad, Hora) %>%
			summarise(Valor_sueno = mean(Valor_sueno)),
		by = c("Id", "Dia_de_actividad", "Hora")
	) %>%
	full_join(
		mets_hr_user %>%
			group_by(Id, Dia_de_actividad, Hora) %>%
			summarise(Equivalente_metabolico = mean(Equivalente_metabolico)),
		by = c("Id", "Dia_de_actividad", "Hora")
	)



View(user_1067_por_hr_v2)

# hay datos en valor sueño que aparrecen n/a porque no existe registro de sueño , luego hay datos en que se registra intensidad 3  y valor en sueño
#esto se debe a que seguramente al ser por minutos y la tabla de intensiades por hora, hay algunos minutos en que hubo intensidad, solo cambiare los n/a de la tabla sueño, los demas quedan asi para el analisis
#en la mayoria de los casos son en el inicio de registro del sueño y al final, me imaginoque son por las variable hr que en alguna tabla estaba en minutos y otras en hr.
#asumiremos para el caso que esto, y las otras en medio de las horas de sueño, sera las veces que se levanto, esto se puede verificar en la tabla dia de sueño

#cambiar n/a columna Valor_sueno de user_1067_por_hr_v2


user_1067_por_hr_v2 <- user_1067_por_hr_v2 %>%
	mutate(Valor_sueno = replace_na(Valor_sueno, 0))

#valores n/a en frecuencia cardiaca, las demás en 0,  menos en calores y met, siempre a la misma hora, 
#para efecto practico del analisis se supondra ,que el dispositivo no estaba directamente con el usuario sino que se cargaba

# poner valor 0 en cada columna. 

user_1067_por_hr_v2 <- user_1067_por_hr_v2 %>%
	mutate(Media_Frecuencia_cardiaca = replace_na(Media_Frecuencia_cardiaca, 0))


#ANALISIS DE USUARIO POR HORA

estadisticas_user_hr <- summary(user_1067_por_hr_v2)

print(estadisticas_user_hr)

View(user_1067_por_hr_v2)
print(user_1067_por_hr_v2)
colnames(user_1067_por_hr_v2)

#cambiar tipo de dato en fecha y hora
user_1067_por_hr_v2 <- user_1067_por_hr_v2 %>%
	mutate(Hora = as.POSIXct(Hora, format = "%H:%M:%S"))

user_1067_por_hr_v2 <- user_1067_por_hr_v2 %>%
	mutate(Dia_de_actividad = as.Date(Dia_de_actividad))


User_1067_por_hr_cor <- user_1067_por_hr_v2 %>%
	ungroup() %>%
	select(-Id, -Dia_de_actividad, -Hora) %>%
	cor(use = "pairwise.complete.obs")


User_1067_hr_cor_redondeado <- round(User_1067_por_hr_cor, 2)
print(User_1067_hr_cor_redondeado)
View(User_1067_hr_cor_redondeado)



summary(user_1067_por_hr_v2)




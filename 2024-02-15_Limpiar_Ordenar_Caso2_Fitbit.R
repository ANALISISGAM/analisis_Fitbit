#Para que sea mas sencillo creare una lista que contenga todos los dataframe.

library(readr)
library(tidyverse)
# Directorio donde se encuentran los archivos CSV
carpeta <- "C:\\Users\\iphone\\Documents\\2024-02-02_archivos_clase_R\\Caso_Fit_bit"
#dobles barras invertidas para que el código sea legible



# Obtener la lista de archivos CSV en el directorio sin la extension...
archivos <- list.files(path = carpeta, pattern = "\\.csv$", full.names = TRUE)

# Crear una lista para almacenar los data frames
lista_dataframes <- list()

# Leer los archivos CSV y almacenarlos en la lista
#bucle for para iterar en cada archivo  de la lista que se creo 
#crear nombre archivo  quitando la extension 
for (archivo in archivos) {  
	nombre <- tools::file_path_sans_ext(basename(archivo)) 
	lista_dataframes[[nombre]] <- read.csv(archivo)
}

# Verificar el contenido de la lista de data frames
print(lista_dataframes)

print(nombre)


# Obtener los nombres de los data frames en la lista
nombres_dataframes <- names(lista_dataframes)

# Imprimir los nombres de los data frames
print(nombres_dataframes)

#TRADUCIR NOMBRE DE CADA DATAFRAME PARA FACILITAR LA MANIPULACION.

nombres_traducidos <- c(
	"Actividad_diaria",
	"Calorías_diarias",
	"Intensidades_diarias",
	"Pasos_diarios_fusionados",
	"Frecuencia_cardíaca_en_segundos",
	"Calorías_por_hora",
	"Intensidades_por_hora",
	"Pasos_por_hora",
	"Calorías_por_minuto_largo",
	"Calorías_por_minuto_ancho",
	"Intensidades_por_minuto_largo",
	"Intensidades_por_minuto_ancho",
	"METs_por_minuto_estilo_largo",
	"Sueño_por_minuto",
	"Pasos_por_minuto_largo",
	"Pasos_por_minuto_ancho",
	"Día_de_sueño",
	"Información_de_registro_de_peso"
)

# Asignar nuevos nombres a los data frames dentro de la lista
names(lista_dataframes) <- nombres_traducidos





# Crear variables para cada dataframe para que sea mas sencillo acceder sin necesidad de pasar por la lista 
# Los corchetes son para acceder al elemento de la lista (python), se utilizan dobles corchetes para 
# Acceder al elemento real dentro de la lista, no solo al nombre sino el dataframe completo. 
# Otra manera de acceder es con $ accede directo al dataframe. 

actividad_diaria <- lista_dataframes[["Actividad_diaria"]]
calorias_diarias <- lista_dataframes[["Calorías_diarias"]]
intensidades_diarias <- lista_dataframes[["Intensidades_diarias"]]
pasos_diarios <- lista_dataframes[["Pasos_diarios_fusionados"]]
frecuencia_cardiaca_segundos <- lista_dataframes[["Frecuencia_cardíaca_en_segundos"]]
calorias_por_hora <- lista_dataframes[["Calorías_por_hora"]]
intensidades_por_hora <- lista_dataframes[["Intensidades_por_hora"]]
pasos_por_hora <- lista_dataframes[["Pasos_por_hora"]]
calorias_por_minuto_largo <- lista_dataframes[["Calorías_por_minuto_largo"]]
calorias_por_minuto_ancho <- lista_dataframes[["Calorías_por_minuto_ancho"]]
intensidades_por_minuto_largo <- lista_dataframes[["Intensidades_por_minuto_largo"]]
intensidades_por_minuto_ancho <- lista_dataframes[["Intensidades_por_minuto_ancho"]]
mets_por_minuto_largo <- lista_dataframes[["METs_por_minuto_estilo_largo"]]
sueño_por_minuto <- lista_dataframes[["Sueño_por_minuto"]]
pasos_por_minuto_largo <- lista_dataframes[["Pasos_por_minuto_largo"]]
pasos_por_minuto_ancho <- lista_dataframes[["Pasos_por_minuto_ancho"]]
día_de_sueño <- lista_dataframes[["Día_de_sueño"]]
registro_de_peso <- lista_dataframes[["Información_de_registro_de_peso"]]

# Acceder a df mediante la variable

actividad_diaria

# Ver columnas actividad diaria

colnames(actividad_diaria)

#renombrar columnas traducidas para mejor manejo



actividad_diaria <- actividad_diaria %>%
	rename(
		Id = Id,
		Fecha_actividad = ActivityDate,
		Pasos_totales = TotalSteps,
		Distancia_total = TotalDistance,
		Distancia_rastreador = TrackerDistance,
		Distancia_actividades_registradas = LoggedActivitiesDistance,
		Distancia_mucha_actividad = VeryActiveDistance,
		Distancia_activad_moderada = ModeratelyActiveDistance,
		Distancia_activad_ligera = LightActiveDistance,
		Distancia_activad_sedentaria = SedentaryActiveDistance,
		Minutos_muy_activos = VeryActiveMinutes,
		Minutos_moderadamente_activos = FairlyActiveMinutes,
		Minutos_ligeramente_activos = LightlyActiveMinutes,
		Minutos_sedentarios = SedentaryMinutes,
		Calorías = Calories
	)

colnames(actividad_diaria)



#ver nombres de los dataframe

print(nombres_traducidos)

# Traducción de columnas df "Calorias_diarias"

colnames(calorias_diarias)

# Renombrar columnas de calorias_diarias

calorias_diarias <- calorias_diarias %>% 
	rename(
		Id = Id,
		Dia_de_actividad = ActivityDay,
		Calorias = Calories
	)
 
# Revisar nombres de columnas 

colnames(calorias_diarias)

# Ver nombres de columnas a traducir

colnames(intensidades_diarias)

intensidades_diarias <- intensidades_diarias %>% 
	rename(
		Id = Id,
		Dia_de_actividad = ActivityDay,
		Minutos_sedentarios = SedentaryMinutes,
		Minutos_ligeramente_activos = LightlyActiveMinutes,
		Minutos_moderadamente_activos = FairlyActiveMinutes,
		Minutos_muy_activos  = VeryActiveMinutes,
		Distancia_activad_sedentaria = SedentaryActiveDistance,
		Distancia_activad_ligera	= LightActiveDistance,
		Distancia_activad_moderada = ModeratelyActiveDistance,
		Distancia_mucha_actividad	= VeryActiveDistance
	)

#Error al nombrar columnas ActivityDay volver a renombrar en ambos df


intensidades_diarias <- intensidades_diarias %>% 
	rename(
		Fecha_actividad = Dia_de_actividad
	)


calorias_diarias <- calorias_diarias %>% 
	rename(
		Fecha_actividad = Dia_de_actividad
	)


# Comprobar 

colnames(calorias_diarias)
colnames(intensidades_diarias)
colnames(actividad_diaria)

#Corregir nombres de columnas donde pone activad por actividad  en df actividad diaria e intensidades 

intensidades_diarias <- intensidades_diarias %>% 
	rename(
    Distancia_actividad_moderada = Distancia_activad_moderada,
    Distancia_actividad_ligera = Distancia_activad_ligera,
    Distancia_actividad_sedentaria = Distancia_activad_sedentaria
	)

actividad_diaria <- actividad_diaria %>% 
	rename(
		Distancia_actividad_moderada = Distancia_activad_moderada,
		Distancia_actividad_ligera = Distancia_activad_ligera,
		Distancia_actividad_sedentaria = Distancia_activad_sedentaria
	)

# Renombrar pasos diarios 

colnames(pasos_diarios)

pasos_diarios <- pasos_diarios %>% 
	rename(
		Dia_de_actividad = ActivityDay,
		Total_pasos = StepTotal
	)

# Aqui aparecia StepTotal en vez de TotalSteps no se si se trata del mismo dato, lo diferenciare por ahora
#corregir otras tablas, existe ActivityDay y ActiviteDate. 

intensidades_diarias <- intensidades_diarias %>% 
	rename(
		Dia_de_actividad = Fecha_actividad
	)


calorias_diarias <- calorias_diarias %>% 
	rename(
		Dia_de_actividad = Fecha_actividad 
	)

# Renombrar columnas dailyCalories
colnames(frecuencia_cardiaca_segundos)


frecuencia_cardiaca_segundos <- frecuencia_cardiaca_segundos %>% 
	rename(
		Tiempo = Time,
		Valor = Value
	)

# Renombrar resto de columnas de los demás DF

calorias_por_hora <- calorias_por_hora %>% 
	rename (
		Hora_de_actividad = ActivityHour,
    Calorias = Calories
	)

#Corregir calorias, en activiad diaria esta con acento , lo quitare para evitar errores

actividad_diaria <- actividad_diaria %>% 
	rename(
		Calorias = Calorías
	)

intensidades_por_hora <- intensidades_por_hora %>% 
	rename(
		Hora_de_actividad = ActivityHour,
		Intensidad_total = TotalIntensity,
	  Promedio_intensidad	= AverageIntensity
	)

pasos_por_hora <- pasos_por_hora %>% 
	rename(
		Hora_de_actividad = ActivityHour,
		Total_pasos = StepTotal
	)


calorias_por_minuto_largo <- calorias_por_minuto_largo %>% 
	rename(
	  Minuto_actividad	=  ActivityMinute,
	  Calorias = Calories
	)

calorias_por_minuto_ancho <- calorias_por_minuto_ancho %>% 
	rename(
		Hora_de_actividad = ActivityHour
	)

intensidades_por_minuto_largo <- intensidades_por_minuto_largo %>% 
	rename(
		Minuto_actividad  =	ActivityMinute,
		Intensidad = Intensity
	)

colnames(mets_por_minuto_largo)
head(mets_por_minuto_largo$METs) #Equivalente metabólico, energía gastada en actividad en  relación con el gasto de energía en reposo.

mets_por_minuto_largo <- mets_por_minuto_largo %>% 
	rename(
		Minuto_actividad	=  ActivityMinute,
		Equivalente_metabolico =  METs
	)

# cambiar nombre de df que contengan ñ para que no genere errores en traducción , sueño por minuto y dia de sueño 
#CREO QUE ESTO QUEDARA PENDIENTE PARA MAS ADELANTE... ASI DA ERROR. =( 
dia_de_sueno <- lista_dataframes[["Día_de_sueño"]]
sueno_por_minuto <- lista_dataframes[["Sueño_por_minuto"]]

#	SOLUCION : 
# Cambiar los nombres de los dataframes dentro de la lista
# habia que asignar los nombres, dentro de la lista, en el vector nombres traducidos que se uso antes
names(lista_dataframes) <- nombres_traducidos

# Acceder a los dataframes utilizando los nuevos nombres traducidos
dia_de_sueno <- lista_dataframes[["Día_de_sueño"]]
sueno_por_minuto <- lista_dataframes[["Sueño_por_minuto"]]

#seguir renombrando columnas 

head(sueno_por_minuto$date)
head(sueno_por_minuto$logId)

sueno_por_minuto <- sueno_por_minuto %>% 
	rename(
		Fecha = date,
		Valor = value,
	)
#pendiente fecha no se si es el mismo calor que fecha_actividad, dia_actividad,

pasos_por_minuto_largo <- pasos_por_minuto_largo %>% 
	rename(
		Minuto_actividad	=  ActivityMinute,
		Pasos = Steps
	)

pasos_por_minuto_ancho <- pasos_por_minuto_ancho %>% 
	rename(
		Hora_de_actividad = ActivityHour
	)

dia_de_sueno <- dia_de_sueno %>% 
	rename(
		Dia_de_sueno = SleepDay,
		Registros_totales_sueño = TotalSleepRecords,
		Minutos_totales_dormido = TotalMinutesAsleep,
		Tiempo_total_en_cama = TotalTimeInBed
	)



registro_de_peso <- registro_de_peso %>% 
	rename(
		Fecha = Date,
		Peso_kg = WeightKg,
		Peso_libras = WeightPounds,
		Grasa = Fat,
		IMC = BMI,
		Reporte_manual = IsManualReport
	)


# 	Cambiar nombre de fecha para que tengan el mismo nombre
actividad_diaria <- actividad_diaria %>% 
	rename(
		Dia_de_actividad = Fecha_actividad 
	)

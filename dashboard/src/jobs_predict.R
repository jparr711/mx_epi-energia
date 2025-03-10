
#### libraries ####
library(caret)
library(pander)
library(e1071)
library(quanteda)
require(quanteda.textmodels)
library(tm)
library(tidyverse)
library(textcat)
# install.packages('cld2')
library(cld2)
library(RPostgreSQL)
# library(mlr)
library(data.table)
library(tidytext)
library(hunspell)
library(DT)
# install.packages("topicmodels")
library(topicmodels)
library(broom)
library(scales)
library(reshape2)
library(sjPlot)
library(sjmisc)
library(corrr)
library(igraph)
# library(ggraph)
library(janitor)
library(gplots)
library(ruimtehol)
library(textstem)
library(data.table)
library(magrittr)
library(glmnet)
rm(list = ls())
#### Pull data from SQL ####
### R - pulling in data from SQL dbase
pg=dbDriver("PostgreSQL")
con = dbConnect(pg, host = "mexico-fco-db.cmjm2q5ghgua.us-east-1.rds.amazonaws.com",
                dbname = "postgres", user = "mexicoFCO", password = "HC2g8Q7^taOq", port=5432)


dbListTables(con)   #list all the tables 

#query the database and store the data in datafame

# postgresqlpqExec(con, "SET client_encoding = 'utf-8'")

set_utf8 <- function(x) {
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)
  x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")
  # Same on column names:
  Encoding(names(x)) <- "UTF-8"
  x
}

scraped_jobs <- set_utf8(dbGetQuery(con, "SELECT * from jobs_dbase limit 100000;"))

rm(pg, con)

#### Clean ####

# cld2::detect_language()

# remove confusing terms from desc.
scraped_jobs <- scraped_jobs %>% 
  mutate(description = str_remove(description, 'Show less|click apply|Search Jobs|
                                  Start typing a location to get suggestions, then press the down arrow to select one.|
                                  View Job'))

#### Municipalities and State - final filtering and cleaning ####
scraped_jobs <- 
  scraped_jobs %>%
  dplyr::mutate(state = str_trim(state),
                municipality = str_trim(municipality)) %>%
  dplyr::mutate(municipality = str_remove(municipality, "Fracc |Centro |Ciudad del |Ciudad |Pueblo ")) %>%
  dplyr::group_by(municipality, state) %>%
  dplyr::mutate(state = ifelse(state == "None", municipality, state),
                state = case_when(state == "Ciudad de México (Distrito Federal)" ~ "Ciudad de México",
                                  state == "Puebla (de los Angeles)" ~ "Puebla",
                                  state == "Estado de México" ~ "México",
                                  TRUE ~ as.character(state))) %>%
  dplyr::mutate(municipality = case_when(municipality == "Jalisco" & state == "Nayarit" ~ "Xalisco", 
                                         municipality == "Santa Elena" & state == "Colima" ~ "Colima", 
                                         municipality == "Nuevo" & state == "Veracruz" ~ "Veracruz", 
                                         municipality == "Benito Juárez" & state == "Sinaloa" ~ "Navolato",
                                         municipality == "Álvaro Obregón" & state == "Zacatecas" ~ "Sombrerete",
                                         municipality == "Santa Clara" & state == "Michoacán" ~ "Puruándiro",
                                         municipality == "Reforma" & state == "Durango" ~ "Durango",
                                         municipality == "Estados Unidos" & state == "Nuevo León" ~ "Galeana",
                                         municipality == "Vista Hermosa" & state == "Puebla" ~ "Puebla",
                                         municipality == "Tulantepec de Lugo Guerrero" & state == "Hidalgo" ~ "Santiago Tulantepec de Lugo Guerrero",
                                         municipality == "Sauces" & state == "Sinaloa" ~ "Culiacán",
                                         municipality == "San Bartolo" & state == "México" ~ "Tianguistenco",
                                         municipality == "Morelos" & state == "Colima" ~ "Colima",
                                         municipality == "Los Reyes" & state == "México" ~ "La Paz",
                                         municipality == "Los Mochis" & state == "Sinaloa" ~ "Ahome",
                                         
                                         
                                         municipality == "Primero de Mayo" & state == "Durango" ~ "Durango",
                                         municipality == "Presas" & state == "Hidalgo" ~ "Tezontepec de Aldama",
                                         municipality == "Popola" & state == "Yucatán" ~ "Valladolid",
                                         municipality == "Juárez" & state == "Ciudad de México" ~ "Benito Juárez",
                                         municipality == "Estado de Hidalgo" & state == "Ciudad de México" ~ "Álvaro Obregón",
                                         municipality == "Coacalco" & state == "México" ~ "Coacalco de Berriozábal",
                                         municipality == "15 de Agosto" & state == "Ciudad de México" ~ "Gustavo A. Madero",
                                         municipality == "Valles" & state == "San Luis Potosí" ~ "Ciudad Valles",
                                         municipality == "Tlajomulco" & state == "Jalisco" ~ "Tlajomulco de Zúñiga",
                                         municipality == "Tezontepec" & state == "Hidalgo" ~ "Tezontepec de Aldama",
                                         municipality == "Santiago" & state == "Puebla" ~ "Puebla",
                                         municipality == "Pantano Grande" & state == "Nayarit" ~ "Santiago Ixcuintla",
                                         municipality == "Miguel Hidalgo" & state == "Oaxaca" ~ "Oaxaca de Juárez",
                                         municipality == "Miguel Hidalgo" & state == "Veracruz" ~ "Poza Rica de Hidalgo",
                                         municipality == "Madero" & state == "Tamaulipas" ~ "Ciudad Madero",
                                         
                                         municipality == "Lerma" & state == "Tabasco" ~ "Centla",
                                         municipality == "Jurica" & state == "Querétaro" ~ "Querétaro",
                                         municipality == "Interlomas" & state == "México" ~ "Huixquilucan",
                                         municipality == "Guerrero" & state == "Guerrero" ~ "Tixtla de Guerrero",
                                         municipality == "Escobedo" & state == "Nuevo León" ~ "General Escobedo",
                                         municipality == "Colomos" & state == "Colima" ~ "Comala",
                                         
                                         
                                         
                                         
                                         municipality == "Benito Juárez" & state == "Oaxaca" ~ "Oaxaca de Juárez",
                                         municipality == "1ro de Mayo" & state == "Baja California" ~ "Mexicali",
                                         municipality == "Juárez" & state == "Colima" ~ "Colima",
                                         municipality == "Ejidal Emiliano Zapata" & state == "México" ~ "Ecatepec de Morelos",
                                         municipality == "Benito Juárez" & state == "Tamaulipas" ~ "San Fernando",
                                         municipality == "Metepec" & state == "Puebla" ~ "Atlixco",
                                         municipality == "Solidaridad" & state == "Tamaulipas" ~ "Matamoros",
                                         municipality == "Cholula de Rivadavia" & state == "Puebla" ~ "Cholula",
                                         municipality == "Benito Juárez" & state == "Nuevo León" ~ "Juárez",
                                         municipality == "Barrio San Pablo Tolimán " & state == "Querétaro" ~ "Tolimán",
                                         municipality == "Barrio San Pablo Tolimán" & state == "Querétaro" ~ "Tolimán",
                                         
                                         
                                         TRUE ~ as.character(municipality)),
                
                state = case_when(municipality == "Nicolás Romero" & state == "Michoacán" ~ "México",
                                  municipality == "Morelia" & state == "México" ~ "Michoacán",
                                  municipality == "Monterrey" & state == "Chiapas" ~ "Nuevo León",
                                  municipality == "Chilpancingo de los Bravo" & state == "Chilpancingo de los Bravo" ~ "Guerrero",
                                  municipality == "Torreón" & state == "NaN" ~ "Coahuila",
                                  municipality == "Ixcateopan de Cuauhtémoc" & state == "Ixcateopan de Cuauhtémoc" ~ "Guerrero",
                                  municipality == "Villa Hermosa" & state == "Villa Hermosa" ~ "Tabasco",
                                  municipality == "Villa Ahumada" & state == "Villa Ahumada" ~ "Chihuahua",
                                  municipality == "Monterrey" & state == "NaN" ~ "Nuevo León",
                                  municipality == "Cuauhtémoc" & state == "Cuauhtémoc" ~ "Chihuahua",
                                  municipality == "Chilpancingo de los Bravos" & state == "Chilpancingo de los Bravos" ~ "Guerrero",
                                  municipality == "Acapulco" & state == "Acapulco" ~ "Guerrero",
                                  
                                  TRUE ~ as.character(state)),
                
                municipality = case_when(municipality == "Club de Golf México" ~ "Tlalpan",
                                         municipality == "San Buenaventura" ~ "Tlalpan",
                                         municipality == "Ecatepec" ~ "Ecatepec de Morelos",
                                         municipality == "Naucalpan" ~ "Naucalpan de Juárez",
                                         municipality == "Culiacan" ~ "Culiacán",
                                         municipality == "Boca del Rio" ~ "Boca del Río",
                                         municipality == "Benito Juarez" ~ "Benito Juárez",
                                         municipality == "Kanasin" ~ "Kanasín",
                                         municipality == "Acapulco" ~ "Acapulco de Juárez",
                                         
                                         
                                         municipality == "San Luis Rio Colorado" ~ "San Luis Río Colorado",
                                         municipality == "San Luis Potosi" ~ "San Luis Potosí",
                                         municipality == "Tultitlan" ~ "Tultitlán",
                                         municipality == "Cordoba" ~ "Córdoba",
                                         municipality == "Tuxtla Gutierrez" ~ "Tuxtla Gutiérrez",
                                         municipality == "Torreon" ~ "Torreón",
                                         municipality == "Merida" ~ "Mérida",
                                         municipality == "Alvaro Obregón" ~ "Álvaro Obregón",
                                         municipality == "Alvaro Obregon" ~ "Álvaro Obregón",
                                         municipality == "Álvaro Obregon" ~ "Álvaro Obregón",
                                         
                                         municipality == "Queretaro" ~ "Querétaro",
                                         municipality == "Unidad Hab o Farril" ~ "Puebla",
                                         municipality == "Barrio Montecillo" ~ "San Luis Potosí",
                                         municipality == "El Roble Agrícola" ~ "Mérida",
                                         municipality == "Bosque de los Remedios" ~ "Naucalpan de Juárez",
                                         municipality == "San Sebastián Tecoloxtitlan" ~ "Iztapalapa",
                                         municipality == "Polotitlán de la Ilustración" ~ "Polotitlán",
                                         municipality == "Rinconada San Jerónimo" ~ "Puebla",
                                         municipality == "San Luis Apizaquito 3ra Secc" ~ "Apizaco",
                                         municipality == "Unidad Hab Bosques del Alba" ~ "Cuautitlán Izcalli",
                                         
                                         municipality == "Parque Industrial Dinatech" ~ "Hermosillo",
                                         municipality == "Cuitzeo (La Estancia)" ~ "Poncitlán",
                                         municipality == "Hacienda Linda Vista" ~ "Tijuana",
                                         municipality == "Ejido Latinoamericana" ~ "Guanajuato",
                                         municipality == "Pueblo Santa Fe" ~ "Álvaro Obregón",
                                         municipality == "Barrio José López Portillo" ~ "Montemorelos",
                                         municipality == "Unidad Hab San Pedro" ~ "San Pedro Garza García",
                                         municipality == "Ojo de Agua de la Trinidad" ~ "Apaseo el Alto",
                                         municipality == "Ampl Simón Bolívar" ~ "Venustiano Carranza",
                                         municipality == "Unidad Hab Bosques del Alba" ~ "Cuautitlán Izcalli",
                                         
                                         municipality == "Zona Milit 17vo Batallón de Infant" ~ "Centro",
                                         municipality == "Unidad Hab El Recreo" ~ "Azcapotzalco",
                                         municipality == "Tijuana Municipality" ~ "Tijuana",
                                         municipality == "Mextepec (Ex-Hacienda Mextepex)" ~ "Mextepec",
                                         municipality == "Fraccionamiento Real Palmas" ~ "General Escobedo",
                                         municipality == "Oaxaca" ~ "Oaxaca de Juárez",
                                         municipality == "Ex- Hacienda Candiani" ~ "Oaxaca de Juárez",
                                         municipality == "Nogales Municipality" ~ "Nogales",
                                         municipality == "2do Sect de Fidelac" ~ "Lázaro Cárdenas",
                                         municipality == "Tultitlán de Mariano Escobedo" ~ "Tultitlán",
                                         municipality == "Lomas de Nueva York" ~ "Aguascalientes",
                                         
                                         municipality == "Jardines de San Rafael" ~ "Guadalupe",
                                         municipality == "Infonavit La Curva" ~ "Topolobampo",
                                         municipality == "Ejido Heredia y Anexas" ~ "Guerrero",
                                         municipality == "Colonia Santa Fe" ~ "Álvaro Obregón",
                                         municipality == "Victoria de Durango (Durango)" ~ "Durango",
                                         municipality == "Tecámac de Felipe Villanueva" ~ "Tecámac",
                                         municipality == "Sultepec de Pedro Ascencio Alquisiras" ~ "Sultepec",
                                         municipality == "Parque Industrial Cerillo II" ~ "Lerma",
                                         municipality == "Cuatrociénegas de Carranza" ~ "Cuatro Ciénegas",
                                         municipality == "Cuatrociénegas" ~ "Cuatro Ciénegas",
                                         municipality == "Ciudad Universitaria" ~ "Coyoacán",
                                         municipality == "Universitaria" ~ "Coyoacán",
                                         
                                         
                                         municipality == "Unidad Hab Plutarco Elías Calles" ~ "Iztapalapa",
                                         municipality == "San José del Cabo" ~ "Los Cabos",
                                         municipality == "Salitrillos (Yucatán)" ~ "Monclova",
                                         municipality == "Lic Adolfo López Mateos" ~ "Villahermosa",
                                         municipality == "Industrial Vallejo" ~ "Azcapotzalco",
                                         municipality == "Paseos del Conquistador" ~ "Mérida",
                                         municipality == "Colonia Lomas de Chapultepec" ~ "Miguel Hidalgo",
                                         municipality == "Colonia Bosques de las Lomas" ~ "Miguel Hidalgo",
                                         municipality == "Barrio San Pablo Tolimán" ~ "Tolimán",
                                         municipality == "Cuatrociénegas de Carranza" ~ "Cuatrociénegas",
                                         municipality == "Ciudad Universitaria" ~ "Coyoacán",
                                         
                                         municipality == "Zona Ind Kalos" ~ "Guadalupe",
                                         municipality == "Las Américas" ~ "Morelia",
                                         municipality == "Tlalnepantla" ~ "Tlalnepantla de Baz",
                                         municipality == "Ciudad del Carmen" ~ "Carmen",
                                         municipality == "Huastecas de Lampasitos" ~ "Reynosa",
                                         municipality == "Lago Sur" ~ "Tijuana",
                                         municipality == "Cancún" ~ "Benito Juárez",
                                         municipality == "Puerto Cancún" ~ "Benito Juárez",
                                         municipality == "Los Ángeles" ~ "Ensenada",
                                         municipality == "Lindavista" ~ "Zamora",
                                         municipality == "Villahermosa" ~ "Centro",
                                         municipality == "Tlalpan " ~ "Tlalpan",
                                         
                                         municipality == "Santa Fe" ~ "Álvaro Obregón",
                                         municipality == "Poza Rica" ~ "Poza Rica de Hidalgo",
                                         municipality == "Ejido Monterrey" ~ "Lerdo",
                                         municipality == "Ciudad Juárez" ~ "Juárez",
                                         municipality == "Colonia Polanco" ~ "Miguel Hidalgo",
                                         municipality == "Silao" ~ "Silao de la Victoria",
                                         municipality == "Ciudad Obregón" ~ "Cajeme",
                                         municipality == "Chicoloapan de Juárez" ~ "Chicoloapan",
                                         municipality == "Santa Rosa" ~ "Valle Hermoso",
                                         municipality == "Punta Arena" ~ "Los Cabos",
                                         municipality == "Tlaquepaque" ~ "San Pedro Tlaquepaque",
                                         
                                         municipality == "Colonia Nápoles" ~ "Benito Juárez",
                                         municipality == "Granja" ~ "Zapopan",
                                         municipality == "Granados " ~ "Granados",
                                         municipality == "Ciudad Juárez" ~ "Juárez",
                                         municipality == "Colonia Polanco" ~ "Miguel Hidalgo",
                                         municipality == "Silao" ~ "Silao de la Victoria",
                                         municipality == "Obregón" ~ "Cajeme",
                                         municipality == "Chicoloapan de Juárez" ~ "Chicoloapan",
                                         municipality == "Santa Rosa" ~ "Valle Hermoso",
                                         municipality == "Punta Arena" ~ "Los Cabos",
                                         municipality == "San Lucas" ~ "Los Cabos",
                                         municipality == "Multé" ~ "Balancán",
                                         
                                         municipality == "Matzaco" ~ "Izúcar de Matamoros",
                                         municipality == "Lampazos" ~ "Lampazos de Naranjo",
                                         municipality == "La Ceiba" ~ "Centro",
                                         municipality == "Ejido Poana" ~ "Tacotalpa",
                                         municipality == "Valle de Chalco" ~ "Valle de Chalco Solidaridad",
                                         municipality == "San Juan del Rio" ~ "San Juan del Río",
                                         municipality == "Juchitán de Zaragoza" ~ "Heroica Ciudad de Juchitán de Zaragoza",
                                         municipality == "Ixtlahuaca de Rayón" ~ "Ixtlahuaca",
                                         municipality == "Colonia Vallejo" ~ "Gustavo A. Madero",
                                         
                                         
                                         
                                         municipality == "San Juan de Ocotán" ~ "Zapopan",
                                         municipality == "San José de las Palmas II" ~ "San Martín de las Pirámides",
                                         municipality == "Cadereyta" ~ "Cadereyta Jiménez",
                                         municipality == "Alfredo V Bonfil" ~ "Villa de Álvarez",
                                         municipality == "Lomas de Ciénega" ~ "Ciénega de Flores",
                                         municipality == "Pachuca de Soto " ~ "Pachuca de Soto",
                                         municipality == "Juventino Rosas" ~ "Santa Cruz de Juventino Rosas",
                                         municipality == "Arboledas Santa Elena" ~ "Pachuca de Soto",
                                         municipality == "Villa Hermosa" ~ "Tezonapa",
                                         municipality == "Topolobampo" ~ "Ahome",
                                         municipality == "Tetla" ~ "Tetla de la Solidaridad",
                                         municipality == "Tetanchopo" ~ "Navojoa",
                                         municipality == "Tastiota" ~ "Hermosillo",
                                         municipality == "San Juan Cuautlancingo" ~ "Cuautlancingo",
                                         municipality == "Sahagún" ~ "Tepeapulco",
                                         municipality == "Rancho Monte Adentro" ~ "Macuspana",
                                         
                                         municipality == "Palo Alto" ~ "Vallecillo",
                                         municipality == "Sinaloa de Leyva" ~ "Sinaloa",
                                         municipality == "San Lorenzo Tezonco" ~ "Iztapalapa",
                                         municipality == "San Juan de las Huertas" ~ "Zinacantepec",
                                         municipality == "Roma Norte" ~ "Cuauhtémoc",
                                         municipality == "Roma" ~ "Cuauhtémoc",
                                         
                                         municipality == "Colonia Roma" ~ "Cuauhtémoc",
                                         municipality == "México Libre" ~ "Antiguo Morelos",
                                         municipality == "Lomas de Chapultepec" ~ "Miguel Hidalgo",
                                         municipality == "León" ~ "Miguel Hidalgo",
                                         municipality == "Capilla de Guadalupe" ~ "Tepatitlán de Morelos",
                                         municipality == "Arcoiris Ll" ~ "La Paz",
                                         municipality == "Arboledas" ~ "Guadalupe",
                                         municipality == "Zamora de Hidalgo" ~ "Zamora",
                                         municipality == "Yucatán" ~ "Mérida",
                                         municipality == "Villahermosa" ~ "Centro",
                                         municipality == "Chetumal" ~ "Othón P. Blanco",
                                         municipality == "Chalet" ~ "Matamoros",
                                         
                                         
                                         municipality == "Villa Hermosa" ~ "Centro",
                                         municipality == "San Francisco Coacalco" ~ "Coacalco de Berriozábal",
                                         municipality == "Rosarito" ~ "Playas de Rosarito",
                                         municipality == "Res Altaria" ~ "Aguascalientes",
                                         municipality == "Playas de Tijuana" ~ "Tijuana",
                                         municipality == "Parque Real" ~ "Zapopan",
                                         municipality == "Paredón" ~ "Tonalá",
                                         municipality == "Nuevo Solistahuacán" ~ "Pueblo Nuevo Solistahuacan",
                                         municipality == "Nueva Atzacoalco" ~ "Gustavo A. Madero",
                                         municipality == "Monaco" ~ "Chihuahua",
                                         municipality == "El Aguaje 2000" ~ "San Luis Potosí",
                                         municipality == "Aguaje 2000" ~ "San Luis Potosí",
                                         
                                         
                                         municipality == "Granada" ~ "Miguel Hidalgo",
                                         municipality == "Barrio Octayo" ~ "San Felipe Orizatlán",
                                         municipality == "Rancho El Orito" ~ "Zacatecas",
                                         municipality == "Ejido Monterrey" ~ "Lerno",
                                         municipality == "Tutitlan" ~ "Tultitlán",
                                         municipality == "San Juan Capistrano" ~ "Valparaíso",
                                         municipality == "San Onofre" ~ "Michoacán",
                                         municipality == "El Pueblito" ~ "Corregidora",
                                         municipality == "López Mateos" ~ "Atizapán de Zaragoza",
                                         municipality == "Pachuca" ~ "Pachuca de Soto",
                                         municipality == "Jilotepec de Abasolo" ~ "Jilotepec",
                                         municipality == "Cuajimalpa" ~ "Cuajimalpa de Morelos",
                                         municipality == "San Andrés" ~ "Huautla", 
                                         municipality == "Carrizal" ~ "Centro", 
                                         municipality == "La Joya" ~ "Tijuana", 
                                         municipality == "Guillermo Prieto" ~ "Nuevo Ideal",
                                         municipality == "Tlalpan Centro" ~ "Tlalpan",
                                         TRUE ~ as.character(municipality))) %>%
  dplyr::mutate(municipality = str_trim(municipality))

#### detect language ####
scraped_jobs <- scraped_jobs %>% 
  mutate(language = cld2::detect_language(description))


scraped_jobs_en <- scraped_jobs %>% filter(language == 'en')
scraped_jobs_es <- scraped_jobs %>% filter(language == 'es')



# english
scraped_jobs_en$sector <- gsub("([a-z])([A-Z])", "\\1, \\2", scraped_jobs_en$sector)
scraped_jobs_en$`function` <- gsub("([a-z])([A-Z])", "\\1, \\2", scraped_jobs_en$`function`)

# Remove - Direct message the job poster
scraped_jobs_en <- 
  scraped_jobs_en %>%
  dplyr::mutate(description = gsub("^Direct message the job poster.*", "", description))

# spanish
scraped_jobs_es$sector <- gsub("([a-z])([A-Z])", "\\1, \\2", scraped_jobs_es$sector)
scraped_jobs_es$`function` <- gsub("([a-z])([A-Z])", "\\1, \\2", scraped_jobs_es$`function`)


scraped_jobs_en <-
  scraped_jobs_en %>%
  mutate(`function` = gsub(",\\sand", ",", `function`))

scraped_jobs_es <-
  scraped_jobs_es %>%
  mutate(`function` = gsub(",\\sand", ",", `function`))



scraped_jobs_en <-
  scraped_jobs_en %>%
  mutate(sector = gsub(",\\sand", ",", sector))

scraped_jobs_es <-
  scraped_jobs_es %>%
  mutate(sector = gsub(",\\sand", ",", sector))


scraped_jobs_en <- 
  scraped_jobs_en %>%
  dplyr::mutate(sector = gsub(",\\s", ",", sector)) %>%
  rownames_to_column(var="row") %>% 
  dplyr::mutate(row = as.numeric(as.character(row))) %>%
  dplyr::filter(source == "LinkedIn") %>%
  separate_rows(sector, sep = ',') %>%
  dplyr::count(link, row, sector) %>% 
  spread(sector, n, fill = 0) %>%
  janitor::clean_names() %>%
  dplyr::left_join(scraped_jobs_en, .)

scraped_jobs_es <- 
  scraped_jobs_es %>%
  dplyr::mutate(sector = gsub(",\\s", ",", sector)) %>%
  rownames_to_column(var="row") %>% 
  dplyr::mutate(row = as.numeric(as.character(row))) %>%
  dplyr::filter(source == "LinkedIn") %>%
  separate_rows(sector, sep = ',') %>%
  dplyr::count(link, row, sector) %>% 
  spread(sector, n, fill= 0) %>%
  janitor::clean_names() %>%
  dplyr::left_join(scraped_jobs_es, .)



#### Supervised learning -  SVM Algorithm to predict tags ####

sectors_col <- colnames(scraped_jobs_en[, -which(names(scraped_jobs_en) %in% c("title","company", "date", "location", "municipality", "state",
                                                                               "salary", "synopsis", "description", "link", "sector", "level",
                                                                               "function", "source", "type", "language", "row"))])



# sectors_col <- c("oil_energy", "renewables_environment")

for (i in sectors_col) {
  print(i)
  print(table(scraped_jobs_en[[i]]))
}

scraped_jobs_en_pred <- scraped_jobs_en

for (i in sectors_col) {
  
  scraped_jobs_en$text <- scraped_jobs_en$description
  scraped_jobs_en$doc_id <- scraped_jobs_en$link
  corp_jobs <- quanteda::corpus(scraped_jobs_en)
  corp_jobs$id_numeric <- 1:ndoc(corp_jobs)
  
  # generate random numbers without replacement
  set.seed(300)
  # id_train <- sample(1:ndoc(corp_jobs), round(ndoc(corp_jobs)/2, 0), replace = FALSE)
  
  
  # get training set
  dfmat_training <- corpus_subset(corp_jobs, source == "LinkedIn") %>%
    dfm(remove = stopwords("english"), remove_punct = TRUE, stem = TRUE)
  
  # get test set (documents not in id_train)
  dfmat_test <- corpus_subset(corp_jobs, source != "LinkedIn") %>%
    dfm(remove = stopwords("english"), remove_punct = TRUE, stem = TRUE)
  
  tmod_svm <- textmodel_svm(dfmat_training, quanteda::docvars(dfmat_training, i))
  # summary(tmod_svm)
  
  dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))
  
  actual_class <- quanteda::docvars(dfmat_matched, i)
  predicted_class <- predict(tmod_svm, newdata = dfmat_matched)
  predicted_class <- as.data.frame(predicted_class)
  predicted_class <- tibble::rownames_to_column(predicted_class, var = "link")
  names(predicted_class)[2] <- paste0(i, "_", "pred")
  scraped_jobs_en_pred <- dplyr::left_join(scraped_jobs_en_pred, predicted_class)
}


sectors_col <- colnames(scraped_jobs_es[, -which(names(scraped_jobs_es) %in% c("title","company", "date", "location", "municipality", "state",
                                                                               "salary", "synopsis", "description", "link", "sector", "level",
                                                                               "function", "source", "type", "language", "row"))])


scraped_jobs_es_pred <- scraped_jobs_es

for (i in sectors_col) {
  
  scraped_jobs_es$text <- scraped_jobs_es$description
  scraped_jobs_es$doc_id <- scraped_jobs_es$link
  corp_jobs <- quanteda::corpus(scraped_jobs_es)
  corp_jobs$id_numeric <- 1:ndoc(corp_jobs)
  
  # generate random numbers without replacement
  set.seed(300)
  # id_train <- sample(1:ndoc(corp_jobs), round(ndoc(corp_jobs)/2, 0), replace = FALSE)
  
  
  # get training set
  dfmat_training <- corpus_subset(corp_jobs, source == "LinkedIn") %>%
    dfm(remove = stopwords("spanish"), remove_punct = TRUE, stem = TRUE)
  
  # get test set (documents not in id_train)
  dfmat_test <- corpus_subset(corp_jobs, source != "LinkedIn") %>%
    dfm(remove = stopwords("spanish"), remove_punct = TRUE, stem = TRUE)
  
  tmod_svm <- textmodel_svm(dfmat_training, quanteda::docvars(dfmat_training, i))
  # summary(tmod_svm)
  
  dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))
  
  actual_class <- quanteda::docvars(dfmat_matched, i)
  predicted_class <- predict(tmod_svm, newdata = dfmat_matched)
  predicted_class <- as.data.frame(predicted_class)
  predicted_class <- tibble::rownames_to_column(predicted_class, var = "link")
  names(predicted_class)[2] <- paste0(i, "_", "pred")
  scraped_jobs_es_pred <- dplyr::left_join(scraped_jobs_es_pred, predicted_class)
}




#### subsector tagging ####

# wind
scraped_jobs_en_pred <-
  scraped_jobs_en_pred %>%
  dplyr::mutate(subsector_wind = str_count(str_remove_all(tolower(description), "[[:punct:]]"), " wind |wind energy")) %>%
  dplyr::mutate(subsector_wind = ifelse(subsector_wind != 0, 1, 0))

# energy eff
scraped_jobs_en_pred <-
  scraped_jobs_en_pred %>%
  dplyr::mutate(count_1 = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "energy|oil |gas ")) %>%
  dplyr::mutate(subsector_energy_efficiency = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "energy efficiency|energy efficient")) %>% 
  dplyr::mutate(subsector_energy_efficiency = ifelse(subsector_energy_efficiency != 0 & count_1 != 0, 1, 0)) %>%
  dplyr::select(-count_1)


# biomass
scraped_jobs_en_pred <-
  scraped_jobs_en_pred %>%
  dplyr::mutate(subsector_biomass = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "biomass|bioenerg|bio-energ|biofuel|bio-fuel|biogas|bio-gas|residual waste|solid waste")) %>% 
  dplyr::mutate(subsector_biomass = ifelse(subsector_biomass != 0, 1, 0)) 



# automobiles

scraped_jobs_en_pred <- 
  scraped_jobs_en_pred %>%
#  dplyr::mutate(count_1 = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "energy|oil |gas ")) %>%
  dplyr::mutate(subsector_electromobility = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "hybrid car|hybrid vehicle|hybrid automobile|electric vehicle|hybrid automobile|tesla|chargepoint|self driving")) %>% 
  dplyr::mutate(subsector_electromobility = ifelse(subsector_electromobility != 0, 1, 0)) 

# solar
scraped_jobs_en_pred <-
  scraped_jobs_en_pred %>%
  dplyr::mutate(subsector_solar = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "solar")) %>% 
  dplyr::mutate(subsector_solar = ifelse(subsector_solar != 0, 1, 0))

# solar pv
scraped_jobs_en_pred <-
  scraped_jobs_en_pred %>%
  dplyr::mutate(subsector_solar_pv = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "solar pv|photovoltaic")) %>% 
  dplyr::mutate(subsector_solar_pv = ifelse(subsector_solar_pv != 0 & subsector_solar != 0, 1, 0))

# solar thermal
scraped_jobs_en_pred <- 
  scraped_jobs_en_pred %>%
  dplyr::mutate(subsector_solar_thermal = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "solar thermal")) %>% 
  dplyr::mutate(subsector_solar_thermal = ifelse(subsector_solar_thermal != 0 & subsector_solar != 0, 1, 0))


###
# downstream 
scraped_jobs_en_pred <- 
  scraped_jobs_en_pred %>%
  dplyr::mutate(count_1 = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "oil |gas ")) %>%
  dplyr::mutate(subsector_downstream = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "downstream|refinery|refineries|refining|refined|purifying|purification")) %>% 
  dplyr::mutate(subsector_downstream = ifelse(subsector_downstream > 1 & count_1 != 0, 1, 0)) %>%
  dplyr::select(-count_1)


# midstream
scraped_jobs_en_pred <- 
  scraped_jobs_en_pred %>%
  dplyr::mutate(count_1 = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "oil |gas ")) %>%
  dplyr::mutate(subsector_midstream = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "midstream|pipelines|trucking|railroad|logistics|rail|barge|storage|wholesale")) %>% 
  dplyr::mutate(subsector_midstream = ifelse(subsector_midstream > 1 & count_1 != 0, 1, 0)) %>%
  dplyr::select(-count_1)

# upstream
scraped_jobs_en_pred <- 
  scraped_jobs_en_pred %>%
  dplyr::mutate(count_1 = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "oil |gas ")) %>%
  dplyr::mutate(subsector_upstream = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "upstream|oil exploration|gas exploration|oil production|gas production|exploration & production|exploration and production|drilling|exploratory wells ")) %>% 
  dplyr::mutate(subsector_upstream = ifelse(subsector_upstream > 1 & count_1 != 0, 1, 0)) %>%
  dplyr::select(-count_1)



scraped_jobs_en_pred <-
  scraped_jobs_en_pred %>%
  dplyr::mutate(oil_energy_pred = case_when(oil_energy_pred == 0 & subsector_downstream == 1 ~ 1,
                                     oil_energy_pred == 0 & subsector_midstream == 1 ~ 1,
                                     oil_energy_pred == 0 & subsector_upstream == 1 ~ 1,
                                     oil_energy == 0 & is.na(oil_energy_pred) & subsector_downstream == 1 ~ 1,
                                     oil_energy == 0 & is.na(oil_energy_pred) & subsector_midstream == 1 ~ 1,
                                     oil_energy == 0 & is.na(oil_energy_pred) & subsector_upstream == 1 ~ 1,
                                     TRUE ~ as.numeric(as.character(oil_energy_pred)))) %>%
  mutate(subsector_oil_gas_crosscutting = case_when(subsector_downstream == 1 & subsector_midstream == 1 ~ 1,
                                                    subsector_downstream == 1 & subsector_upstream == 1 ~ 1,
                                                    subsector_midstream == 1 & subsector_midstream == 1 ~ 1,
                                                    TRUE ~ 0)) 

# renewables tag 
scraped_jobs_en_pred <-
  scraped_jobs_en_pred %>%
  mutate(renewables_environment_pred = case_when(renewables_environment_pred == 0 & subsector_wind == 1 ~ 1,
                                                 renewables_environment_pred == 0 & subsector_energy_efficiency == 1 ~ 1,
                                                 renewables_environment_pred == 0 & subsector_electromobility == 1 ~ 1,
                                                 renewables_environment_pred == 0 & subsector_solar == 1 ~ 1,
                                                 renewables_environment == 0 & is.na(renewables_environment_pred) & subsector_wind == 1 ~ 1,
                                                 renewables_environment == 0 & is.na(renewables_environment_pred) & subsector_energy_efficiency == 1 ~ 1,
                                                 renewables_environment == 0 & is.na(renewables_environment_pred) & subsector_electromobility == 1 ~ 1,
                                                 renewables_environment == 0 & is.na(renewables_environment_pred) & subsector_solar == 1 ~ 1,
                                                 renewables_environment == 0 & is.na(renewables_environment_pred) & subsector_biomass == 1 ~ 1,
                                                 TRUE ~ as.numeric(as.character(renewables_environment_pred)))) %>%
  mutate(subsector_renewables_crosscutting = case_when(subsector_wind == 1 & subsector_energy_efficiency == 1 ~ 1,
                                                       subsector_wind == 1 & subsector_electromobility == 1 ~ 1,
                                                       subsector_wind == 1 & subsector_solar == 1 ~ 1,
                                                       subsector_wind == 1 & subsector_biomass == 1 ~ 1,
                                                       
                                                       subsector_energy_efficiency == 1 & subsector_electromobility == 1 ~ 1,
                                                       subsector_energy_efficiency == 1 & subsector_solar == 1 ~ 1,
                                                       subsector_energy_efficiency == 1 & subsector_biomass == 1 ~ 1,
                                                       
                                                       subsector_electromobility == 1 & subsector_solar == 1 ~ 1,
                                                       subsector_electromobility == 1 & subsector_biomass == 1 ~ 1,
                                                       
                                                       subsector_solar == 1 & subsector_biomass == 1 ~ 1,
                                                       
                                                       TRUE ~ 0)) 

# subsector_wind subsector_energy_efficiency subsector_electromobility subsector_solar

# subsector tagging - spanish

# wind
scraped_jobs_es_pred <-
  scraped_jobs_es_pred %>%
  dplyr::mutate(subsector_wind = str_count(str_remove_all(tolower(description), "[[:punct:]]"), 
                                           "aerogenerador|turbina|eólic|eolic|masas del aire| viento")) %>%
  dplyr::mutate(subsector_wind = ifelse(subsector_wind != 0, 1, 0))

# energy eff
scraped_jobs_es_pred <-
  scraped_jobs_es_pred %>%
  dplyr::mutate(count_1 = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "energy|oil |gas ")) %>%
  dplyr::mutate(subsector_energy_efficiency = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "eficiencia energética|eficiencia energetica|ahorro energético|iluminación|iluminación led|fluorescente|tragalu")) %>% 
  dplyr::mutate(subsector_energy_efficiency = ifelse(subsector_energy_efficiency != 0 & count_1 != 0, 1, 0)) %>%
  dplyr::select(-count_1)


# %>% 
#   dplyr::filter(subsector_electromobility != 0) %>%
#   nrow(.)

# automobiles

scraped_jobs_es_pred <- 
  scraped_jobs_es_pred %>%
 # dplyr::mutate(count_1 = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "energy|oil |gas ")) %>%
  dplyr::mutate(subsector_electromobility = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "carro eléctrico|carro electrico|auto eléctrico|auto electrico|vehículo eléctrico|vehiculo electrico|híbrido")) %>% 
  dplyr::mutate(subsector_electromobility = ifelse(subsector_electromobility != 0, 1, 0)) 

# biomass
scraped_jobs_es_pred <-
  scraped_jobs_es_pred %>%
  dplyr::mutate(subsector_biomass = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "biomasa|bioenerg|bio-energ|biocombustible|bio-combustible|biogás|bio-gás|residuo|residuo sólido")) %>% 
  dplyr::mutate(subsector_biomass = ifelse(subsector_biomass != 0, 1, 0)) 


# solar
scraped_jobs_es_pred <-
  scraped_jobs_es_pred %>%
  dplyr::mutate(subsector_solar = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "cuidado de panel| solar ")) %>% 
  dplyr::mutate(subsector_solar = ifelse(subsector_solar != 0, 1, 0)) 


# solar pv
scraped_jobs_es_pred <-
  scraped_jobs_es_pred %>%
  #  dplyr::mutate(subsector_solar = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "cuidado de panel| solar ")) %>% 
  dplyr::mutate(subsector_solar_pv = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "solar fv |fotovoltaic")) %>% 
  dplyr::mutate(subsector_solar_pv = ifelse(subsector_solar_pv != 0 & subsector_solar != 0, 1, 0)) 


# solar thermal
scraped_jobs_es_pred <- 
  scraped_jobs_es_pred %>%
  #  dplyr::mutate(subsector_solar = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "cuidado de panel| solar ")) %>% 
  dplyr::mutate(subsector_solar_thermal = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "termosifon|termosifón|solar térmica|solar termica")) %>% 
  dplyr::mutate(subsector_solar_thermal = ifelse(subsector_solar_thermal != 0 & subsector_solar != 0, 1, 0)) 




###
# downstream 
scraped_jobs_es_pred <- 
  scraped_jobs_es_pred %>%
  dplyr::mutate(count_1 = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "oil |petroleo|gas |gasolina")) %>%
  dplyr::mutate(subsector_downstream = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "downstream|refinery|refinamiento|procesamiento|refinación|refinería|refined|refinado|purificación |purification")) %>% 
  dplyr::mutate(subsector_downstream = ifelse(subsector_downstream != 0 & count_1 != 0, 1, 0)) %>%
  dplyr::select(-count_1) 

# %>% 
#   dplyr::filter(subsector_downstream != 0) %>%
#   nrow(.)

# midstream
scraped_jobs_es_pred <- 
  scraped_jobs_es_pred %>%
  dplyr::mutate(count_1 = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "oil |petroleo|gas |gasolina")) %>%
  dplyr::mutate(subsector_midstream = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "midstream|ferrocarril|barcaza|camión|tubería|almacenamiento")) %>% 
  dplyr::mutate(subsector_midstream = ifelse(subsector_midstream != 0 & count_1 != 0, 1, 0)) %>%
  dplyr::select(-count_1) 

# upstream
scraped_jobs_es_pred <- 
  scraped_jobs_es_pred %>%
  dplyr::mutate(count_1 = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "oil |petroleo|gas |gasolina")) %>%
  dplyr::mutate(subsector_upstream = str_count(str_remove_all(tolower(description), "[[:punct:]]"), "exploración y producción|e&p|exploración de petroleo|exploración de gasolina|yacimientos de gasolina|yacimientos de petróleo|yacimiento de gasolina|yacimiento de petróleo|gas exploration|oil production|gas production|exploration & production|exploration and production|drilling|perforación|pozo")) %>% 
  dplyr::mutate(subsector_upstream = ifelse(subsector_upstream != 0 & count_1 != 0, 1, 0)) %>%
  dplyr::select(-count_1)



scraped_jobs_es_pred <-
  scraped_jobs_es_pred %>%
  ungroup(.) %>%
  mutate(oil_energy_pred = case_when(oil_energy_pred == 0 & subsector_downstream == 1 ~ 1,
                                     oil_energy_pred == 0 & subsector_midstream == 1 ~ 1,
                                     oil_energy_pred == 0 & subsector_upstream == 1 ~ 1,
                                     oil_energy == 0 & is.na(oil_energy_pred) & subsector_downstream == 1 ~ 1,
                                     oil_energy == 0 & is.na(oil_energy_pred) & subsector_midstream == 1 ~ 1,
                                     oil_energy == 0 & is.na(oil_energy_pred) & subsector_upstream == 1 ~ 1,
                                     TRUE ~ as.numeric(as.character(.$oil_energy_pred)))) %>%
  mutate(subsector_oil_gas_crosscutting = case_when(subsector_downstream == 1 & subsector_midstream == 1 ~ 1,
                                                    subsector_downstream == 1 & subsector_upstream == 1 ~ 1,
                                                    subsector_midstream == 1 & subsector_midstream == 1 ~ 1,
                                                    TRUE ~ 0)) 

# renewables tag 
scraped_jobs_es_pred <-
  scraped_jobs_es_pred %>%
  ungroup(.) %>%
  mutate(renewables_environment_pred = case_when(renewables_environment_pred == 0 & subsector_wind == 1 ~ 1,
                                                 renewables_environment_pred == 0 & subsector_energy_efficiency == 1 ~ 1,
                                                 renewables_environment_pred == 0 & subsector_electromobility == 1 ~ 1,
                                                 renewables_environment_pred == 0 & subsector_solar == 1 ~ 1,
                                                 renewables_environment == 0 & is.na(renewables_environment_pred) & subsector_wind == 1 ~ 1,
                                                 renewables_environment == 0 & is.na(renewables_environment_pred) & subsector_energy_efficiency == 1 ~ 1,
                                                 renewables_environment == 0 & is.na(renewables_environment_pred) & subsector_electromobility == 1 ~ 1,
                                                 renewables_environment == 0 & is.na(renewables_environment_pred) & subsector_solar == 1 ~ 1,
                                                 renewables_environment == 0 & is.na(renewables_environment_pred) & subsector_biomass == 1 ~ 1,
                                                 TRUE ~ as.numeric(as.character(renewables_environment_pred)))) %>%
  mutate(subsector_renewables_crosscutting = case_when(subsector_wind == 1 & subsector_energy_efficiency == 1 ~ 1,
                                                       subsector_wind == 1 & subsector_electromobility == 1 ~ 1,
                                                       subsector_wind == 1 & subsector_solar == 1 ~ 1,
                                                       subsector_wind == 1 & subsector_biomass == 1 ~ 1,
                                                       
                                                       subsector_energy_efficiency == 1 & subsector_electromobility == 1 ~ 1,
                                                       subsector_energy_efficiency == 1 & subsector_solar == 1 ~ 1,
                                                       subsector_energy_efficiency == 1 & subsector_biomass == 1 ~ 1,
                                                       
                                                       subsector_electromobility == 1 & subsector_solar == 1 ~ 1,
                                                       subsector_electromobility == 1 & subsector_biomass == 1 ~ 1,
                                                       
                                                       subsector_solar == 1 & subsector_biomass == 1 ~ 1,
                                                       
                                                       TRUE ~ 0)) 




##### filter out non relevant jobs #####

scraped_jobs_en_pred <-
  scraped_jobs_en_pred %>%
  dplyr::filter(oil_energy == 1|oil_energy_pred == 1|renewables_environment == 1|renewables_environment_pred == 1)

scraped_jobs_es_pred <-
  scraped_jobs_es_pred %>%
  dplyr::filter(oil_energy == 1|oil_energy_pred == 1|renewables_environment == 1|renewables_environment_pred == 1)

#### Clean company ####




#### clean title ####



final_sj <- 
  dplyr::bind_rows(scraped_jobs_es_pred, scraped_jobs_en_pred)

final_sj_es <- final_sj %>%
  dplyr::filter(language == "es")
  
final_sj_en <- final_sj %>%
  dplyr::filter(language == "en")




# Final wrangling




# sectors

# subsectors (not ready)

# relationship between sectors

# others: skill level, function

jobs.corpus <- scraped_jobs_en_pred


jobs.corpus <- Corpus(VectorSource(jobs.corpus$description))


jobs.corpus <- tm_map(jobs.corpus, tolower)
as.character(jobs.corpus[[1]]) # examine the changes to job posting #1

jobs.corpus <- tm_map(jobs.corpus, removeNumbers)
jobs.corpus <- tm_map(jobs.corpus, removePunctuation)

for (j in seq(jobs.corpus)) {
  
  jobs.corpus[[j]] <- gsub("/", " ", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("microsoft word", "microsoft_word", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("communication skills", "communication_skills", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("computer science", "computer_science", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("panel install", "panel_installation", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("high school|high-school", "high_school", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("trade school", "trade_school", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("electrical engineering", "electrical_engineering", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("civil engineering", "civil_engineering", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("environmental engineering", "environmental_engineering", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("mechanical_engineering", "mechanical engineering", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("social science", "social_science", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("computer science", "computer_science", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("data science", "data_science", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("law degree", "law_degree", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("data analysis", "data_analysis", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("program management", "program_management", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("project management", "project_management", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("client relations", "client_relations", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("legal services", "legal_services", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("program evaluation", "program_evaluation", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("economic evaluation", "economic_evaluation", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("information technology", "information_technology", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("iso 45001|iso-45001|iso45001", "iso_45001", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("iso 14001|iso-14001|iso14001", "iso_14001", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("iso 9001|iso-9001|iso9001", "iso_9001", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("iso 50001|iso-50001|iso50001", "iso_50001", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("iso 26001|iso-26001|iso26001", "iso_26001", jobs.corpus[[j]])
  
  
  jobs.corpus[[j]] <- gsub("battery electric vehicle", "battery_electric_vehicle", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("hybrid electric vehicle", "hybrid_electric_vehicle", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("plug-in hybrid electric vehicle|plug in hybrid electric vehicle", "plugin_hybrid_electric_vehicle", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("fuel cell electric vehicle", "fuel_cell_electric_vehicle", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("private electric car", "private_electric_car", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("electric taxi", "electric_taxi", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("electric bus", "electric_bus", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("electric truck", "electric_truck", jobs.corpus[[j]])
  
  jobs.corpus[[j]] <- gsub("electric light commercial vehicle", "electric_light_commercial_vehicle", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("electric scooter", "electric_scooter", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("electric bicycle", "electric_bicycle", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("lithium batter", "lithium_batter", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("electric vehicle manufacturing", "electric_vehicle_manufacturing", jobs.corpus[[j]])
  
  jobs.corpus[[j]] <- gsub("electric vehicle supply equipment", "electric_vehicle_supply_equipment", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("electric vehicle connector", "electric_vehicle_connector", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("retrofitting vehicle", "retrofitting_vehicle", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("battery reuse|battery recycling", "battery_reuse_recycling", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("vehicle grid connection", "vehicle_grid_connection", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("charging center", "charging_center", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("fleeting operation", "fleeting_operation", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("ancillary grid service", "ancillary_grid_service", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("electric vehicle charging network management", "electric_vehicle_charging_network_management", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("battery management system", "battery_management_system", jobs.corpus[[j]])
  jobs.corpus[[j]] <- gsub("electric vehicle training", "electric_vehicle_training", jobs.corpus[[j]])
  
  
  
}

remove(j)

jobs.corpus <- tm_map(jobs.corpus, stripWhitespace)
jobs.corpus <- tm_map(jobs.corpus, removeWords, stopwords("english")) # remove commonly-used words (e.g., "is", "what", etc.)

library(SnowballC)
# jobs.corpus <- tm_map(jobs.corpus, stemDocument) # Removing common word endings (e.g., “ing”, “es”, “s”)


df <- TermDocumentMatrix(jobs.corpus)
df <- as.matrix(df) # convert TermDocumentMatrix object to a matrix object
df <- data.frame(df) # convert matrix to a data frame

names(df)[1:length(jobs.corpus)] <- paste("job", 1:length(jobs.corpus), sep="")



n_jobs <- ncol(df)

# df[, 1:n_jobs] <- sapply(df[, 1:n_jobs], function(x) ifelse(x > 0, 1, 0))
# df$percentage <- (rowSums(df)/n_jobs)*100
# df <- df[order(-df$percentage),]
df[, 1:n_jobs] <- sapply(df[, 1:n_jobs], function(x) ifelse(x > 0, 1, 0))
df$percentage <- (rowSums(df)/n_jobs)*100
remove(n_jobs)

df <- df[order(-df$percentage),]
df$word <- rownames(df)

# df <- subset(df, select=c("word", "percentage"))




tools <- c("aws","excel","java","nosql","oracle",
           "python", "sql","sqlserver", "microsoft_word", "tableau", "powerbi")

operations <- c("crane", "cat", "operator", "platform", 
                "panel_installation", "repair", "maintenance", "lift", 
                "forklift", "driver", "truck")

education <- c("bachelor", "master", "mba", "high_school", 
               "ged", "vocational", "trade_school", "doctorate", "phd")

study_areas <- c("business", "mba", "engineering", "electrical_engineering", "civil_engineering", 
                 "environmental_engineering", "mechanical_engineering", "economics", "sociology", 
                 "political_science", "social_science", 
                 "statistics", "computer_science", "data_science", "law_degree", "finance")

other <- c("communication_skills", "data_analysis", "teamwork", "administration", 
           "program_management", "project_management", "marketing", "client_relations", 
           "legal_services", "information_technology",
           "scalable","supervision", "vision", "contracts", "audit", "procurement", "strategy",
           "program_evaluation", "economic_evaluation", "sales")

certificates <- c("ec0586", "ec0530", "ec0473", "ec0325", "ec0412", 
                  "ec0413", "ec0414", "ec0416", "ec0431", "ipmvp", "iadc",
                  "iwcf", "stcw", "nebosh", "lnglp", "sqasvq", "iso_45001",
                  "iso_14001", "iso_9001", "iso_50001", "iso_26001", "cmvp")

electromobility <- c("electric_vehicle",
                     "battery_reuse_recycling",
                     "vehicle_grid_connection",
                     "charging_center",
                     "fleeting_operation", 
                     "ancillary_grid_service",
                     "electric_vehicle_charging_network_management",
                     "battery_management_system",
                     "electric_vehicle_training",
                     "battery_electric_vehicle",
                     "hybrid_electric_vehicle",
                     "plugin_hybrid_electric_vehicle",
                     "fuel_cell_electric_vehicle",
                     "private_electric_car",
                     "electric_taxi",
                     "electric_bus",
                     "electric_truck",
                     "electric_light_commercial_vehicle",
                     "electric_scooter",
                     "electric_bicycle",
                     "lithium_batter",
                     "electric_vehicle_manufacturing")



df_sub <- subset(df, word %in% tools | word %in% operations | word %in% education |
               word %in% study_areas | word %in% certificates | word %in% other| word %in% electromobility)

remove(tools, techniques, other, certificates, study_areas, education, electromobility)


df$word[df$word=="aws"] <- "amazon web services"


df$word <- gsub("_", " ", df$word)

df_inverse <- 
  df_sub %>% 
  tidyr::pivot_longer(cols = contains("job"), names_to = "job_id") %>%
  dplyr::mutate(job_id = str_remove(job_id, "job")) %>%
  dplyr::left_join(., scraped_jobs_en_pred %>% 
                     dplyr::mutate(job_id = as.character(row_number())), by = "job_id")



all_wordpct <- 
  df_inverse %>%
  dplyr::group_by(word) %>%
  dplyr::summarize(all_percent = (sum(value)/length(unique(df_inverse$job_id))) * 100)


reneg_wordpct <- 
  df_inverse %>%
  dplyr::mutate(ren_eng = ifelse(renewables_environment == 1|renewables_environment_pred == 1, 1, 0)) %>%
  dplyr::group_by(word, ren_eng) %>%
  dplyr::summarize(ren_eg_percent = (sum(value)/length(unique(df_inverse$job_id))) * 100) %>%
  dplyr::filter(ren_eng == 1)


og_wordpct <- 
  df_inverse %>%
  dplyr::mutate(og = ifelse(oil_energy == 1|oil_energy_pred == 1, 1, 0)) %>%
  dplyr::group_by(word, og) %>%
  dplyr::summarize(og_percent = (sum(value)/length(unique(df_inverse$job_id))) * 100) %>%
  dplyr::filter(og == 1)

wordpct <- 
  left_join(all_wordpct, reneg_wordpct) %>%
  left_join(., og_wordpct)

ren_eg_subsector <- 
  df_inverse %>%
  dplyr::mutate(tot_subsector = case_when(subsector_wind == 1 ~ "Wind",
                                          subsector_solar == 1 ~ "Solar",
                                          subsector_energy_efficiency == 1 ~ "Energy Efficiency",
                                          subsector_electromobility == 1 ~ "Electromobility",
                                          subsector_biomass == 1 ~ "Biomass",
                                          subsector_renewables_crosscutting == 1 ~ "Crosscutting")) %>%
  dplyr::filter(!is.na(tot_subsector)) %>%
  dplyr::group_by(word, tot_subsector) %>%
  dplyr::summarize(percent = (sum(value)/length(unique(job_id))) * 100) %>%
  dplyr::mutate(sector_type = "Renewables")


og_subsector <- 
  df_inverse %>%
  dplyr::mutate(tot_subsector = case_when(subsector_upstream == 1 ~ "Upstream",
                                          subsector_downstream == 1 ~ "Downstream",
                                          subsector_midstream == 1 ~ "Midstream",
                                          subsector_oil_gas_crosscutting == 1 ~ "Crosscutting")) %>%
  dplyr::filter(!is.na(tot_subsector)) %>%
  dplyr::group_by(word, tot_subsector) %>%
  dplyr::summarize(percent = (sum(value)/length(unique(job_id))) * 100) %>%
  dplyr::mutate(sector_type = "Oil & Gas")

subsector_word_pct <- 
  full_join(og_subsector, ren_eg_subsector) #

save(final_sj, file = 'final_sj.RData')
save(final_sj_en, file = 'final_sj_en.RData')
save(final_sj_es, file = 'final_sj_es.RData')
save(subsector_word_pct, file = "subsector_word_pct.RData")
save(wordpct, file = "wordpct.RData")


write.csv(final_sj_en, "scraped_jobs_en.csv")
write.csv(final_sj_es, "scraped_jobs_es.csv")

write.csv(final_sj, file = 'final_sj.csv')
write.csv(final_sj_en, file = 'final_sj_en.csv')
write.csv(final_sj_es, file = 'final_sj_es.csv')



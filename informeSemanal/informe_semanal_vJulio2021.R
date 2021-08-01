## ---------------------------
##
## File name: informe_semanal_vJulio2021.R
##
## Purpose of file: Obtain summaries and statistics about weekly procurement data from PLACSP.
##                    Draft version, sorry for dirty code
##                    (OpenPLACSP was used as preprocesing tool in order to export from XML to xlsx)
##
## Author: Pedro Del Pozo Jiménez
##
## Date Created: 2021-07-25
##
## Email: pedrodpj92@gmail.com
##
## This file is part of DatosAbiertos_OpenPLACSP.
## 
## DatosAbiertos_OpenPLACSP is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## DatosAbiertos_OpenPLACSP is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.
##
## ---------------------------
##
## Notes:
##   Sorry for Spanish comments, only for clarify myself. 
##    If you are interested in, contact me: pedrodpj92@gmail.com
##  - Visit https://contrataciondelsectorpublico.gob.es/wps/portal/DatosAbiertos for more info.
##
## ---------------------------


library("readxl")


dt_perfil_licitaciones <- read_xlsx(path = "./informeSemanal/dataInput/datos_sind_643.xlsx",
                                    sheet = 2)

dt_perfil_resultados <- read_xlsx(path = "./informeSemanal/dataInput/datos_sind_643.xlsx",
                                  sheet = 3)
colnames(dt_perfil_licitaciones) <- gsub(" ","_",colnames(dt_perfil_licitaciones))
colnames(dt_perfil_resultados) <- gsub(" ","_",colnames(dt_perfil_resultados))



#######Parte 1) Contratos Formalizados#######

# colnames(dt_perfil_licitaciones)
df_p_lic <- dt_perfil_licitaciones[,-c(17,18,19,23,24,25,26)]

# colnames(dt_perfil_resultados)
df_p_res <- dt_perfil_resultados[,-c(2,3,4,7,8,19,21)]


df_contratos <- merge(x = df_p_lic, y = df_p_res, by = "Identificador")

#descartamos los establecimientos de acuerdo marco y sistema dinámico de adquisición, no suelen añadir correctamente los datos para las estadísticas
df_contratos <- df_contratos[!(df_contratos$Sistema_de_contratación %in% c("Establecimiento del Acuerdo Marco",
                                                                           "Establecimiento del Sistema Dinámico de Adquisición")),]

df_contratos <- df_contratos[!is.na(df_contratos$`Fecha_del_acuerdo_licitación/lote`),]
df_contratos <-df_contratos[df_contratos$`Fecha_del_acuerdo_licitación/lote`>=as.POSIXct("2021-07-01"),]

df_contratos <- df_contratos[!is.na(df_contratos$`Importe_adjudicación_sin_impuestos_licitación/lote`),]

# número de contratos
print(nrow(df_contratos))


#meter dentro de una funcion en el futuro, código repetido más abajo...
# desglose por tipo
dt_pie1 <- df_contratos[,c("Identificador","Tipo_de_contrato","Importe_adjudicación_sin_impuestos_licitación/lote")]
colnames(dt_pie1) <- c("id","tipo","importe_adj_sin_impuestos")
agg_sum_pie1 <- aggregate(importe_adj_sin_impuestos ~ tipo, dt_pie1, sum)
agg_count_pie1 <- aggregate(importe_adj_sin_impuestos ~ tipo, dt_pie1, length)
colnames(agg_sum_pie1)[2] <- "sum"
colnames(agg_count_pie1)[2] <- "count"
agg_pie1 <- cbind(agg_sum_pie1, count = agg_count_pie1[,2])
agg_pie1$sum_ratio <- 100*(agg_pie1$sum/sum(agg_pie1$sum))
agg_pie1$count_ratio <- 100*(agg_pie1$count/sum(agg_pie1$count))

# write.table(x = agg_pie1,file = "./informeSemanal/outputs_varios/resumen_tiposContratos.csv",
#             sep = ";",row.names = FALSE,col.names = TRUE,dec = ",")



#sobre los CPV
df_contratos$CPV_licLote_Agrupado1 <- substr(df_contratos$`CPV_licitación/lote`, start = 1, stop = 2)
conteo_CPV <- as.data.frame(table(as.factor(df_contratos$CPV_licLote_Agrupado1)))

dt_CPV <- read.csv(file = "./informeSemanal/dataInput/mapeo_CPV.csv",
                   sep = ";",colClasses = c("character",
                                            "character",
                                            "character"))

conteo_CPV$Var1_aumentado <- as.character(conteo_CPV$Var1)
conteo_CPV$Var1_aumentado <- paste0(conteo_CPV$Var1_aumentado,"000000")
dt_CPV$CPV_division <- substring(dt_CPV$CPV, first = 1, last = 8)
colnames(conteo_CPV)[3] <- c("CPV_division")

conteo_CPV <- merge(x = conteo_CPV, y = dt_CPV, by = "CPV_division")


#importe adjudicación total sin impuestos
print("importe de adjudicación total, sin impuestos: ")
print(sum(df_contratos$`Importe_adjudicación_sin_impuestos_licitación/lote`))
#resumen tipo de tramitación (emergencia / ordinaria / urgente)
print("resumen por tipo de tramitación: ")
print(summary(as.factor(df_contratos$Tramitación)))
#cantidad de OC diferentes
print("Número de Órganos de Contratación distintos implicados: ")
print(length(unique(df_contratos$NIF_OC)))
print(length(unique(df_contratos$`Identificador_Adjudicatario_de_la_licitación/lote`)))
# si NA, se considera no PyME para facilitar calcular cuántas PyMES hay de seguro
df_contratos$`El_adjudicatario_es_o_no_PYME_de_la_licitación/lote`[is.na(df_contratos$`El_adjudicatario_es_o_no_PYME_de_la_licitación/lote`)] <- FALSE

df_pymes <- df_contratos[,c("Identificador_Adjudicatario_de_la_licitación/lote",
                            "El_adjudicatario_es_o_no_PYME_de_la_licitación/lote")]
colnames(df_pymes) <- c("NIF","esPyME")
df_pymes <- df_pymes[df_pymes$esPyME,]
agg_esPyME <- aggregate(esPyME ~ NIF, df_pymes, FUN=function(x) length(unique(x)))

# % mínimo de PyMES
print("porcentaje mínimo de PyMES: ")
print(nrow(agg_esPyME)/length(unique(df_contratos$`Identificador_Adjudicatario_de_la_licitación/lote`)))




#######Parte 2) Licitaciones Abiertas y Desiertas#######
df_licitaciones <- dt_perfil_licitaciones[,-c(17,18,19,23,24,25,26)]
df_licitaciones <- df_licitaciones[df_licitaciones$Estado=="En plazo",]
df_licitaciones <-df_licitaciones[df_licitaciones$Primera_publicación>=as.POSIXct("2021-07-01"),]
# df_licitaciones <- df_licitaciones[!(df_licitaciones$Sistema_de_contratación %in% c("Establecimiento del Acuerdo Marco",
#                                                                                     "Establecimiento del Sistema Dinámico de Adquisición")),]
# desglose por tipo
dt_pie2 <- df_licitaciones[,c("Identificador","Tipo_de_contrato","Presupuesto_base_sin_impuestos")]
colnames(dt_pie2) <- c("id","tipo","presupuesto_base_sin_impuestos")
agg_sum_pie2 <- aggregate(presupuesto_base_sin_impuestos ~ tipo, dt_pie2, sum)
agg_count_pie2 <- aggregate(presupuesto_base_sin_impuestos ~ tipo, dt_pie2, length)
colnames(agg_sum_pie2)[2] <- "sum"
colnames(agg_count_pie2)[2] <- "count"
agg_pie2 <- cbind(agg_sum_pie2, count = agg_count_pie2[,2])
agg_pie2$sum_ratio <- 100*(agg_pie2$sum/sum(agg_pie2$sum))
agg_pie2$count_ratio <- 100*(agg_pie2$count/sum(agg_pie2$count))

# write.table(x = agg_pie2,file = "./informeSemanal/outputs_varios/resumen_nuevasLicitaciones.csv",
#             sep = ";",row.names = FALSE,col.names = TRUE,dec = ",")

# df_licitaciones <- df_licitaciones[!is.na(df_licitaciones$Presupuesto_base_sin_impuestos),]
print(nrow(df_licitaciones))
print(sum(df_licitaciones$Presupuesto_base_sin_impuestos))

print(summary(as.factor(dt_perfil_resultados$`Resultado_licitación/lote`)))

df_desiertas <- dt_perfil_resultados[dt_perfil_resultados$`Resultado_licitación/lote`=="Desierto",]
# df_p_lic <- dt_perfil_licitaciones[,-c(17,18,19,23,24,25,26)]
df_desiertas <- df_desiertas[,-c(2,3,4,7,8,19,21)]
df_desiertas <- merge(x = df_p_lic, y = df_desiertas, by = "Identificador")

print(nrow(df_desiertas[df_desiertas$`Número_de_ofertas_recibidas_por_licitación/lote`==0,]))


# summary(as.factor(df_desiertas$CPV))
df_desiertas$CPV_licLote_Agrupado1 <- substr(df_desiertas$CPV, start = 1, stop = 2)
conteo_desierto_CPV <- as.data.frame(table(as.factor(df_desiertas$CPV_licLote_Agrupado1)))

conteo_desierto_CPV$Var1_aumentado <- as.character(conteo_desierto_CPV$Var1)
conteo_desierto_CPV$Var1_aumentado <- paste0(conteo_desierto_CPV$Var1_aumentado,"000000")
# dt_CPV$CPV_division <- substring(dt_CPV$CPV, first = 1, last = 8)
colnames(conteo_desierto_CPV)[3] <- c("CPV_division")

conteo_desierto_CPV <- merge(x = conteo_desierto_CPV, y = dt_CPV, by = "CPV_division")







## ---------------------------
##
## File name: preparareData_PLACSP.R
##
## Purpose of file: Prepare common data used as introduction in order to perform 
##                     a couple of plot examples using OpenPLACSP output info.
##
## Author: Pedro Del Pozo Jiménez
##
## Date Created: 2021-06-05
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

rm(list=ls())

# install.packages("readxl")
# install.packages("plotly")
# install.packages("data.table")
# install.packages("dplyr")
library("readxl")
library("plotly")
library("data.table")
library("dplyr")

dt_perfil_licitaciones <- read_xlsx(path = "./data_input/datos_enero_mayo_2021.xlsx",
                                    sheet = 2)

dt_perfil_resultados <- read_xlsx(path = "./data_input/datos_enero_mayo_2021.xlsx",
                                  sheet = 3)
colnames(dt_perfil_licitaciones) <- gsub(" ","_",colnames(dt_perfil_licitaciones))
colnames(dt_perfil_resultados) <- gsub(" ","_",colnames(dt_perfil_resultados))

# revisión manual de licitaciones segun ID, algunos ejemplos
# dt_perfil_resultados[dt_perfil_resultados$Identificador=="3728298",2]
# dt_perfil_resultados[dt_perfil_resultados$Identificador=="2476981",2]
# dt_perfil_resultados[dt_perfil_resultados$Identificador=="6231566",2]
# dt_perfil_resultados[dt_perfil_resultados$Identificador=="7019841",2]
# dt_perfil_resultados[dt_perfil_resultados$Identificador=="6091943",2]
# dt_perfil_resultados[dt_perfil_resultados$Identificador=="7627054",2]

df_p_lic <- dt_perfil_licitaciones[,c("Identificador","Tipo_de_contrato",
                                      "Sistema_de_contratación",
                                      "Tramitación", "Órgano_de_Contratación",
                                      "NIF_OC","Tipo_de_Administración")]
# df_p_lic <- dt_perfil_licitaciones[,c("Identificador","Tipo_de_contrato")]
df_p_res <- dt_perfil_resultados[,c("Identificador","Número_del_contrato_licitación/lote",
                                    "Fecha_del_acuerdo_licitación/lote",
                                    "Resultado_licitación/lote",
                                    "Importe_adjudicación_sin_impuestos_licitación/lote",
                                    "Importe_adjudicación_con_impuestos_licitación/lote",
                                    "Identificador_Adjudicatario_de_la_licitación/lote",
                                    "Adjudicatario_licitación/lote")]

df_p_res <- df_p_res[!is.na(df_p_res$`Número_del_contrato_licitación/lote`),]
df_p_res <- df_p_res[df_p_res$`Resultado_licitación/lote`=="Adjudicado",]

#unir datos de la licitación con sus resultados
df_pie_res <- merge(x = df_p_lic, y = df_p_res, by = "Identificador")

#descartamos los establecimientos de acuerdo marco y sistema dinámico de adquisición, no suelen añadir correctamente los datos para las estadísticas
df_pie_res <- df_pie_res[!(df_pie_res$Sistema_de_contratación %in% c("Establecimiento del Acuerdo Marco",
                                                                     "Establecimiento del Sistema Dinámico de Adquisición")),]
df_pie_res$Sistema_de_contratación <- NULL

#solo nos interesan aquellos contratos que estén dentro del 2021
df_pie_res <- df_pie_res[!is.na(df_pie_res$`Fecha_del_acuerdo_licitación/lote`),]
df_pie_res <-df_pie_res[df_pie_res$`Fecha_del_acuerdo_licitación/lote`>=as.POSIXct("2021-01-01"),]

#algunos datos estadísticos, quizás sea conveniente añadir prints si se ejecuta el código directamente
#n_contratos
print("Número de contratos de enero a mayo de 2021 (incluídos): ")
print(nrow(df_pie_res))
#importe adjudicación total sin impuestos
print("importe de adjudicación total, sin impuestos: ")
print(sum(df_pie_res$`Importe_adjudicación_sin_impuestos_licitación/lote`))
#resumen tipo de tramitación (emergencia / ordinaria / urgente)
print("resumen por tipo de tramitación: ")
print(summary(as.factor(df_pie_res$Tramitación)))
#cantidad de OC diferentes
print("Número de Órganos de Contratación distintos implicados: ")
print(length(unique(df_pie_res$NIF_OC)))
#distribución por tipos de OC
print("Distribución de Órganos de Contratación según su tipo: ")
df_pie_res$conteo <- 1
agg_OC_tipo <- aggregate(conteo ~ NIF_OC + Tipo_de_Administración, df_pie_res, sum)
print(summary(as.factor(agg_OC_tipo$Tipo_de_Administración)))



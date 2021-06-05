## ---------------------------
##
## File name: barChartAndBubbleExamples_PLACSP.R
##
## Purpose of file: Some examples using bar and bubble charts. Types of public procurement in public sector.
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

# watch out with current directory execution, see getwd()
source(file = "./introductionExample/preparareData_PLACSP.R",encoding = "UTF-8")



##Diagrama de barras, top 10 de adjudicatario
dt_bar1 <- df_pie_res[,c("Adjudicatario_licitación/lote","Identificador_Adjudicatario_de_la_licitación/lote",
                         "Órgano_de_Contratación","NIF_OC",
                         "Importe_adjudicación_sin_impuestos_licitación/lote","conteo")]
colnames(dt_bar1) <- c("adjudicatario","id_adjudicatario",
                       "oc","id_oc",
                       "importe_adj","conteo")
agg_sum_bar1 <- aggregate(importe_adj ~ adjudicatario + id_adjudicatario, dt_bar1, sum)
agg_count_bar1 <- aggregate(importe_adj ~ adjudicatario + id_adjudicatario, dt_bar1, length)
#cuantos OC por adjudicatario, gracias stackoverflow --> https://stackoverflow.com/questions/5891927/counting-unique-factors-in-r
agg_count_OC_bar1 <- aggregate(id_oc ~ adjudicatario + id_adjudicatario, data=dt_bar1, FUN=function(x) length(unique(x)))

agg_bar1 <- cbind(agg_sum_bar1,agg_count_bar1[,3],agg_count_OC_bar1[,3])
colnames(agg_bar1) <- c("adjudicatario","id_adjudicatario","importe_adj","n_contratos","n_OC")

#nombres muy inconsistentes, y algunos NIF, para esta vesión que solo usaremos los 10 primeros, 
#se podría limpiar fácilmente por openrefien?
#solución temporal: nos quedamos con el primer nombre que encontremos y nos guiamos por el NIF
#habrá algún error aun así, pero serán mínimos

#para el diagrama de burbujas, juntar por NIF, 
agg2_adjudicatario_bar1 <- aggregate(adjudicatario ~ id_adjudicatario, data=agg_bar1,FUN=function(x) head(x,1))
agg2_sum_bar1 <- aggregate(importe_adj ~  id_adjudicatario, dt_bar1, sum)
agg2_count_bar1 <- aggregate(importe_adj ~ id_adjudicatario, dt_bar1, length)
agg2_count_OC_bar1 <- aggregate(id_oc ~ id_adjudicatario, data=dt_bar1, FUN=function(x) length(unique(x)))
agg2_bar1 <- cbind(agg2_adjudicatario_bar1,agg2_sum_bar1[,2],
                   agg2_count_bar1[,2],agg2_count_OC_bar1[,2])
colnames(agg2_bar1) <- c("id_adjudicatario","adjudicatario","importe_adj","n_contratos","n_OC")

#para descartar casos con importe == 0, o bien son errores de entrada o bien algunos casos que no tienen la obligación de incluir el valor
# para quitar casos pequeños que pueden meter ruido, se tiene en cuenta además solo aquellos importes > 5 mil euros
agg2_bar1_limpio <- agg2_bar1[agg2_bar1$importe_adj>5000,]

agg2_bar1_top10 <- head(agg2_bar1_limpio[order(agg2_bar1_limpio$importe_adj, decreasing= T),], n = 10)
#ideas para el barchart --> https://plotly.com/r/bar-charts/
fig_bar1 <- agg2_bar1_top10 %>% plot_ly()
fig_bar1 <- fig_bar1 %>% add_trace(x = ~adjudicatario, y = ~n_contratos, type = 'bar',
                                   text = ~n_contratos, textposition = 'auto', name="número de contratos",
                                   insidetextfont = list(color = '#FFFFFF'),
                                   marker = list(color = 'rgb(120,170,90)',
                                                 line = list(color = 'rgb(10,80,0)', width = 1.5)))
fig_bar1 <- fig_bar1 %>% add_trace(x = ~adjudicatario, y = ~n_OC, type = 'bar',
                                   text = ~n_OC, textposition = 'auto', name="número de OC",
                                   insidetextfont = list(color = '#000000'),
                                   marker = list(color = 'rgb(150,240,60)',
                                                 line = list(color = 'rgb(10,80,0)', width = 1.5)))
fig_bar1 <- fig_bar1 %>% layout(title = "Número de contratos y Órganos de Contratación\nasociados al Top 10 de Adjudicatarios\nlicitaciones enero-mayo 2021",
                                barmode = 'group',
                                xaxis = list(title = ""),
                                yaxis = list(title = ""),
                                margin = list(b=120))
fig_bar1



fig_bar2 <- plot_ly(agg2_bar1_top10, x = ~adjudicatario, y = ~importe_adj, type = 'bar',
                    # text = ~paste(format(importe_adj, nsmall=2, big.mark=".", decimal.mark=","), '€'), textposition = 'outside',
                    marker = list(color = 'rgb(120,170,90)',
                                  line = list(color = 'rgb(10,80,0)', width = 1.5)))
fig_bar2 <- fig_bar2 %>% layout(title = "Importe total adjudicado al Top 10 de Adjudicatarios\nlicitaciones enero-mayo 2021",
                                xaxis = list(title = ""),
                                yaxis = list(title = ""),
                                margin = list(b=120,
                                              r=120))

fig_bar2


fig_bubble <- plot_ly(agg2_bar1_limpio, x = ~n_OC, y = ~n_contratos,# text = ~importe_adj,
                      type = 'scatter', mode = 'markers',
                      size = ~importe_adj,
                      sizes = c(10, 4000),
                      # marker = list(size = ~(round(importe_adj/1000000)+1), opacity = 0.3))
                      marker = list(opacity = 0.3,
                                    color = 'rgb(80,170,20)',
                                    line = list(width = 1, color = 'rgb(20,20,20)',
                                                opacity = 0.4)),
                      text = ~paste0("número contratos: ", n_contratos,"\n",
                                     "número Org. Contratación: ", n_OC,"\n",
                                     "importe de adjudicación: ", 
                                     format(importe_adj, big.mark=".", decimal.mark=","), "\n",
                                     "adjudicatario: ", adjudicatario,"\n",
                                     "NIF: ",id_adjudicatario))
fig_bubble <- fig_bubble %>% layout(title = 'info. adjudicatarios\nlicitaciones enero-junio 2021',
                                    xaxis = list(showgrid = FALSE,
                                                 title = "Número de Órganos de Contratación"),
                                    yaxis = list(showgrid = FALSE,
                                                 title = "Número de contratos adjudicados o formalizados",
                                                 margin = list(b=100)))

fig_bubble

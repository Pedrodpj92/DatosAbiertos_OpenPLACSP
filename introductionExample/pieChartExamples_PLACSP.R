## ---------------------------
##
## File name: pieChartExamples_OpenPLACSP.R
##
## Purpose of file: Some examples using pie charts. Types of public procurement in public sector
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

#preparar data frame para el pie chart
#cambiar si se desea Importe_adjudicación_sin_impuestos_licitación/lote por Importe_adjudicación_con_impuestos_licitación/lote
dt_pie1 <- df_pie_res[,c("Identificador","Tipo_de_contrato","Importe_adjudicación_sin_impuestos_licitación/lote")]
colnames(dt_pie1) <- c("id","tipo","importe_adj_sin_impuestos")
agg_sum_pie1 <- aggregate(importe_adj_sin_impuestos ~ tipo, dt_pie1, sum)
agg_count_pie1 <- aggregate(importe_adj_sin_impuestos ~ tipo, dt_pie1, length)
colnames(agg_sum_pie1)[2] <- "sum"
colnames(agg_count_pie1)[2] <- "count"
agg_pie1 <- cbind(agg_sum_pie1, count = agg_count_pie1[,2])
agg_pie1$sum_ratio <- 100*(agg_pie1$sum/sum(agg_pie1$sum))
agg_pie1$count_ratio <- 100*(agg_pie1$count/sum(agg_pie1$count))
#para agrupar las categorías menores en "otros", las n dependen de los datos, cuidado
#sería más elegante revisar que su % sea menor de X, pero más adelante quizás lo cambie
agg_pie1_top <- head(agg_pie1[order(agg_pie1$sum, decreasing= T),], n = 3)
agg_pie1_bottom <- tail(agg_pie1[order(agg_pie1$sum, decreasing= T),], n = 7)
agg_pie1_top <- rbind(agg_pie1_top,
                      list("Otros",
                           sum(agg_pie1_bottom$sum),
                           sum(agg_pie1_bottom$count),
                           sum(agg_pie1_bottom$sum_ratio),
                           sum(agg_pie1_bottom$count_ratio)))

# idea de tutorial: https://plotly.com/r/pie-charts/
colors1 <- c('rgb(170,0,20)', 'rgb(110,170,0)', 'rgb(0,170,130)', 'rgb(40,0,170)')

fig_sum_top <- plot_ly(agg_pie1_top, labels = ~paste0(tipo, ':\n', format(sum, nsmall=2, big.mark=".", decimal.mark=","),  ' euros\n',
                                                      format(sum_ratio, digits=2, nsmall=2, big.mark=".", decimal.mark=","),'%'),
                       values = ~sum_ratio, type = 'pie',
                       textposition = 'inside',
                       textinfo = 'label',
                       insidetextfont = list(color = '#FFFFFF'),
                       hoverinfo = 'text',
                       text = ~paste(tipo, '-', format(sum, nsmall=2, big.mark=".", decimal.mark=","), 'euros'),
                       marker = list(colors = colors1,
                                     line = list(color = '#FFFFFF', width = 1)),
                       showlegend = TRUE)
fig_sum_top <- fig_sum_top %>% layout(title = 'importe de adjudicación sin impuestos licitaciones enero a mayo 2021',
                                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig_sum_top



fig_count <- plot_ly(agg_pie1_top, labels = ~paste0(tipo,": ",format(count, nsmall=2, big.mark=".", decimal.mark=","), "\n",
                                                    format(count_ratio, digits=2, nsmall=2, big.mark=".", decimal.mark=","),'%'),
                     values = ~count_ratio, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste(tipo,'-', format(count, nsmall=2, big.mark=".", decimal.mark=","), 'licitaciones'),
                     marker = list(colors = colors1,
                                   line = list(color = '#FFFFFF', width = 1)),
                     showlegend = TRUE)
fig_count <- fig_count %>% layout(title = 'cantidad de contratos por tipo licitaciones enero a mayo 2021',
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig_count










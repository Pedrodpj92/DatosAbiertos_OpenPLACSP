# DatosAbiertos_OpenPLACSP

Este es un proyecto en desarrollo. El contenido puede verse actualizado con el paso del tiempo.

La idea surge para reaprovechar el potencial que arrojan los Datos Abiertos en el ámbito de la Contratación en el Sector Público.

Por ahora, se muestran algunas gráficas de ejemplo realizadas con [R](https://cran.r-project.org/).

## Origen de los datos
Estos gráficos se han obtenido a partir de los Datos Abiertos disponibles en la Plataforma de Contratación del Sector Público ([PLACSP](https://contrataciondelsectorpublico.gob.es/wps/portal/DatosAbiertos)).
El período mostrado abarca de enero a mayo de 2021, ambos incluídos. El repositorio original de los conjuntos de datos se encuentra disponible [aquí](https://www.hacienda.gob.es/ca-ES/GobiernoAbierto/Datos%20Abiertos/Paginas/licitaciones_plataforma_contratacion.aspx). Se ha utilizado la herramienta [OpenPLACSP](https://contrataciondelestado.es/datosabiertos/open-placsp-1.0-bin.zip) para facilitar el tratamiento de la información. [Enlace de los datos procesados utilizados](https://github.com/Pedrodpj92/DatosAbiertos_OpenPLACSP/tree/main/data_input). El código está disponible en esta [carpeta](https://github.com/Pedrodpj92/DatosAbiertos_OpenPLACSP/tree/main/introductionExample) del proyecto.


## Plots 
### Relación de tipos de licitaciones:
* [Según importe de adjudicación (sin impuestos)](https://pedrodpj92.github.io/DatosAbiertos_OpenPLACSP/plot_example/pieChart_importeAdjudicacion_enero_mayo_2021.html)
* [Según número de contratos concedidos](https://pedrodpj92.github.io/DatosAbiertos_OpenPLACSP/plot_example/pieChart_cantidadContratos_enero_mayo_2021.html)

### Top 10 de adjudicatarios:
* [Número de contratos y número de Órganismos de Contratación relacionados](https://pedrodpj92.github.io/DatosAbiertos_OpenPLACSP/plot_example/barChart_Top10_empresas1.html)
* [Importe total de adjudicación](https://pedrodpj92.github.io/DatosAbiertos_OpenPLACSP/plot_example/barChart_Top10_empresas2_importe.html)

### Conjunto de adjudicatarios que han recibido contratos públicos:
* [Gráfico de burbujas](https://pedrodpj92.github.io/DatosAbiertos_OpenPLACSP/plot_example/bubbleChart_empresas.html)


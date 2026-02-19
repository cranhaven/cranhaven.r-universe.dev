# UnalR 1.0.1

v1.0.1 fue lanzada el 15/09/2025

## Correcciones (*bug fixes*)
  * Se ajustó la implementación para garantizar compatibilidad con `ggplot2` `v4`, actualizando las pruebas afectadas en `test-Plot_Series.R` según los nuevos requisitos de clase y tipo de objeto. Estos cambios resuelven el problema reportado por el equipo de `ggplot2` ([tidyverse/ggplot2#6498](https://github.com/tidyverse/ggplot2/issues/6498)) y aseguran que el paquete pase las validaciones con la próxima versión mayor de `ggplot2`.

___

# UnalR 1.0.0

v1.0.0 fue lanzada el 03/06/2023

## Cambios
  * En los casos aplicables, se reemplaza el operador de `magrittr` por el "native pipe" de `R` (`%>%` *por* `|>`), incluido en la versión `4.1.0`, reemplazando así la "tubería" la cual es el icono distintivo de `dplyr` y el `tidyverse`.

## Nuevas características (*new features*)
  * Se modifica la función `Agregar()`, ahora permite realizar múltiples agregados simultáneamente, es decir, especificando más de una variable de interés. Internamente realiza agregados individuales y los concatena uno debajo del otro (*por filas*).
  * Para las funciones `Plot.Series()`, `Plot.Barras()` y `Plot.Apiladas()` se agrega el parámetro `estatico`, el cual ahora permite generar el gráfico de manera invariable mediante la librería `ggplot2`. Para lo cual se agregan una serie de parámetros específicos para dicha librería (*dentro de `estilo`, las podrá encontrar cómo* `gg.*`).


## Correcciones (*bug fixes*)
  * En tablas pequeñas, con pocas columnas, se desplazaba el encabezado (*nombre de las columnas*) a la izquierda, esto sin importar el tamaño de la ventana. Se establece el argumento `scrollX` (fijado antes como `TRUE`) en las funciones `Tabla()`, `Tabla.General()` y `Tabla.SaberPro()`.
  * Se solicita la inclusión del argumento `escape = TRUE` para que las diversas funciones de tablas permitan la inclusión de entidades **HTML**, se advierte que al ponerlo como `FALSE` puede haber posibles problemas de seguridad cuando la tabla se representa en aplicaciones web dinámicas.

___

# UnalR 0.1.0 (*minor version*)

v0.1.0 fue lanzada el 28/11/2021

## Cambios
  * Se modificó la selección y el orden de algunos temas de highcharter usados en el argumento `hc.Tema`.
  * Por cuestiones estéticas en `Plot.Mapa()` se redondea el valor de cualquier estadístico a tres cifras decimales.

## Nuevas características (*new features*)
  * Se adicionó las funciones `Agregar()`, `Plot.Boxplot()`, `Plot.Radar()`, `Plot.Treemap()`, `Plot.Drilldown()` y `Plot.Apiladas()`.
  * Se incluyó nuevos argumentos en la función `Plot.Series()` tales como `freqRelativa`, `invertir` y `ylim`.
  * Se incorporó a la función `Plot.Barras()` el argumento `freqRelativa`.
  * Se incorporó a la función `Plot.Torta()` el argumento `label`.
  * Se añade en `Plot.Mapa()` los argumento `estadistico` (*para mostrar distintas estadísticas descriptivas*), `naTo0` y `colNA` además de agregar un nuevo estadístico (*coeficiente de variación* **CV**) en el argumento `estadistico`.
  * Las nuevas funciones incorporan la gramática del tidyverse, la cual permite trabajar los datos como si fueran objetos reales en el espacio de trabajo. Para más información del framework tidy evaluation puede consultar [aquí](https://dplyr.tidyverse.org/articles/programming.html).
  * La escritura de todas las funciones (*incluyendo la documentación y los ejemplos*) cumplen con la guía de estilos del tidyverse, la cual puede consultar [aquí](https://style.tidyverse.org).

## Correcciones (*bug fixes*)
  * Se corrigieron algunos errores ortográficos en la documentación del paquete.
  * Se modificó la forma de calcular el centroide de los polígonos en `Plot.Mapa()`, pues para aquellos municipios cuyo polígono espacial es altamente irregular su centroide caía fuera de éste.
  * Para los municipios homónimos, en `Plot.Mapa()`, se subsanó el problema con estos, pues en la "Lupa" no era posible diferenciarlos y realizar la búsqueda de forma correcta.
  * Se solvento el error en `Plot.Mapa()` presente en el argumento `centroideMapa`, en el cual al especificarlo el mapa sí iniciaba en dicha ubicación pero al momento de cliquear en el botón "Retornar" no se enviaba al centroide especificado.

___

# UnalR 0.0.0.9000 (*development version*)

v0.0.0.9000 fue lanzada el 28/02/2021

  * ¡Primera versión estable de implementación avanzada de `UnalR`!
  * Muchas funciones escritas para hacer uso de métodos y clases. En consecuencia, se han implementado varias funciones y se han documentado muchos argumentos. Consulte la ayuda para obtener más detalles.
  * Funciones añadidas `Tabla()`, `Tabla.SaberPro()`, `Plot.Series()`, `Plot.Torta()`, `Plot.Barras()`, `Plot.Mapa()` y `StaticPlot()`.
  * Se realizaron varios cambios para garantizar la compatibilidad.

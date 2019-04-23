############### 1. Construir tablas ###############

tabla1 <- function(datos, categoria, variable,  mensaje, titulo){
  Tot <- datos %>% filter(Variable==categoria) %>% group_by(YEAR, SEMESTRE) %>% summarise(Tot=sum(Total))
  tba0 <- datos %>% filter(Variable==categoria) %>% spread(key = Clase, value = Total) %>% select(-Variable) %>% left_join(Tot)
  n <- datos %>% filter(Variable==categoria) %>% group_by(Clase) %>% distinct(Clase)
  categorias <- colnames(tba0 %>% ungroup() %>% select(-YEAR, -SEMESTRE, -Tot))
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Año'),
        th(rowspan = 2, 'Periodo'),
        th(colspan = n_groups(n), variable),
        th(rowspan = 2, 'Total')
      ),
      tr(
        lapply(categorias, th)
      )
    )
  ))
  tba <- datatable(
    tba0, 
    fillContainer = paste0("Tabla: ",mensaje),
    filter = 'bottom',
    extensions = c('Buttons', 'Responsive', 'KeyTable'), width="100%", 
    container = sketch,
    rownames = FALSE,
    options = list(columnDefs = list(list(className = 'dt-center', targets = 0:(n_groups(n)+2))),
                   order = list(list(0, 'desc'),list(1, 'desc')),
                   searchHighlight = TRUE,
                   pageLength = 8,
                   keys = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(list(extend='copy',text='Copiar'), list(extend = 'csv', text='CSV',filename = 'FormatCSV',title=titulo), list(extend = 'excel', text='Excel',filename = 'FormatExcel',title=titulo),list( extend = 'pdf', pageSize = 'A4', filename = 'pdf', message=mensaje,title=titulo), list(extend='print',text='Imprimir',pageSize = 'A4', message=mensaje,title=titulo)),
                   language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.16/i18n/Spanish.json'))) 
  return(tba)
}



tablaall <- function(datos, categoria,  mensaje, titulo){
  Tot <- datos %>% filter(Variable==categoria) %>% group_by(YEAR, SEMESTRE) %>% summarise(Tot=sum(Total))
  tba0 <- datos %>% filter(Variable==categoria) %>% spread(key = Clase, value = Total) %>% select(-Variable) 
  n <- datos %>% filter(Variable==categoria) %>% group_by(Clase) %>% distinct(Clase)
  categorias <- colnames(tba0 %>% ungroup() %>% select(-YEAR, -SEMESTRE))
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Año'),
        th(rowspan = 2, 'Periodo')      ),
      tr(
        lapply(categorias, th)
      )
    )
  ))
  tba <- datatable(
    tba0, 
    fillContainer = paste0("Tabla: ",mensaje),
    filter = 'bottom',
    extensions = c('Buttons', 'Responsive', 'KeyTable'), width="100%", 
    container = sketch,
    rownames = FALSE,
    options = list(columnDefs = list(list(className = 'dt-center', targets = 0:(0))),
                   order = list(list(0, 'desc'),list(1, 'desc')),
                   searchHighlight = TRUE,
                   pageLength = 8,
                   keys = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(list(extend='copy',text='Copiar'), list(extend = 'csv', text='CSV',filename = 'FormatCSV',title=titulo), list(extend = 'excel', text='Excel',filename = 'Excel',title=titulo),list( extend = 'pdf', pageSize = 'A4', filename = 'pdf', message=mensaje,title=titulo), list(extend='print',text='Imprimir',pageSize = 'A4', message=mensaje,title=titulo)),
                   language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.16/i18n/Spanish.json'))) 
  return(tba)
}


tablaall2 <- function(datos, categoria,  mensaje, titulo){
  Tot <- datos %>% filter(Variable==categoria) %>% group_by(DEP_RES, CIU_RES) %>% summarise(Tot=sum(Total))
  tba0 <- datos %>% filter(Variable==categoria) %>% spread(key = Clase, value = Total) %>% select(-Variable) 
  n <- datos %>% filter(Variable==categoria) %>% group_by(Clase) %>% distinct(Clase)
  categorias <- colnames(tba0 %>% ungroup() %>% select(-DEP_RES, -CIU_RES))
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Departamento'),
        th(rowspan = 2, 'Municipio')      ),
      tr(
        lapply(categorias, th)
      )
    )
  ))
  tba <- datatable(
    tba0, 
    fillContainer = paste0("Tabla: ",mensaje),
    filter = 'bottom',
    extensions = c('Buttons', 'Responsive', 'KeyTable'), width="100%", 
    container = sketch,
    rownames = FALSE,
    options = list(columnDefs = list(list(className = 'dt-center', targets = 0:(0))),
                   order = list(list(0, 'desc'),list(1, 'desc')),
                   searchHighlight = TRUE,
                   pageLength = 8,
                   keys = TRUE,
                   dom = 'Bfrtip',
                   buttons = list(list(extend='copy',text='Copiar'), list(extend = 'csv', text='CSV',filename = 'FormatCSV',title=titulo), list(extend = 'excel', text='Excel',filename = 'Excel',title=titulo),list( extend = 'pdf', pageSize = 'A4', filename = 'pdf', message=mensaje,title=titulo), list(extend='print',text='Imprimir',pageSize = 'A4', message=mensaje,title=titulo)),
                   language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.16/i18n/Spanish.json'))) 
  return(tba)
}
############### 1. Construir tabla 1 variable ###############

tabla <- function(datos, categoria, variable,  mensaje, titulo){
  Tot <- datos %>% filter(Variable==categoria) %>% group_by(YEAR, SEMESTRE) %>% summarise(Tot=sum(Total))
  tba0 <- datos %>% filter(Variable==categoria) %>% spread(key = Clase, value = Total) %>% select(-Variable) %>% left_join(Tot)
  n <- datos %>% filter(Variable==categoria) %>% group_by(Clase) %>% distinct(Clase)
  categorias <- colnames(tba0 %>% ungroup() %>% select(-YEAR, -SEMESTRE, -Tot))
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Año'),
        th(rowspan = 2, 'Periodo'),
        th(colspan = n_groups(n), variable),
        th(rowspan = 2, 'Total')
      ),
      tr(
        lapply(categorias, th)
      )
    )
  ))
  tba1 <- tba0
  tba1$YEAR <- as.factor(tba1$YEAR)
  tba1$SEMESTRE <- as.factor(tba1$SEMESTRE)
  tba <- datatable(
    tba1, 
    fillContainer = paste0("Tabla: ",mensaje),
    filter = 'top',
    extensions = c('Buttons', 'Responsive', 'KeyTable'), width="100%", 
    container = sketch,
    rownames = FALSE,
    options = list(columnDefs = list(list(className = 'dt-center', targets = 0:(n_groups(n)+2))),
                   order = list(list(0, 'desc'),list(1, 'desc')),
                   searchHighlight = TRUE,
                   pageLength = 8,
                   keys = TRUE,
                   dom = 'Bfrtip',
                   language = list(
                     processing = "Procesando...",
                     lengthMenu =     "Mostrar _MENU_ registros",
                     zeroRecords =    "No se encontraron resultados",
                     emptyTable =     "Ningún dato disponible en esta tabla",
                     info =           "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                     infoEmpty =      "Mostrando registros del 0 al 0 de un total de 0 registros",
                     infoFiltered =   "(filtrado de un total de _MAX_ registros)",
                     infoPostFix =    "",
                     search =         "Buscar:",
                     url =            "",
                     infoThousands =  ",",
                     loadingRecords = "Cargando...",
                     paginate = list(
                       first =    "Primero",
                       last =     "Último",
                       `next` =     "Siguiente",
                       previous = "Anterior"
                     ),
                     aria = list(
                       sortAscending =   "Activar para ordenar la columna de manera ascendente",
                       sortDescending =  "Activar para ordenar la columna de manera descendente"
                     )
                   ),
                   buttons = list(list(extend='copy',text='Copiar'), list(extend = 'csv', text='CSV',filename = 'FormatCSV',title=titulo), list(extend = 'excel', text='Excel',filename = 'Excel',title=titulo),list( extend = 'pdf', pageSize = 'A4', filename = 'pdf', message=mensaje,title=titulo), list(extend='print',text='Imprimir',pageSize = 'A4', message=mensaje,title=titulo))
    )) 
  return(tba)
}


############### 1.1. Construir tabla n variables ###############
tabla_n <- function(datos, categorias, variable,  mensaje, titulo){
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = length(categorias), titulo)
      ),
      tr(
        lapply(categorias, th)
      )
    )
  ))
  tba1 <- datos
  tba2 <- data.frame(apply(tba1[,-3],2,as.factor),Total=tba1$Total)
  tba <- datatable(
    tba2, 
    fillContainer = paste0("Tabla: ",mensaje),
    filter = 'top',
    extensions = c('Buttons', 'Responsive', 'KeyTable'),
    width="100%", 
    container = sketch,
    rownames = FALSE,
    options = list(columnDefs = list(list(className = 'dt-center', targets = 0:(length(categorias)-1))),
                   order = list(list(2, 'desc'),list(1, 'desc')),
                   searchHighlight = TRUE,
                   pageLength = 10,
                   keys = TRUE,
                   dom = 'Bfrtip',
                   language = list(
                     processing = "Procesando...",
                     lengthMenu =     "Mostrar _MENU_ registros",
                     zeroRecords =    "No se encontraron resultados",
                     emptyTable =     "Ningún dato disponible en esta tabla",
                     info =           "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                     infoEmpty =      "Mostrando registros del 0 al 0 de un total de 0 registros",
                     infoFiltered =   "(filtrado de un total de _MAX_ registros)",
                     infoPostFix =    "",
                     search =         "Buscar:",
                     url =            "",
                     infoThousands =  ",",
                     loadingRecords = "Cargando...",
                     paginate = list(
                       first =    "Primero",
                       last =     "Último",
                       `next` =     "Siguiente",
                       previous = "Anterior"
                     ),
                     aria = list(
                       sortAscending =   "Activar para ordenar la columna de manera ascendente",
                       sortDescending =  "Activar para ordenar la columna de manera descendente"
                     )
                   ),
                   buttons = list(list(extend='copy',text='Copiar'), list(extend = 'csv', text='CSV',filename = 'FormatCSV',title=titulo), list(extend = 'excel', text='Excel',filename = 'Excel',title=titulo),list( extend = 'pdf', pageSize = 'A4', filename = 'pdf', message=mensaje,title=titulo), list(extend='print',text='Imprimir',pageSize = 'A4', message=mensaje,title=titulo))
    )) 
  return(tba)
}

tabla_n3 <- function(datos, categorias, variable,  mensaje, titulo){
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = length(categorias), titulo)
      ),
      tr(
        lapply(categorias, th)
      )
    )
  ))
  tba1 <- datos
  tba2 <- data.frame(apply(tba1[,-5],2,as.factor),Total=tba1$Total)
  tba <- datatable(
    tba2, 
    fillContainer = paste0("Tabla: ",mensaje),
    filter = 'top',
    extensions = c('Buttons', 'Responsive', 'KeyTable'),
    width="100%", 
    container = sketch,
    rownames = FALSE,
    options = list(columnDefs = list(list(className = 'dt-center', targets = 0:(length(categorias)-1))),
                   order = list(list(2, 'desc'),list(1, 'desc')),
                   searchHighlight = TRUE,
                   pageLength = 10,
                   keys = TRUE,
                   dom = 'Bfrtip',
                   language = list(
                     processing = "Procesando...",
                     lengthMenu =     "Mostrar _MENU_ registros",
                     zeroRecords =    "No se encontraron resultados",
                     emptyTable =     "Ningún dato disponible en esta tabla",
                     info =           "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                     infoEmpty =      "Mostrando registros del 0 al 0 de un total de 0 registros",
                     infoFiltered =   "(filtrado de un total de _MAX_ registros)",
                     infoPostFix =    "",
                     search =         "Buscar:",
                     url =            "",
                     infoThousands =  ",",
                     loadingRecords = "Cargando...",
                     paginate = list(
                       first =    "Primero",
                       last =     "Último",
                       `next` =     "Siguiente",
                       previous = "Anterior"
                     ),
                     aria = list(
                       sortAscending =   "Activar para ordenar la columna de manera ascendente",
                       sortDescending =  "Activar para ordenar la columna de manera descendente"
                     )
                   ),
                   buttons = list(list(extend='copy',text='Copiar'), list(extend = 'csv', text='CSV',filename = 'FormatCSV',title=titulo), list(extend = 'excel', text='Excel',filename = 'Excel',title=titulo),list( extend = 'pdf', pageSize = 'A4', filename = 'pdf', message=mensaje,title=titulo), list(extend='print',text='Imprimir',pageSize = 'A4', message=mensaje,title=titulo))
    )) 
  return(tba)
}

tabla_n2 <- function(datos, categorias, variable,  mensaje, titulo){
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = length(categorias), titulo)
      ),
      tr(
        lapply(categorias, th)
      )
    )
  ))
  tba1 <- datos
  tba2 <- data.frame(apply(tba1[,-4],2,as.factor),Total=tba1$Total)
  tba <- datatable(
    tba2, 
    fillContainer = paste0("Tabla: ",mensaje),
    filter = 'top',
    extensions = c('Buttons', 'Responsive', 'KeyTable'),
    width="100%", 
    container = sketch,
    rownames = FALSE,
    options = list(columnDefs = list(list(className = 'dt-center', targets = 0:(length(categorias)-1))),
                   order = list(list(2, 'desc'),list(1, 'desc')),
                   searchHighlight = TRUE,
                   pageLength = 10,
                   keys = TRUE,
                   dom = 'Bfrtip',
                   language = list(
                     processing = "Procesando...",
                     lengthMenu =     "Mostrar _MENU_ registros",
                     zeroRecords =    "No se encontraron resultados",
                     emptyTable =     "Ningún dato disponible en esta tabla",
                     info =           "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                     infoEmpty =      "Mostrando registros del 0 al 0 de un total de 0 registros",
                     infoFiltered =   "(filtrado de un total de _MAX_ registros)",
                     infoPostFix =    "",
                     search =         "Buscar:",
                     url =            "",
                     infoThousands =  ",",
                     loadingRecords = "Cargando...",
                     paginate = list(
                       first =    "Primero",
                       last =     "Último",
                       `next` =     "Siguiente",
                       previous = "Anterior"
                     ),
                     aria = list(
                       sortAscending =   "Activar para ordenar la columna de manera ascendente",
                       sortDescending =  "Activar para ordenar la columna de manera descendente"
                     )
                   ),
                   buttons = list(list(extend='copy',text='Copiar'), list(extend = 'csv', text='CSV',filename = 'FormatCSV',title=titulo), list(extend = 'excel', text='Excel',filename = 'Excel',title=titulo),list( extend = 'pdf', pageSize = 'A4', filename = 'pdf', message=mensaje,title=titulo), list(extend='print',text='Imprimir',pageSize = 'A4', message=mensaje,title=titulo))
    )) 
  return(tba)
}
############### 2. Construir series: ###############

relativo <- function(x){
  div <- function(m){m*100/sum(m,  na.rm =  TRUE)}
  x1 <- t(apply(x,1,div))
  x2 <- round(x1,0)
  return(x2)
}

series2 <- function(datos,categoria,colores,titulo,eje){
  fecha <- (datos %>% filter(Variable == categoria)%>% 
              spread(key = Clase, value = Total) %>% 
              mutate(Fecha = paste(YEAR,sep = "-")))$Fecha
  datos <- ungroup(datos)
  tba <- datos %>% filter(Variable == categoria) %>%
    mutate(Fecha = paste(YEAR,sep = "-"))
  tba0 <- expand.grid(unique(tba$Fecha),unique(tba$Clase))
  colnames(tba0) <- c("Fecha","Clase")
  tba0 <- tba0 %>% left_join(tba %>% select(-YEAR,-Variable)) %>% arrange(Fecha)
  tba2 <- datos %>% filter(Variable == categoria) %>% 
    spread(key = Clase, value = Total) %>% 
    select(-YEAR,-Variable)
  tba2_nombres <- colnames(tba2)
  tba2 <- tba2 %>% relativo()
  tba2 <- matrix(tba2,ncol=length(tba2_nombres))
  tba2 <- data.frame(tba2)
  colnames(tba2) <- tba2_nombres
  tba2 <- tba2 %>% 
    mutate(Fecha=fecha) %>%  
    gather(key="Clase", value = "Relativo",tba2_nombres)
  n <- tba %>% select(Clase) %>% distinct() %>% nrow()
  cat <- tba %>% select(Clase) %>% distinct() %>% arrange(Clase)
  colo <- as.matrix(colores[1:n],ncol=1)
  rownames(colo)= cat$Clase
  tba0 <- tba0 %>% inner_join(tba2)
  gfa <-  tba0 %>% 
    hchart( "line", hcaes(x = Fecha, y = Total, group = Clase), color = as.vector(colo), zoomType = list(enabled=FALSE),  resetZoomButton=TRUE)%>% 
    hc_rangeSelector(inputEnabled = FALSE, enabled=FALSE) %>% 
    hc_add_theme(hc_theme_elementary()) %>% 
    hc_chart(zoomType = "x",type = "datetime") %>% 
    hc_plotOptions(line = list(
      marker = list( enabled = TRUE, symbol = "square", radius = 1) ))%>%
    hc_yAxis( lineColor = '#787878', lineWidth = 1, min = 0,
              title = list(text = eje,  offset = 70,
                           style = list( fontWeight = "bold",
                                         fontSize = "18px",
                                         color = 'black')),
              opposite = FALSE,
              labels = list(
                style = list(
                  fontWeight = "bold",
                  color = 'black',
                  fontSize = '18px'
                ))
    ) %>% 
    hc_xAxis(  lineColor = '#787878', 
               title = list(text = "Año",  offset = 70, 
                            style = list( fontWeight = "bold",
                                          fontSize = "18px",
                                          color = 'black')),
               opposite = FALSE,
               labels = list(
                 style = list(
                   fontWeight = "bold",
                   color = 'black',
                   fontSize = '18px'
                 ))
    ) %>% 
    hc_title(style = list( fontWeight = "bold", 
                           fontSize = "22px", 
                           useHTML = TRUE ), text = titulo) %>%
    hc_exporting(enabled = TRUE, filename = "export") %>%
    hc_legend(enabled = TRUE, align = "center",layout = "horizontal",
              x = 42, y = 0,
              itemStyle = list(
                fontWeight = "bold",
                color = 'black',
                fontSize = '18px'
              )) %>%
    hc_tooltip(crosshairs = TRUE, 
               pointFormat= '<span style="color:{series.color}">\u25CF </span><b>{series.name}: {point.y}</b> ({point.Relativo}%)<br/>',
               backgroundColor =  hex_to_rgba("#baaeae", 0.7), 
               borderColor = "#6d6666", shared = TRUE,
               borderWidth = 5, useHTML = TRUE) %>%
    hc_add_theme(hc_theme_flat())
  
  return(gfa)
}


series <- function(datos,categoria,colores,titulo,eje){
  fecha <- (datos %>% filter(Variable == categoria)%>% 
              spread(key = Clase, value = Total) %>% 
              mutate(Fecha = paste(WEEK,sep = "-")))$Fecha
  datos <- ungroup(datos)
  tba <- datos %>% filter(Variable == categoria) %>%
    mutate(Fecha = paste(WEEK,sep = "-"))
  tba0 <- expand.grid(unique(tba$Fecha),unique(tba$Clase))
  colnames(tba0) <- c("Fecha","Clase")
  tba0 <- tba0 %>% left_join(tba %>% select(-WEEK,-Variable)) %>% arrange(Fecha)
  tba2 <- datos %>% filter(Variable == categoria) %>% 
    spread(key = Clase, value = Total) %>% 
    select(-WEEK,-Variable)
  tba2_nombres <- colnames(tba2)
  tba2 <- tba2 %>% relativo()
  tba2 <- matrix(tba2,ncol=length(tba2_nombres))
  tba2 <- data.frame(tba2)
  colnames(tba2) <- tba2_nombres
  tba2 <- tba2 %>% 
    mutate(Fecha=fecha) %>%  
    gather(key="Clase", value = "Relativo",tba2_nombres)
  n <- tba %>% select(Clase) %>% distinct() %>% nrow()
  cat <- tba %>% select(Clase) %>% distinct() %>% arrange(Clase)
  colo <- as.matrix(colores[1:n],ncol=1)
  rownames(colo)= cat$Clase
  tba0 <- tba0 %>% inner_join(tba2)
  gfa <-  tba0 %>% 
    hchart( "line", hcaes(x = Fecha, y = Total, group = Clase), color = as.vector(colo), zoomType = list(enabled=FALSE),  resetZoomButton=TRUE)%>% 
    hc_rangeSelector(inputEnabled = FALSE, enabled=FALSE) %>% 
    hc_add_theme(hc_theme_elementary()) %>% 
    hc_chart(zoomType = "x",type = "datetime") %>% 
    hc_plotOptions(line = list(
      marker = list( enabled = TRUE, symbol = "square", radius = 1) ))%>%
    hc_yAxis( lineColor = '#787878', lineWidth = 1, min = 0,
              title = list(text = eje,  offset = 70,
                           style = list( fontWeight = "bold",
                                         fontSize = "18px",
                                         color = 'black')),
              opposite = FALSE,
              labels = list(
                style = list(
                  fontWeight = "bold",
                  color = 'black',
                  fontSize = '18px'
                ))
    ) %>% 
    hc_xAxis(  lineColor = '#787878', 
               title = list(text = "Semana",  offset = 70, 
                            style = list( fontWeight = "bold",
                                          fontSize = "18px",
                                          color = 'black')),
               opposite = FALSE,
               labels = list(
                 style = list(
                   fontWeight = "bold",
                   color = 'black',
                   fontSize = '18px'
                 ))
    ) %>% 
    hc_title(style = list( fontWeight = "bold", 
                           fontSize = "22px", 
                           useHTML = TRUE ), text = titulo) %>%
    hc_exporting(enabled = TRUE, filename = "export") %>%
    hc_legend(enabled = TRUE, align = "center",layout = "horizontal",
              x = 42, y = 0,
              itemStyle = list(
                fontWeight = "bold",
                color = 'black',
                fontSize = '18px'
              )) %>%
    hc_tooltip(crosshairs = TRUE, 
               pointFormat= '<span style="color:{series.color}">\u25CF </span><b>{series.name}: {point.y}</b> ({point.Relativo}%)<br/>',
               backgroundColor =  hex_to_rgba("#baaeae", 0.7), 
               borderColor = "#6d6666", shared = TRUE,
               borderWidth = 5, useHTML = TRUE) %>%
    hc_add_theme(hc_theme_flat())
  
  return(gfa)
}

series3 <- function(datos,categoria,colores,titulo,eje){
  fecha <- (datos %>% filter(Variable == categoria)%>% 
              spread(key = Clase, value = Total) %>% 
              mutate(Fecha = paste(MONTH,sep = "-")))$Fecha
  datos <- ungroup(datos)
  tba <- datos %>% filter(Variable == categoria) %>%
    mutate(Fecha = paste(MONTH,sep = "-"))
  tba0 <- expand.grid(unique(tba$Fecha),unique(tba$Clase))
  colnames(tba0) <- c("Fecha","Clase")
  tba0 <- tba0 %>% left_join(tba %>% select(-MONTH,-Variable)) %>% arrange(Fecha)
  tba2 <- datos %>% filter(Variable == categoria) %>% 
    spread(key = Clase, value = Total) %>% 
    select(-MONTH,-Variable)
  tba2_nombres <- colnames(tba2)
  tba2 <- tba2 %>% relativo()
  tba2 <- matrix(tba2,ncol=length(tba2_nombres))
  tba2 <- data.frame(tba2)
  colnames(tba2) <- tba2_nombres
  tba2 <- tba2 %>% 
    mutate(Fecha=fecha) %>%  
    gather(key="Clase", value = "Relativo",tba2_nombres)
  n <- tba %>% select(Clase) %>% distinct() %>% nrow()
  cat <- tba %>% select(Clase) %>% distinct() %>% arrange(Clase)
  colo <- as.matrix(colores[1:n],ncol=1)
  rownames(colo)= cat$Clase
  tba0 <- tba0 %>% inner_join(tba2)
  gfa <-  tba0 %>% 
    hchart( "line", hcaes(x = Fecha, y = Total, group = Clase), color = as.vector(colo), zoomType = list(enabled=FALSE),  resetZoomButton=TRUE)%>% 
    hc_rangeSelector(inputEnabled = FALSE, enabled=FALSE) %>% 
    hc_add_theme(hc_theme_elementary()) %>% 
    hc_chart(zoomType = "x",type = "datetime") %>% 
    hc_plotOptions(line = list(
      marker = list( enabled = TRUE, symbol = "square", radius = 1) ))%>%
    hc_yAxis( lineColor = '#787878', lineWidth = 1, min = 0,
              title = list(text = eje,  offset = 70,
                           style = list( fontWeight = "bold",
                                         fontSize = "18px",
                                         color = 'black')),
              opposite = FALSE,
              labels = list(
                style = list(
                  fontWeight = "bold",
                  color = 'black',
                  fontSize = '18px'
                ))
    ) %>% 
    hc_xAxis(  lineColor = '#787878', 
               title = list(text = "mes",  offset = 70, 
                            style = list( fontWeight = "bold",
                                          fontSize = "18px",
                                          color = 'black')),
               opposite = FALSE,
               labels = list(
                 style = list(
                   fontWeight = "bold",
                   color = 'black',
                   fontSize = '18px'
                 ))
    ) %>% 
    hc_title(style = list( fontWeight = "bold", 
                           fontSize = "22px", 
                           useHTML = TRUE ), text = titulo) %>%
    hc_exporting(enabled = TRUE, filename = "export") %>%
    hc_legend(enabled = TRUE, align = "center",layout = "horizontal",
              x = 42, y = 0,
              itemStyle = list(
                fontWeight = "bold",
                color = 'black',
                fontSize = '18px'
              )) %>%
    hc_tooltip(crosshairs = TRUE, 
               pointFormat= '<span style="color:{series.color}">\u25CF </span><b>{series.name}: {point.y}</b> ({point.Relativo}%)<br/>',
               backgroundColor =  hex_to_rgba("#baaeae", 0.7), 
               borderColor = "#6d6666", shared = TRUE,
               borderWidth = 5, useHTML = TRUE) %>%
    hc_add_theme(hc_theme_flat())
  
  return(gfa)
}
############### 3. Construir torta: ###############

torta <- function(datos,variable,colores,titulo,etiqueta,ano,periodo,periodo_titulo){
  datos <- ungroup(datos)
  tba <- datos %>% filter(Variable == variable) %>% select(-Variable)
  n <- tba %>% select(Clase) %>% distinct() %>% nrow()
  cat <- tba %>% select(Clase) %>% distinct() %>% arrange(Clase)
  colo <- as.matrix(colores[1:n],ncol=1)
  rownames(colo)= cat$Clase
  tba_actual <- tba %>% filter(YEAR == ano, SEMESTRE == periodo)
  colo2 <- as.vector(colo[tba_actual$Clase,])
  gfa_final <- highchart()  %>%
    hc_title(style = list( fontWeight = "bold",
                           fontSize = "22px", 
                           useHTML = TRUE ), 
             text = paste0(titulo, ', periodo', periodo_titulo)) %>% 
    hc_add_series(tba_actual, "pie", hcaes(name = Clase, y = Total), name = etiqueta, showInLegend = TRUE) %>%
    hc_plotOptions( pie = list(
      allowPointSelect = TRUE,
      colorByPoint = TRUE, colors=colo2,
      dataLabels = list(
        enabled = TRUE,
        format = '<b>{point.name}</b>: {point.percentage:.1f} %',
        style = list(
          fontWeight = "bold",
          color = 'black',
          fontSize = '18px'
        )
      ) )) %>%
    hc_legend(enabled = TRUE, align = "center",
              itemStyle = list(
                fontWeight = "bold",
                color = 'black',
                fontSize = '18px'
              )) %>%
    hc_exporting(enabled = TRUE, filename = "export")
  return(gfa_final)
}

############### 3.1. Torta una categoría ###############

torta_1 <- function(datos,variable,colores,titulo,etiqueta,ano,periodo,periodo_titulo){
  datos <- ungroup(datos)
  tba <- datos %>% filter(Variable == variable) %>% select(-Variable)
  n <- tba %>% select(Clase) %>% distinct() %>% nrow()
  cat <- tba %>% select(Clase) %>% distinct() %>% arrange(Clase)
  colo <- as.matrix(colores[1:n],ncol=1)
  rownames(colo)= cat$Clase
  tba0 <- expand.grid(unique(tba$YEAR),unique(tba$SEMESTRE),unique(tba$Clase))
  colnames(tba0) <- c("YEAR","SEMESTRE","Clase")
  tba1 <- tba0 %>% left_join(tba)
  tba_actual <- tba1 %>% filter(YEAR == ano, SEMESTRE == periodo)
  colo2 <- as.vector(colo[tba_actual$Clase,])
  gfa_final <- highchart() %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "22px" ), 
             text = paste0(titulo, ', periodo', periodo_titulo)) %>% 
    hc_add_series(tba_actual, "pie", hcaes(name = Clase, y = Total), 
                  name = etiqueta, showInLegend = FALSE) %>%
    hc_plotOptions( pie = list(
      allowPointSelect = TRUE, 
      colorByPoint = TRUE, colors=colo2,
      dataLabels = list(
        enabled = TRUE,
        format = '<b>{point.name}</b>: {point.percentage:.1f} %',
        style = list(
          fontWeight = "bold",
          color = 'black',
          fontSize = '18px'
        )
      ) ))%>%
    hc_exporting(enabled = TRUE, filename = "export")
  return(gfa_final)
}


############### 3.2. Construir drilldown torta en dos niveles (si y no): ###############

drilldown_si_no_torta <- function(datos,categoria,categoria_drilldown,colores,titulo,titulo_drilldown,etiqueta,eje,ano,periodo,periodo_titulo){
  datos <- ungroup(datos)
  tba <- datos %>% filter(Variable == categoria) %>% 
    select(-Variable) %>% filter(is.na(Clase)!=TRUE)
  n <- tba %>% select(Clase) %>% distinct() %>% nrow()
  cat <- tba %>% select(Clase) %>% distinct() %>% arrange(Clase)
  colo <- as.matrix(colores[1:n],ncol=1)
  rownames(colo)= cat$Clase
  tba_actual <- tba %>% filter(YEAR == ano, SEMESTRE == periodo) %>%
    arrange( desc(Total)) %>% select(-YEAR, -SEMESTRE) %>% mutate(drilldown=tolower(Clase))
  colo2 <- as.vector(colo[tba_actual$Clase,])
  datos <- ungroup(datos)
  sub_tba <- datos %>% filter(Variable == categoria_drilldown) %>% 
    select(-Variable) %>% filter(is.na(Clase) != TRUE, Clase != "No Aplica")
  sub_tba_actual <- sub_tba %>% filter(YEAR == ano, SEMESTRE == periodo) %>%
    arrange( desc(Total)) %>% select(-YEAR, -SEMESTRE)
  
  gfa <- highchart()  %>%
    hc_add_series(tba_actual, "pie", hcaes(x = paste(Clase, "-", round(Total*100/sum(Total), 1), "%"), y = Total), name = etiqueta, showInLegend =FALSE,
                  colorByPoint = TRUE ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "25px" ), text = paste0(titulo, periodo_actual_titulo)
    ) %>%
    hc_plotOptions( pie = list(
      dataLabels= list(enabled=TRUE,
                       style = list(
                         fontWeight = "bold",
                         color = 'black',
                         fontSize = '18px'
                       )),
      colors=colo2),
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE,
                          style = list( fontWeight = "bold",
                                        color = 'black',
                                        fontSize = "18px"))
      ))%>%
    hc_yAxis(
      title = list(style = list( fontWeight = "bold",
                                 color = 'black',
                                 fontSize = "18px"),text = eje),
      labels = list(
        style = list(
          fontWeight = "bold",
          color = 'black',
          fontSize = '18px'
        ))
    ) %>% 
    hc_xAxis(type="category",
             labels = list(
               style = list(
                 fontWeight = "bold",
                 color = 'black',
                 fontSize = '18px'
               ))) %>%
    hc_add_theme(hc_theme_elementary()) %>%
    hc_exporting(enabled = TRUE, filename = "export")%>%
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list(
        list(
          id = "sí",
          name = titulo_drilldown,
          data =  list_parse2(sub_tba_actual),
          type = "column"
        )
      )
    )
  return(gfa)
}

############### 4. Construir barras horizantales: ###############

barra_horizontal <- function(datos,categoria,colores,ano,periodo,periodo_titulo,titulo,eje){
  datos <- ungroup(datos)
  tba <- datos %>% filter(Variable == categoria) %>% select(-Variable) %>% filter(is.na(Clase)!=TRUE)
  n <- tba %>% select(Clase) %>% distinct() %>% nrow()
  cat <- tba %>% select(Clase) %>% distinct()  %>% arrange(Clase)
  colo <- as.matrix(colores[1:n],ncol=1)
  rownames(colo)= cat$Clase
  tba_actual <- tba %>% filter(YEAR == ano, SEMESTRE == periodo) %>%  arrange( desc(Total))
  colo2 <- as.vector(colo[tba_actual$Clase,])
  gfa <- highchart()  %>%
    hc_add_series(tba_actual, "bar", hcaes(x = paste(Clase,"-",round(Total*100/sum(Total),1),"%"),  y = Total), name = eje, showInLegend =FALSE) %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "25px" ), text = paste0(titulo,', periodo',periodo_titulo)) %>%
    hc_plotOptions( bar = list( dataLabels = list(enabled=TRUE,
                                                  style = list(
                                                    fontWeight = "bold",
                                                    color = 'black',
                                                    fontSize = '18px'
                                                  )),
                                colorByPoint = TRUE, colors=colo2 ))%>%
    hc_yAxis(
      title = list(text = eje,
                   style = list( fontWeight = "bold",
                                 color = 'black',
                                 fontSize = "18px" )),
      labels = list(
        style = list(
          fontWeight = "bold",
          color = 'black',
          fontSize = '18px'
        ))
    ) %>% 
    hc_xAxis(categories = tba_actual$Clase,
             labels = list(
               style = list(
                 fontWeight = "bold",
                 color = 'black',
                 fontSize = '18px'
               ))) %>%
    hc_add_theme(hc_theme_elementary()) %>%
    hc_exporting(enabled = TRUE, filename = "export")
  return(gfa)
}

############### 4.1. Construir drilldown barra horizontal en dos niveles (si y no): ###############

drilldown_si_no_barra_horizontal <- function(datos,categoria,categoria_drilldown,colores,titulo,titulo_drilldown,etiqueta,eje,ano,periodo,periodo_titulo){
  datos <- ungroup(datos)
  tba <- datos %>% filter(Variable == categoria) %>% 
    select(-Variable) %>% filter(is.na(Clase)!=TRUE)
  n <- tba %>% select(Clase) %>% distinct() %>% nrow()
  cat <- tba %>% select(Clase) %>% distinct() %>% arrange(Clase)
  colo <- as.matrix(colores[1:n],ncol=1)
  rownames(colo)= cat$Clase
  tba_actual <- tba %>% filter(YEAR == ano, SEMESTRE == periodo) %>%
    arrange( desc(Total)) %>% select(-YEAR, -SEMESTRE) %>% mutate(drilldown=tolower(Clase))
  colo2 <- as.vector(colo[tba_actual$Clase,])
  datos <- ungroup(datos)
  sub_tba <- datos %>% filter(Variable == categoria_drilldown) %>% 
    select(-Variable) %>% filter(is.na(Clase) != TRUE, Clase != "No Aplica")
  sub_tba_actual <- sub_tba %>% filter(YEAR == ano, SEMESTRE == periodo) %>%
    arrange( desc(Total)) %>% select(-YEAR, -SEMESTRE)
  
  gfa <- highchart()  %>%
    hc_add_series(tba_actual, "bar", hcaes(x = paste(Clase, "-", round(Total*100/sum(Total), 1), "%"), y = Total), name = etiqueta, showInLegend =FALSE,
                  colorByPoint = TRUE ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "25px" ), text = paste0(titulo, periodo_actual_titulo)
    ) %>%
    hc_plotOptions( bar = list(
      dataLabels= list(enabled=TRUE,
                       style = list(
                         fontWeight = "bold",
                         color = 'black',
                         fontSize = '18px'
                       )),
      colors=colo2),
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE,
                          style = list( fontWeight = "bold",
                                        color = 'black',
                                        fontSize = "18px"))
      ))%>%
    hc_yAxis(
      title = list(style = list( fontWeight = "bold",
                                 color = 'black',
                                 fontSize = "18px"),text = eje),
      labels = list(
        style = list(
          fontWeight = "bold",
          color = 'black',
          fontSize = '18px'
        ))
    ) %>% 
    hc_xAxis(type="category",
             labels = list(
               style = list(
                 fontWeight = "bold",
                 color = 'black',
                 fontSize = '18px'
               ))) %>%
    hc_add_theme(hc_theme_elementary()) %>%
    hc_exporting(enabled = TRUE, filename = "export")%>%
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list(
        list(
          id = "sí",
          name = titulo_drilldown,
          data =  list_parse2(sub_tba_actual),
          type = "column"
        )
      )
    )
  return(gfa)
}

############### 5. Construir barras verticales: ###############

barra_vertical <- function(datos,categoria,colores,ano,periodo,periodo_titulo,titulo,eje){
  datos <- ungroup(datos)
  tba <- datos %>% filter(Variable == categoria) %>% select(-Variable) %>% filter(is.na(Clase)!=TRUE)
  n <- tba %>% select(Clase) %>% distinct() %>% nrow()
  cat <- tba %>% select(Clase) %>% distinct()  %>% arrange(Clase)
  colo <- as.matrix(colores[1:n],ncol=1)
  rownames(colo)= cat$Clase
  tba_actual <- tba %>% filter(YEAR == ano, SEMESTRE == periodo) %>%  arrange( desc(Total))
  colo2 <- as.vector(colo[tba_actual$Clase,])
  gfa <- highchart()  %>%
    hc_add_series(tba_actual, "column", hcaes(x = paste(Clase,"-",round(Total*100/sum(Total),1),"%"),  y = Total), name = eje, showInLegend =FALSE) %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "25px" ), text = paste0(titulo,', periodo',periodo_titulo)) %>%
    hc_plotOptions( column = list( dataLabels = list(enabled=TRUE,
                                                     style = list(
                                                       fontWeight = "bold",
                                                       color = 'black',
                                                       fontSize = '18px'
                                                     )),
                                   colorByPoint = TRUE, colors=colo2 ))%>%
    hc_yAxis(
      title = list(text = eje,
                   style = list( fontWeight = "bold",
                                 fontSize = "18px",
                                 color = 'black' )),
      labels = list(
        style = list(
          fontWeight = "bold",
          color = 'black',
          fontSize = '18px'
        ))
    ) %>% 
    hc_xAxis(categories = tba_actual$Clase,
             labels = list(
               style = list(
                 fontWeight = "bold",
                 color = 'black',
                 fontSize = '18px'
               ))) %>%
    hc_add_theme(hc_theme_elementary()) %>%
    hc_exporting(enabled = TRUE, filename = "export")
  return(gfa)
}

############### 5.1. Construir drilldown barras verticales en dos niveles (si y no): ###############

drilldown_si_no_barra_vertical <- function(datos,categoria,categoria_drilldown,colores,titulo,titulo_drilldown,etiqueta,eje,ano,periodo,periodo_titulo){
  datos <- ungroup(datos)
  tba <- datos %>% filter(Variable == categoria) %>% 
    select(-Variable) %>% filter(is.na(Clase)!=TRUE)
  n <- tba %>% select(Clase) %>% distinct() %>% nrow()
  cat <- tba %>% select(Clase) %>% distinct()  %>% arrange(Clase)
  colo <- as.matrix(colores[1:n],ncol=1)
  rownames(colo)= cat$Clase
  tba_actual <- tba %>% filter(YEAR == ano, SEMESTRE == periodo) %>%
    arrange( desc(Total)) %>% select(-YEAR, -SEMESTRE) %>% mutate(drilldown=tolower(Clase))
  colo2 <- as.vector(colo[tba_actual$Clase,])
  datos <- ungroup(datos)
  sub_tba <- datos %>% filter(Variable == categoria_drilldown) %>% 
    select(-Variable) %>% filter(is.na(Clase) != TRUE, Clase != "No Aplica")
  sub_tba_actual <- sub_tba %>% filter(YEAR == ano, SEMESTRE == periodo) %>%
    arrange( desc(Total)) %>% select(-YEAR, -SEMESTRE)
  
  gfa <- highchart()  %>%
    hc_add_series(tba_actual, "column", hcaes(x = paste(Clase, "-", round(Total*100/sum(Total), 1), "%"), y = Total), name = etiqueta, showInLegend =FALSE,
                  colorByPoint = TRUE ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "25px" ), text = paste0(titulo, periodo_actual_titulo)
    ) %>%
    hc_plotOptions( column = list(
      dataLabels= list(enabled=TRUE,
                       style = list(
                         fontWeight = "bold",
                         color = 'black',
                         fontSize = '18px'
                       )),
      colors=colo2),
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)
      ))%>%
    hc_yAxis(
      title = list(style = list( fontWeight = "bold",
                                 color = 'black',
                                 fontSize = "18px"),text = eje),
      labels = list(
        style = list(
          fontWeight = "bold",
          color = 'black',
          fontSize = '18px'
        ))
    ) %>% 
    hc_xAxis(type="category",
             labels = list(
               style = list(
                 fontWeight = "bold",
                 color = 'black',
                 fontSize = '18px'
               ))) %>%
    hc_add_theme(hc_theme_elementary()) %>%
    hc_exporting(enabled = TRUE, filename = "export")%>%
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list(
        list(
          id = "sí",
          name = titulo_drilldown,
          data =  list_parse2(sub_tba_actual),
          type = "column"
        )
      )
    )
  return(gfa)
}

############### 6. Construir Treemap: ###############
treemap_dos_niveles <- function(datos,ano,periodo,variable_externa,variable_interna,titulo, periodo_titulo,colores){
  tba <- datos %>% filter(YEAR==ano,SEMESTRE==periodo) %>% ungroup() %>% select(variable_externa,variable_interna,Total)
  tba0 <- apply(tba[,1:2], 2, as.factor)
  tba0 <- data.frame(tba0) %>% mutate(Total = tba$Total)
  n <- tba %>% distinct(variable_externa) %>% nrow()
  arbol_carreras <- treemap(tba0, 
                            index = c(variable_externa,variable_interna),  
                            vSize = "Total", draw = FALSE, 
                            type = "categorical",
                            vColor = variable_externa,
                            palette = colores[1:n])
  gfa <- hctreemap(arbol_carreras, allowDrillToNode=TRUE,
                   levelIsConstant= FALSE,
                   levels = list(list( level=1,
                                       dataLabels = list( enabled = TRUE
                                                          #, style = list(fontSize = "20px" ) 
                                       )),
                                 list( level=2, dataLabels = list( enabled = FALSE))
                   ) ) %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "20px" ), text = paste0(titulo, periodo_titulo))%>%
    hc_exporting(enabled = TRUE, filename = "export")
  return(gfa)
}

treemap_dos_niveles2 <- function(datos,variable_externa,variable_interna,  Total,titulo, periodo_titulo,colores){
  tba <- datos %>% select(variable_externa,variable_interna,Total)
  tba0 <- apply(tba[,1:2], 2, as.factor)
  tba0 <- data.frame(tba0) %>% mutate(Total = tba$Total)
  n <- tba %>% distinct(variable_externa) %>% nrow()
  arbol_carreras <- treemap(tba0, 
                            index = c(variable_externa,variable_interna),  
                            vSize = "Total", draw = FALSE, 
                            type = "categorical",
                            vColor = variable_externa,
                            palette = colores[1:n])
  gfa <- hctreemap(arbol_carreras, allowDrillToNode=TRUE,
                   levelIsConstant= FALSE,
                   levels = list(list( level=1,
                                       dataLabels = list( enabled = TRUE
                                                          #, style = list(fontSize = "20px" ) 
                                       )),
                                 list( level=2, dataLabels = list( enabled = FALSE))
                   ) ) %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "20px" ), text = paste0(titulo, periodo_titulo))%>%
    hc_exporting(enabled = TRUE, filename = "export")
  return(gfa)
}


############### 7. Construir boxplot: ###############

caja <- function(datos, titulo, eje){
  datos_2 <- datos %>% filter(Serie != "2008-1")
  gfa <- hcboxplot(x = datos_2$PTOTAL, var = datos_2$Serie, outliers = FALSE) %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "25px" ), text = titulo) %>%
    hc_plotOptions( boxplot = list(
      colorByPoint = F, color="#00a703" ))%>%
    hc_exporting(enabled = TRUE, filename = "export") %>% 
    hc_yAxis( min = 0, max = 1000,
              title = list(style = list( fontWeight = "bold",
                                         color = "black",
                                         fontSize = "18px"), text = eje),
              labels = list(
                style = list(
                  fontWeight = "bold",
                  color = 'black',
                  fontSize = '18px'
                ))
    ) %>% 
    hc_xAxis(
      title = list(style = list( fontWeight = "bold",
                                 color = "black",
                                 fontSize = "18px"), text = "Periodo"),
      labels = list(
        style = list(
          fontWeight = "bold",
          color = 'black',
          fontSize = '18px'
        ))
    ) %>% 
    hc_add_theme(hc_theme_elementary()) %>%
    hc_chart(type = "column")
  return(gfa)
}

############### 7.1. Construir boxplot varias categorias: ###############

caja_n <- function(datos, titulo, eje, colores ){
  n <- datos %>% select(Agrupacion) %>% distinct() %>% nrow()
  colo <- colores[1:n]
  datos_2 <- datos %>% filter(Serie != "2008-1")
  gfa <- hcboxplot(x = datos_2$PTOTAL, var = datos_2$Serie, var2 = datos_2$Agrupacion, outliers = FALSE, color=colo) %>%
    hc_title(style = list( fontWeight = "bold",fontSize = "25px" ), text = titulo) %>%
    hc_exporting(enabled = TRUE, filename = "export") %>% 
    hc_yAxis(lineColor = '#787878', lineWidth = 1, min = 0, max = 1000,
             title = list(style = list( fontWeight = "bold",
                                        color = "black",
                                        fontSize = "18px"), text = eje),
             labels = list(
               style = list(
                 fontWeight = "bold",
                 color = 'black',
                 fontSize = '18px'
               ))
    ) %>% 
    hc_xAxis(lineColor = '#787878', lineWidth = 1,
             title = list(style = list( fontWeight = "bold",
                                        color = "black",
                                        fontSize = "18px"), text = "Periodo"),
             labels = list(
               style = list(
                 fontWeight = "bold",
                 color = 'black',
                 fontSize = '18px'
               ))
    )  %>%
    hc_add_theme(hc_theme_elementary()) %>%
    hc_legend(enabled = TRUE, align = "center",layout = "horizontal",
              x = 42, y = 0,
              itemStyle = list(
                fontWeight = "bold",
                color = 'black',
                fontSize = '18px'
              )) %>%
    hc_chart(type = "column") 
  return(gfa)
}

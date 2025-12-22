plot_ly_fn <- function(
    data = NULL,
    x = NULL,
    y = NULL,
    type = c("line","point","linepoint","bar"),
    labelx = "eixo x",
    labely = "eixo y",
    corl = "blue",
    widthl = 2,
    corp = "blue",
    corlp = "black",
    sizep = 6,
    widthlp = 1,
    corb = "blue",
    corlb = "black",
    widthlb = 1.5
){
  if(length(type) > 1){
    type <- type[1]
  } 
  if(type == "line"){
    graf <-
      plot_ly()%>%
      add_trace(data = data,
                type = 'scatter',mode='lines',
                x = ~eval(parse(text=x)), y = ~eval(parse(text=y)),
                line = list(
                  color = corl,  # Define a cor da linha, pode ser um nome de cor ou um código hexadecimal
                  width = widthl # Define a espessura da linha
                )) %>%
      layout(xaxis = list(title = labelx),
             yaxis = list(title = labely))
    return(graf)
  }
  if(type == "point"){
    graf <-
      plot_ly()%>%
      add_trace(data = data,
                type = 'scatter',mode='markers',
                x = ~eval(parse(text=x)), y = ~eval(parse(text=y)),
                marker = list(color = corp,# Define a cor do interior dos pontos como vermelho
                              line = list(color = corlp,
                                          width = widthlp),# Define a cor da borda dos pontos como azul e largura como 2
                              size = sizep)# Define o tamanho dos pontos como 8
      )%>%
      layout(xaxis = list(title = labelx),
             yaxis = list(title = labely))
    return(graf)
  }
  if(type =="linepoint"){
    graf <-
      plot_ly()%>%
      add_trace(data = data,
                type = 'scatter',mode='lines+markers',
                x = ~eval(parse(text=x)), y = ~eval(parse(text=y)),
                line = list(
                  color = corl,  # Define a cor da linha, pode ser um nome de cor ou um código hexadecimal
                  width = widthl # Define a espessura da linha
                ),
                marker = list(color = corp,# Define a cor do interior dos pontos como vermelho
                              line = list(color = corlp,
                                          width = widthlp),# Define a cor da borda dos pontos como azul e largura como 2
                              size = sizep)
      ) %>%
      layout(xaxis = list(title = labelx),
             yaxis = list(title = labely))
    return(graf)
  }
  
  if(type =="bar"){
    graf <-
      plot_ly()%>%
      add_bars(data = data,
               x = ~eval(parse(text=x)), y = ~eval(parse(text=y)),
               marker = list(
                 color = corb,
                 line = list(color = corlb,
                             width = widthlb)
               )
      )%>%
      layout(xaxis = list(title = labelx),
             yaxis = list(title = labely))
    return(graf)
  }
}




# config(toImageButtonOptions = list(
#format = "svg"
#scale=3
#))





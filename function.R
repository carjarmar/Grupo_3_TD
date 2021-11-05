source("teoriadecision_funciones_incertidumbre.R")


  ## Funcion que busca los alphas

search.alphas = function(search_vector, Altmax, Altmin, precision, current_digits, favorable=TRUE){
  # return alpha values in between which the alternatives change with our
  # chosen precision
  
  # parameters
  #   search vector: vector which we want to search for margin alpha values
  #   Altmax, Altmin: minimal and maximal value out of X values, computed in
  #   the function criterio.Hurwicz
  #   precision: The amount of digits we want our alphas to have
  #   current_digits: The amount of digits we have in our current 
  #   computation (starting with 1 for 0.x in the above function criterio.Hurwicz)
  
  if (favorable) {
    remember = c()
    indices = c()
    AltH = search_vector[1] * Altmax + (1-search_vector[1]) * Altmin 
    previous = which.max.general(AltH)
    for (i in 2:length(search_vector)){
      AltH = search_vector[i] * Altmax + (1-search_vector[i]) * Altmin 
      alternativa = which.max.general(AltH)
      print(alternativa)
      if (alternativa != previous){
        # this will save the two values of alpha for which a change happens
        remember <- c(remember, search_vector[i])
        indices <- c(indices, i)
      }
      previous = alternativa
    }
  } else {
    
    remember = c()
    indices = c()
    AltH = (1-search_vector[1]) * Altmax +  search_vector[1] * Altmin 
    previous = which.min.general(AltH)
    for (i in 2:length(search_vector)){
      AltH = (1-search_vector[i]) * Altmax + search_vector[i] * Altmin 
      alternativa = which.min.general(AltH)
      print(alternativa)
      if (alternativa != previous){
        # this will save the two values of alpha for which a change happens
        remember <- c(remember, search_vector[i])
        indices <- c(indices, i)
      }
      previous = alternativa
    }
  }
  
  return(remember)
}



  #Funcion pre-definida criterio.Hurwicz con algunas modificaciones

criterio.Hurwicz.mod = function(tablaX,favorable=TRUE, precision = 2) {
  X = tablaX;
  if (favorable) {
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    search_vector = seq(0, 1, by=0.01)
    # return alpha values in between which the alternatives change with our
    # chosen precision
    Hurwicz = c()
    Alt_Hurwicz = c()
    alfas = search.alphas(search_vector, Altmax, Altmin, precision, 1, favorable=TRUE)
    for (alfa in alfas){
      AltH =  alfa * Altmax + (1-alfa) * Altmin
      Alt_Hurwicz = c(Alt_Hurwicz, which.max.general(AltH))
      Hurwicz = c(Hurwicz, max(AltH))
      }
    metodo = 'favorable';
  } else {
    Hurwicz = c()
    Alt_Hurwicz = c()
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    search_vector = seq(0, 1, by=0.1)
    alfas = search.alphas(search_vector, Altmax, Altmin, precision, 1, favorable=FALSE)
    for (alfa in alfas){
      AltH = (1-alfa) * Altmax + alfa * Altmin
      Alt_Hurwicz = c(Alt_Hurwicz, which.min.general(AltH))
      Hurwicz = c(Hurwicz, min(AltH))
    }
    metodo = 'desfavorable';
  }
  resultados = list();
  resultados$criterio = 'Hurwicz';
  resultados$alfa = alfas;
  resultados$metodo = metodo;
  resultados$tablaX = tablaX;
  resultados$ValorOptimo = Hurwicz;
  resultados$AlternativaOptima = Alt_Hurwicz;
  resultados$Conclusion = paste("La mejor alternativa es la", resultados$AlternativaOptima, "a partir del alpha", resultados$alfa)

  return(resultados);
  return(resultados$Conclusion);

  
}






## EJEMPLOS PARA PROBAR LA FUNCION ##

    #ejemplo 1
#X1 = matrix(c(5,4,6,2,3,1,-1,8,7,5,2,0),nrow=4,ncol=3,byrow=TRUE)
#colnames(X1)=c('e1','e2','e3')
#rownames(X1)=c('d1','d2','d3','d4')

#which.min.general(c(3,2,8,2,9,2))

#criterio.Hurwicz.mod(X1)
#dibuja.criterio.Hurwicz(X1)



    #ejemplo 2 (ejercicio relacion 1.4)
#tablaX = crea.tablaX(c(2160,360,720,720,3480,480), numalternativas = 3, numestados = 2)
#colnames(tablaX)=c('e1','e2')
#rownames(tablaX)=c('d1','d2','d3')

#(caso no favorable)
#criterio.Hurwicz.mod(tablaX, favorable = FALSE)
#dibuja.criterio.Hurwicz(tablaX, favorable = FALSE)



    #ejemplo 3 (ejercicio relacion 1.5)
#X3 = crea.tablaX(c(125,120,156,60,130,80), numalternativas = 3, numestados = 2)
#colnames(X3)=c('e1','e2')
#rownames(X3)=c('d1','d2','d3')

#(caso no favorable, y con dos puntos de corte, luego deberia darnos 2 alfas diferentes)
#criterio.Hurwicz.mod(X3, favorable = FALSE)
#dibuja.criterio.Hurwicz(X3, favorable = FALSE)



    #ejemplo 4 (ejercicio relacion 1.6)
#X4 = crea.tablaX(c(55,5,70,-30,85,-65), numalternativas = 3, numestados = 2)
#colnames(X4)=c('e1','e2')
#rownames(X4)=c('d1','d2','d3')

#(caso favorable)
#criterio.Hurwicz.mod(X4)
#dibuja.criterio.Hurwicz(X4)

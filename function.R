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







## factor de optimismo   (alfab * "lo mejor" Altmax en favor. y Altmin en desf.)
criterio.Hurwicz = function(tablaX,favorable=TRUE, precision = 2) {
  # alfa es un escalar entre 0 y 1 lo obtiene para ese Ãºnico valor
  # precision dice por cuántos lugares decimales alfa debe ser exactamente

  X = tablaX;
  if (favorable) {
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    search_vector = seq(0, 1, by=0.1)
    # return alpha values in between which the alternatives change with our
    # chosen precision
    Hurwicz = c()
    Alt_Hurwicz = c()
    alfas = search.alphas(search_vector, Altmax, Altmin, precision, 1)
    for (alfa in alfas){
    AltH = (1-alfa) * Altmax + alfa * Altmin
    Alt_Hurwicz = c(Alt_Hurwicz, which.max.general(AltH))
    Hurwicz = c(Hurwicz, max(AltH))
    }
    metodo = 'favorable';
    Alt_Hurwicz = c()
  } else {
    Hurwicz = c()
    Alt_Hurwicz = c()
    Altmin = apply(X,MARGIN=1,min);
    Altmax= apply(X,MARGIN=1,max);
    alfas = search.alphas(search_vector, Altmax, Altmin, precision, 1)
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
  
  return(resultados);
  
}

search.alphas = function(search_vector, Altmax, Altmin, precision, current_digits){
  # return alpha values in between which the alternatives change with our
  # chosen precision
  # parameters
  #   search vector: vector which we want to search for margin alpha values
  #   Altmax, Altmin: minimal and maximal value out of X values, computed in
  #   the above function criterio.Hurwicz
  #   precision: The amount of digits we want our alphas to have
  #   current_digits: The amount of digits we have in our current 
  #   computation (starting with 1 for 0.x in the above function criterio.Hurwicz)
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
  if (current_digits < precision){
    remember = c()
    for (i in indices){
    search_vector = seq(search_vector[i-1], search_vector[i], by=1/(10^(current_digits+1)))
    # using recursion
    result = search.alphas(search_vector, Altmax, Altmin, precision, current_digits + 1)
    remember = c(remember, result)
    }
  }
  
  return(remember)
  }



# this is solely for testing
X = matrix(c(5,4,6,2,3,1,-1,8,7,5,2,0),nrow=4,ncol=3,byrow=TRUE)

colnames(X)=c('e1','e2','e3')
rownames(X)=c('d1','d2','d3','d4')

which.min.general(c(3,2,8,2,9,2))


criterio.Hurwicz(X)

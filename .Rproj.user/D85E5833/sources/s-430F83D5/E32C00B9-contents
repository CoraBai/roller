#' @title check_sides
#' @description check if sides of the object are non-duplicated positive number
#' @param sides vector of object sides
#' @return TRUE/FALSE if the sides are legit or not

check_sides = function(sides){
  if(length(sides)<= 1){
    stop("'sides' must be a vector of length greater than 1")
  }
  if(any(duplicated(sides))== TRUE){
    stop("'sides' cannot have duplicated elements")
  }
  TRUE
}

#' @title check_prob
#' @description check if probability of the object is between 0 and 1, summed up to be 1
#' @param prob probability of sides
#' @return TRUE/FALSE if the probabilities are legit or not

check_prob = function(prob){
  if(!is.numeric(prob) | length(prob)<= 1){
    stop("\n 'prob' must be a numeric vector of length 2")
  }
  if(any(prob< 0) | any(prob> 1)){
    stop("\n'prob' values must be between 0 and 1")
  }
  if(sum(prob)!= 1){
    stop("\n elements in 'prob' must add up to 1")
  }
  TRUE
}

#' @title is.device
#' @description check if the class of input is device
#' @param x input object
#' @return TRUE/FALSE if the input are objects or not
#' @export

is.device = function(x){
  if(class(x) == "device"){
    return(TRUE)
  }
  else
    return(FALSE)
}

#' @title print.device
#' @description print device information in an organized manner
#' @param res input device
#' @return print sides and probability of device
#' @export

print.device = function(res) {
  cd = data.frame(sides = res$sides, prob = res$prob)
  cat('object "device" \n\n')
  print(cd)
  invisible(res)
}

#constuctor function
#' @title device
#' @description return information of device
#' @param sides sides info of the object
#' @param prob prob info of the object
#' @return device information, including sides and probabilities
#' @export

device = function(sides = c(1,2), prob = c(0.5, 0.5)){
  if(length(sides) != length(prob)){
    stop("'sides' and 'prob' have different lengths")
  }
  if(check_prob(prob) == TRUE && check_sides(sides) == TRUE){
    res = list(sides = sides, prob = prob)
    class(res) = "device"
    return(res)

  }
}

#fair_coin = device()
#fair_coin

#weird_die = device(sides = c('i', 'ii', 'iii', 'iv'), prob = rep(1/4, 4))
#weird_die
#is.device(weird_die)

#loaded_die = device(sides = 1:6, prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
#loaded_die
#is.device(loaded_die)

#invalid_device = device(sides = c('a'))
#bad_coin = device(sides = c('heads', 'heads'))
#bad_example = device(sides = c('a', 'b', 'c'), prob = c(0.2, 0.8))
#is.device(c(1, 2, 3))

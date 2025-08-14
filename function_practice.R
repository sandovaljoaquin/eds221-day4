# adds up the number of birds and dogs 


#define function 
birddog_sum <- function(bird, dog) {
  pets = bird + dog 
  return(pets)
}

# using function, need to store to an object 
total_pets <- birddog_sum(bird = 2, dog = 5)

#alternatively, same thing 
total_pets <- birddog_sum(2, 5)

#create a function to double values 

double_it <- function(x) {
  print(2 * x)
}


double_it(50)
#write a function with conditionals 
#example is converting animals' ages

animal_age <- function(animal, age) {
if(animal == "dog") { 
  print(age * 7)
  }else if (animal == "goat") {
    print(age * 4.7)
  }
}

# try using for an 8 year old dog 

animal_age(animal = "dog", age = 8)

#try using for a cow 

animal_age(animal = "cow", age = 8)

#write an updated version of the animal age function with error messages 

animal_age_stop <- function(animal, age){ 
  if(!animal %in% c("dog", "goat")){
    stop("Oops! Animal must be a dog or goat.")
  }
  if (is.numeric(age) == FALSE){
    stop("The age must be a number")
  }
  if(age <= 0 | age > 50) {
    warning("Are you sure about your animal's age?")
  }
}

animal_age_stop("dog", 100)





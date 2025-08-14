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

animal_age_stop("elephant", 100)

# Functions meet for loops 

# All the dataframes in the function are called df --> argument df

df_means <- function(df) {
for(i in 1: ncol(df)) {
  col_mean <- mean(df[[i]])
  col_name <- colnames(df[i])
  print(paste("The mean value of", col_name, "is", col_mean))
}
}

df_means(df = mtcars)

# to get rid of NA #####

df_means <- function(df) {
  for(i in 1: ncol(df)) {
    col_mean <- mean(df[[i]])
    col_name <- colnames(df[i])
    print(paste("The mean value of", col_name, "is", col_mean))
  }
}

df_means(df = penguins)

# Logistic growth example

#logistic growth equation 

logistic_growth <- function(N0, K, r, time) {
  Nt <- K / (1 + ((K - N0)/N0) * exp(-r * time))
  print(Nt)
}

#check for one set of values 

logistic_growth(N0 = 100, K = 6000, r = 0.27, time = 40)


# working on an example just dealing with time 

time_vec <- seq(from = 0, to = 35, by = 0.1)

# apply the logistic growth function to that vector 

pop_35 <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec)

#combining time steps and population size into dataframe
pop_time_35 <- data.frame(time_vec, pop_35)

#plot 
ggplot(data = pop_time_35, aes(x = time_vec, y = pop_35)) + geom_line()

# alternatively, with an internal for loop 
pop_35_vec <- vector(mode = "numeric", length = length(time_vec))

for(i in seq_along(time_vec)) {
population <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec[i])
pop_35_vec[i] <- population 
}

# Now building to estimate across growth rates 

#creating a series of growth rates 
r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)

#creating a matrix to store output values 
out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))


for(j in seq_along(r_seq)) {
  for(i in seq_along(time_vec)) {
    population <- logistic_growth(N0 = 100, K = 6000, 
                                  r = r_seq[j], time = time_vec[i])
    out_matrix[i,j] <- population 
  }
}

# data wrangling to plot 

#adding time as a variable
out_df <- data.frame(out_matrix, time = time_vec) 

#update column names for growth rates 

colnames(out_df) <- c(paste0("gr_", r_seq), "time")

# pivot longer to make it tidy

out_df_long <- out_df |> 
  pivot_longer(cols = -time, 
               names_to = "growth_rate",
               values_to = "population")


# ways of "massaging dataframe" into format that ggplot likes better 


ggplot(data = out_df_long, 
       aes(x = time, y = population)) + 
  geom_line(aes(color = growth_rate)) + 
  theme_minimal()






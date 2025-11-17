# Interesting resource:
#   http://adv-r.had.co.nz/memory.html#:~:text=R%20uses%20an%20alternative%20approach,object%2C%20it%20deletes%20that%20object.

# Want to bet on what takes more memory ?

vec_1_million <- rnorm(n=1e7)

binary_vec_1_million <- round(rnorm(n=1e7))

char_vec_1_million <- as.character(rnorm(n=1e7))

categ_vec_1_million <- sample(
  x=c('France', 'England', 'Spain', 'Hungary', 'Nigeria', 'Laos', 'Australia'), 
  size=1e7, 
  replace=TRUE
)

compar <- t(sapply(X=ls(), FUN=function(e){
  c(e, format(object.size(get(e)), units='Mb'))
}))
print(data.frame(compar))

### Appendix A Computing in R ###

# A.3 The basics
1/3
sqrt(2)
curve(x^2 + 5, from = -2, to = 2)

# Calling functions
a <- 3
a
b <- 10
a + b
a * b
exp(a)
10^a
log(b)
log10(b)
a^b
round(3.435, 0)
round(3.435, 1)
round(3.435, 2)

x <- c(4, 10, -1, 2.4)
x

c(1, 3, 5)
1:5
c(1:5, 1, 3, 5)
c(1:5, 10:20)
seq(-1, 9, 2)

# Sampling and random numbers
runif(1, 0, 100)
runif(50, 0, 100)

color <- c("blue", "red", "green")
sample(color, 1) 

color <- c("blue", "red", "green")
p <- c(0.5, 0.3, 0.2)
sample(color, 1, prob = p)

# Loops
for (i in 1:10) {
  print(i)
}

for (i in 1:10) {
  print(paste("hello", i))
}

for (i in 1:10) {
  number <- runif(1, 0, 100)
  color <- ifelse(number>30, "red", "blue")
  print(color)
}

# Working with vectors
x <- 1:5
y <- c(3, 4, 1, 1, 1)
z <- c("A", "B", "C")
u <- runif(5, 0, 100)

x
y
x + y
1000*x + u

1 + x
2 * x
x / 3
x^4

sum(x)
mean(x)

x <- c(100, 200, 600)
w1 <- c(1/3, 1/3, 1/3)
w2 <- c(0.5, 0.2, 0.3)
sum(w1*x)
sum(w2*x)

N <- c(310e6, 112e6, 34e6)
sum(N*x)/sum(N)

N <- c(310e6, 112e6, 34e6)
w <- N/sum(N)
sum(w*x)

a <- c(1, 1, 1, 1, 1)
cumsum(a)
a <- c(2, 4, 6, 8, 10)
cumsum(a)

# Subscripting
a <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
a[1]
a[4:6]
a[c(1, 3, 5)]
a[c(8, 1:3, 2)]

x <- c(1, 1, 1, 2, 2)
y <- c(2, 4, 6)
x[1:3] + y
x[3:5] * y
y[3] * y
y[3]^x[4]


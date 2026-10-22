x <- c(1, 0, 2, 1, 1, 0, 1, 2, 0, 0)
size <- 2
p <- 0.5

#Plot binomial 
geno <- rbinom(x, size, p)
barplot(geno, main="Binomial Distribution of genotypes")

#Observed counts from random binomial
observed <- factor(geno, levels=c(0, 1, 2))
table(observed)

#Expected counts from random binomial
expected <- c(p^2*10, 2*p*(1-p)*10, (1-p)^2*10)
table(expected, levels = c(0,1,2))

#-------------------------------------------------------------------

#GWAS Lin Reg

G_maf_p <- 0.39 #minor allele frequency
true_beta = 0.5 #effect size
beta_0 <- 0
n <- 1000
sigma <- 0.31
beta_estimate <- numeric(100)

for (j in 1:100) {
  set.seed(j)
  epsilon = rnorm(n, 0, sigma)
  genotypes = rbinom(n, 2, G_maf_p)
  table (genotypes) #just checking

  #now we generate phenotypes
  phenotypes <- numeric(1000)
  for (i in 1:n) {
    phenotypes[i] <- beta_0 + true_beta * genotypes[i] + epsilon[i]
  }

  sim <- lm (phenotypes ~ genotypes)
  summary(sim)
  beta_estimate[j] <-coef(sim)["genotypes"]
}
density_genotype <- density(beta_estimate)
plot(density_genotype)




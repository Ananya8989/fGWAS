SNP <- readline("Please enter the genotype vector: ")
phenotype <- readline("Please enter the phenotype vector: ")

SNP_vector <- as.numeric(unlist(strsplit(genotype, " ")))
pheno_vector <- as.numeric(unlist(strsplit(phenotype, " ")))
print(SNP_vector)
print(pheno_vector)
results <- lm (pheno_vector ~ SNP_vector)
beta <- coef(results)["SNP_vector"]
SE <- sqrt(diag(vcov(results)))["SNP_vector"]
p_value <- summary(results)$coefficients[,4]["SNP_vector"]
cat("The beta value is: ", beta)
cat("The SE value is: ", SE)
cat("The p-value is: ", p_value)

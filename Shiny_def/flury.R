my_flury.test <- function(covmats, nvec, B = cpc::FG(covmats = covmats, nvec = nvec)$B, p = dim(covmats)[1], qmax = p - 2, commonvec.order = findcpc(covmats = covmats, B = B, plotting = FALSE)$commonvec.order)
{

if ((qmax + 2) > p) {
  qmax <- p - 2
  model.names <- c("Equality", "Proportionality", "CPC", paste("CPC(", seq(from = qmax, to = 1), ")", sep = ""), "Heterogeneity")
  No.of.CPCs <- c(p, p, p, (p - 2):1, 0)
} else if (qmax < 1) {
  qmax <- 0
  model.names <- c("Equality", "Proportionality", "CPC", "Heterogeneity")
  No.of.CPCs <- c(p, p, p, 0)
} else {
  model.names <- c("Equality", "Proportionality", "CPC", paste("CPC(", seq(from = qmax, to = 1), ")", sep = ""), "Heterogeneity")
  No.of.CPCs <- c(p, p, p, (p - 2):1, 0)
}

nmodels <- length(model.names)
chi.square <- numeric(nmodels)
df <- numeric(nmodels)
model.AIC <- numeric(nmodels)

# Equality
equal.test.output <- equal.test(covmats, nvec)
chi.square[1] <- equal.test.output$chi.square
df[1] <- equal.test.output$df
model.AIC[1] <- flury.AIC(equal.test.output$covmats.equal, covmats, nvec, df = equal.test.output$df)

# Proportionality
prop.test.output <- cpc::prop.test(covmats, nvec)
chi.square[2] <- prop.test.output$chi.square
df[2] <- prop.test.output$df
model.AIC[2] <- flury.AIC(prop.test.output$covmats.prop, covmats, nvec, df = prop.test.output$df)

# CPC
cpc.test.output <- cpc.test(covmats = covmats, nvec = nvec, B = B)
chi.square[3] <- cpc.test.output$chi.square
df[3] <- cpc.test.output$df
model.AIC[3] <- flury.AIC(cpc.test.output$covmats.cpc, covmats, nvec, df = cpc.test.output$df)

# CPC(q)
if (qmax > 0) {
  B <- B[, commonvec.order]
  q <- qmax
  for (i in 1:qmax) {
    cpcq.test.output <- cpcq.test(covmats, nvec, B, q = q)
    chi.square[3 + i] <- cpcq.test.output$chi.square
    df[3 + i] <- cpcq.test.output$df
    model.AIC[3 + i] <- flury.AIC(cpcq.test.output$covmats.cpcq, covmats, nvec, df = cpcq.test.output$df)
    q <- q - 1
  }
}

# Heterogeneity
model.AIC[3 + qmax + 1] <- flury.AIC(covmats, covmats, nvec, df = 0)

chi.square[1:(nmodels - 2)] <- chi.square[1:(nmodels - 2)] - chi.square[2:(nmodels - 1)]
df[1:(nmodels - 2)] <- df[1:(nmodels - 2)] - df[2:(nmodels - 1)]
chi.div.df <- chi.square / df
chi.square <- round(chi.square, 2)
chi.div.df <- round(chi.div.df, 2)
model.AIC <- round(model.AIC, 2)

resultmat <- list(Model = model.names, Chi.square = chi.square, DF = df, Chi2.div.df = chi.div.df, AIC = model.AIC, No.of.CPCs = No.of.CPCs)
}
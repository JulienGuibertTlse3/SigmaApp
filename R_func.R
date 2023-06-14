K.linear = function(x1,x2=x1){
  KL <- as.matrix(x1)%*%(t(x2))*(1/length(x1))
  
  # Assuming KL is a numeric matrix
  diag_vals <- diag(KL)
  
  # Ensure diagonal values are non-negative
  diag_vals <- pmax(diag_vals, 0)
  
  # Ensure KL values are non-negative
  KL <- pmax(KL, 0)
  
  # Calculate the outer product of the diagonal values
  outer_product <- outer(diag_vals, diag_vals, pmax)
  
  # Perform normalization by dividing KL by the outer product
  KL <- KL / outer_product
  
  KL <- ifelse(KL<0.0000001, 0, (KL - min(KL)) / (max(KL) - min(KL)))
  
  KL
}


K.Polynomial=function(x1, x2=x1, gamma=1/length(x1), b=1, d=3){
  PL <- (gamma*(as.matrix(x1)%*%t(x2))+b)^d
  
  # Assuming KL is a numeric matrix
  diag_vals <- diag(PL)
  
  # Ensure diagonal values are non-negative
  diag_vals <- pmax(diag_vals, 0)
  
  # Ensure PL values are non-negative
  PL <- pmax(PL, 0)
  
  # Calculate the outer product of the diagonal values
  outer_product <- outer(diag_vals, diag_vals, pmax)
  
  # Perform normalization by dividing PL by the outer product
  PL <- PL / outer_product
  
  PL <- ifelse(PL<0.0000001, 0, (PL - min(PL)) / (max(PL) - min(PL)))
  
  PL
}


l2norm=function(x){sqrt(sum(x^2))}

K.Gaussian=function(x1,x2=x1, gamma=1/length(x1)){
  gauss<-exp(-gamma*outer(1:nrow(x1<- as.matrix(x1)), 1:ncol(x2<- t(x2)),
                   Vectorize(function(i, j) l2norm(x1[i,]-x2[,j])^2)))
  colnames(gauss)<-rownames(x1)
  rownames(gauss)<-rownames(x1)
  
  # Assuming KL is a numeric matrix
  diag_vals <- diag(gauss)
  
  # Ensure diagonal values are non-negative
  diag_vals <- pmax(diag_vals, 0)
  
  # Ensure gauss values are non-negative
  gauss <- pmax(gauss, 0)
  
  # Calculate the outer product of the diagonal values
  outer_product <- outer(diag_vals, diag_vals, pmax)
  
  # Perform normalization by dividing gauss by the outer product
  gauss <- gauss / outer_product
  
  gauss <- ifelse(gauss<0.0000001, 0, (gauss - min(gauss)) / (max(gauss) - min(gauss)))

  return(gauss)
}

K.AK1_Final<-function(x1,x2=x1){
  n1<-nrow(x1)
  n2<-nrow(x2)
  x1tx2<-as.matrix(x1)%*%t(x2)
  norm1<-sqrt(apply(x1,1,function(x) crossprod(x)))
  norm2<-sqrt(apply(x2,1,function(x) crossprod(x)))
  costheta = diag(1/norm1)%*%x1tx2%*%diag(1/norm2)
  costheta[which(abs(costheta)>1,arr.ind = TRUE)] = 1
  theta<-acos(costheta)
  normxy<-as.matrix(norm1)%*%t(norm2)
  J = (sin(theta)+(pi-theta)*cos(theta))
  AK1 = 1/pi*normxy*J
  AK1<-AK1/median(AK1)
  colnames(AK1)<-rownames(x2)
  rownames(AK1)<-rownames(x1)
  
  # Assuming KL is a numeric matrix
  diag_vals <- diag(AK1)
  
  # Ensure diagonal values are non-negative
  diag_vals <- pmax(diag_vals, 0)
  
  # Ensure AK1 values are non-negative
  AK1 <- pmax(AK1, 0)
  
  # Calculate the outer product of the diagonal values
  outer_product <- outer(diag_vals, diag_vals, pmax)
  
  # Perform normalization by dividing AK1 by the outer product
  AK1 <- AK1 / outer_product
  
  AK1 <- ifelse(AK1<0.0000001, 0, (AK1 - min(AK1)) / (max(AK1) - min(AK1)))
  
  AK1
  
  return(AK1)
}

MDS_fnc <- function(x1){
  matrix_E <- vegdist(x1, method="bray")
  MDS_mat<-pcoa(matrix_E)
  vec_mds<-as.data.frame(MDS_mat$vectors)
  dat_Mds <- K.linear((vec_mds))
  colnames(dat_Mds)<-rownames(x1)
  rownames(dat_Mds)<-rownames(x1)
  
  # Assuming KL is a numeric matrix
  diag_vals <- diag(dat_Mds)
  
  # Ensure diagonal values are non-negative
  diag_vals <- pmax(diag_vals, 0)
  
  # Ensure dat_Mds values are non-negative
  dat_Mds <- pmax(dat_Mds, 0)
  
  # Calculate the outer product of the diagonal values
  outer_product <- outer(diag_vals, diag_vals, pmax)
  
  # Perform normalization by dividing dat_Mds by the outer product
  dat_Mds <- dat_Mds / outer_product
  
  dat_Mds <- ifelse(dat_Mds<0.0000001, 0, (dat_Mds - min(dat_Mds)) / (max(dat_Mds) - min(dat_Mds)))
  
  dat_Mds
  
  
  dat_Mds
}

DCA_fnc <- function(x1){
  matrix_E <- vegdist(x1, method="bray")
  dca_mat <- decorana(matrix_E)
  proj_dca <- as.data.frame(dca_mat$rproj)
  dat_DCA <- K.linear((proj_dca))
  colnames(dat_DCA)<-rownames(x1)
  rownames(dat_DCA)<-rownames(x1)
  
  # Assuming KL is a numeric matrix
  diag_vals <- diag(dat_DCA)
  
  # Ensure diagonal values are non-negative
  diag_vals <- pmax(diag_vals, 0)
  
  # Ensure dat_DCA values are non-negative
  dat_DCA <- pmax(dat_DCA, 0)
  
  # Calculate the outer product of the diagonal values
  outer_product <- outer(diag_vals, diag_vals, pmax)
  
  # Perform normalization by dividing dat_DCA by the outer product
  dat_DCA <- dat_DCA / outer_product
  
  dat_DCA <- ifelse(dat_DCA<0.0000001, 0, (dat_DCA - min(dat_DCA)) / (max(dat_DCA) - min(dat_DCA)))
  
  dat_DCA
}

Euc_fnc <- function(x1){
  matrix_E <- vegdist(x1, method="euclidean")
  matrix_E <- as.matrix(matrix_E)
  
  # Calculate the interquartile range for each column
  iqr <- apply(matrix_E, 2, IQR)
  
  # Define a threshold to determine outliers (e.g., 1.5 times the IQR)
  threshold <- 1.5
  
  # Identify the outliers in each column
  outliers <- matrix_E > (quantile(matrix_E, 0.75) + threshold * iqr) | matrix_E < (quantile(matrix_E, 0.25) - threshold * iqr)
  
  # Replace the outliers with appropriate values (e.g., the maximum distance)
  max_distance <- max(matrix_E[!outliers], na.rm = TRUE)
  matrix_E[outliers] <- max_distance
  matrix_E <- (matrix_E - min(matrix_E)) / (max(matrix_E) - min(matrix_E))
  
  diag(matrix_E) <- 1
  
  matrix_E
}

PLN_fnc <- function(x1){
  
  x1 <- as.matrix(x1)
  x1 <- t(x1)
  x1 <- as.data.frame(x1)
  x1$cov <- "2"
  cov_tr <- as.data.frame(x1$cov)
  rownames(cov_tr) <- rownames(x1)
  x1<-x1[,!names(x1) %in% c("cov")]
  
  pln_data <- PLNmodels::prepare_data(x1[,1:length(x1)], cov_tr)
  myPLN <- PLN(Abundance~1, data=pln_data)
  PLN_res <- myPLN %>% sigma() %>% cov2cor() 
  
  PLN_res <- ifelse(PLN_res<0.0000001, 0, (PLN_res - min(PLN_res)) / (max(PLN_res) - min(PLN_res)))
  
  PLN_res
}


BC_fnc <- function(x1){
  matrix_E <- vegdist(x1, method="bray")
  matrix_E <- as.matrix(matrix_E)
  matrix_E <- 1-matrix_E
  diag(matrix_E) <- 1
  matrix_E
}


JC_fnc <- function(x1){
  matrix_E <- vegdist(x1, method="jaccard", binary = T)
  matrix_E <- as.matrix(matrix_E)
  matrix_E <- 1-matrix_E
  diag(matrix_E) <- 1
  matrix_E
}


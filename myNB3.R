## input: i) feature matrix X: each column is a feature and each row is an instance
#         ii) label vector y

X <- subset(train.ds, select=c("campaign.id",  "placement.id"))
num.feat <- ncol(X)

y <- train.ds$ex.info
classes <- c("[e2]", "[e10]")

train <- function(X.train, y.train) {
  cat("start training \n")
  table(y.train)/sum(table(y.train)) -> priors
  
  getCondProbs <- function(feat) {
    
    cat("estimating cond. probs for feature", feat, "...\n")
    cond.probs <- laply(classes, function(c) {
      
      table(X.train[y.train == c, feat]) -> freq
      ## laplace smoothing
      ## assign freq 1 to those values of the feature which do not occur in instances of class c
      unique(X[ , feat]) -> feat.lvl
      freq <- ifelse(feat.lvl %in% names(freq), freq[feat.lvl]+1, 1)
      names(freq) <- feat.lvl
      freq/sum(freq) -> cond.prob
      cond.prob
    })
    t(cond.probs)
  }
  
  features <- 1:ncol(X)
  llply(features, getCondProbs) -> cond.probs
  list(priors = priors, cond.probs = cond.probs)
}

## funcs for validating and predicting ==================================================
condProbMat <- function(c, X, cond.probs) {
  cat("querying cond probs given class", c, "...\n")
  mat <- laply(1:num.feat, function(feat) {
    cond.probs[[feat]][ X[ , feat], c]
  })
  t(mat)
}

likelihood <- function(X, c, priors, cond.probs) {
  cat("estimating likelihoods being in class", c, "of instances in validation set ...\n")
  apply(condProbMat(c, X, cond.probs), 1, prod) * priors[classes[c]]
} 

predict <- function(X, priors, cond.probs) {
  likelihood.mat <- laply(1:length(classes), function(c) likelihood(X,c, priors, cond.probs))
  classes[apply(likelihood.mat, 2, which.max)]
}

validate <- function(priors, cond.probs, X.valid, y.valid) {
  cat("start validating\n")
  predict(X.valid, priors, cond.probs) -> pred
  table(pred, y.valid) -> contingency.tab
  error.1 <- contingency.tab[2, 1]/sum(contingency.tab[,1])
  error.2 <- contingency.tab[1, 2]/sum(contingency.tab[,2])
  data.frame("error.type.1" = error.1, "error.type.2" = error.2)
}
#================================================================================
CV <- function(k, X, y) {
  num.instance <- nrow(X)
  fold.size <- floor(num.instance/k)
  
  runCV <- function(i) {
    cat("start CV using fold", i,"\n")
    start <- (i-1)*fold.size + 1
    if (i < k) {end <- (i-1)*fold.size + fold.size} else {end <- num.instance}
    idx <- start:end
    X.train <- as.data.frame(X[-idx, ])
    y.train <- y[-idx]
    X.valid <- as.data.frame(X[idx, ])
    y.valid <- y[idx]
    
    train(X.train, y.train) -> model
    validate(model$priors, model$cond.probs, X.valid, y.valid) -> res
    cat("CV by fold ", i, "done\n")
    res
  }
#   k <- 10
  ldply(1:k, runCV, .progress="time") -> res
}

CV(k=10, X, y) -> res.campaign.placement

features.used <- c("adv.id and pub.id", "adv.id and campaign.id", 
                   "adv.id and placement.id", "campaign.id and placement.id",
                   "adv.id, campaign.id and placement.id")

med.type.1 <- c(median(res.adv.and.pub$error.type.1), median(res.adv.and.campaign$error.type.1),
                median(res.adv.placement$error.type.1), median(res.camp.placement$error.type.1),
                median(res.adv.campaign.placement$error.type.1))

med.type.2 <- c(median(res.adv.and.pub$error.type.2), median(res.adv.and.campaign$error.type.2),
                median(res.adv.placement$error.type.2), median(res.camp.placement$error.type.2),
                median(res.adv.campaign.placement$error.type.2))

report <- data.frame("features used" = features.used, 
                     "type.1.ratio" = med.type.1,
                     "type.2.ratio" = med.type.2)

report <- report[order(report$type.1.ratio), ]
write.csv(report, file="report.csv", row.names=F)

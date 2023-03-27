### mda

#<<classification_study>>=
library(MASS)
library(mvtnorm)
library(mda)
library(ggplot2)

set.seed(42)
n <- 500

# Randomly sample data
x11 <- rmvnorm(n = n, mean = c(-4, -4))
x12 <- rmvnorm(n = n, mean = c(0, 4))
x13 <- rmvnorm(n = n, mean = c(4, -4))

x21 <- rmvnorm(n = n, mean = c(-4, 4))
x22 <- rmvnorm(n = n, mean = c(4, 4))
x23 <- rmvnorm(n = n, mean = c(0, 0))

x31 <- rmvnorm(n = n, mean = c(-4, 0))
x32 <- rmvnorm(n = n, mean = c(0, -4))
x33 <- rmvnorm(n = n, mean = c(4, 0))

x <- rbind(x11, x12, x13, x21, x22, x23, x31, x32, x33)
train_data <- data.frame(x, y = gl(3, 3 * n))

# Trains classifiers
lda_out <- lda(y ~ ., data = train_data)
qda_out <- qda(y ~ ., data = train_data)
mda_out <- mda(y ~ ., data = train_data)

# Generates test data that will be used to generate the decision boundaries via
# contours
contour_data <- expand.grid(X1 = seq(-8, 8, length = 300),
                            X2 = seq(-8, 8, length = 300))

# Classifies the test data
lda_predict <- data.frame(contour_data,
                          y = as.numeric(predict(lda_out, contour_data)$class))
qda_predict <- data.frame(contour_data,
                          y = as.numeric(predict(qda_out, contour_data)$class))
mda_predict <- data.frame(contour_data,
                          y = as.numeric(predict(mda_out, contour_data)))


Welcome!
  Here you will find daily news and tutorials about R, contributed by over 750 bloggers.
There are many ways to follow us -
  By e-mail:
  On Facebook:
  If you are an R blogger yourself you are invited to add your own R content feed to this site (Non-English R bloggers should add themselves- here)
RSS Jobs for R-users

R Developer
postdoc in psychiatry: machine learning in human genomics
Lead Quantitative Developer
Research Data Analyst @ Arlington, Virginia, U.S.
Market Research Analyst: Mobility for RSG

Popular Searches
Recent Posts

Why R 2018 Winners
Extracting a Reference Grid of your Data for Machine Learning Models Visualization
#19: Intel MKL in Debian / Ubuntu follow-up
Statistics Sunday: Converting Between Effect Sizes for Meta-Analysis
Let R/Python send messages when the algorithms are done training
Forecasting my weight with R
A useful forecast combination benchmark
A primer in using Java from R - part 1
future.apply - Parallelize Any Base R Apply Function
Thanks for Reading!
  A guide to working with character data in R
Using DataCamp's Autograder to Teach R
Melt and cast the shape of your data.frame - Exercises
Creating Slopegraphs with R
Parallelizing Linear Regression or Using Multiple Sources

Other sites

Jobs for R-users
SAS blogs

A Brief Look at Mixture Discriminant Analysis
July 2, 2013
By John Ramey

inShare
(This article was first published on Category: R | John Ramey, and kindly contributed to R-bloggers)

Share
Tweet

Lately, I have been working with finite mixture models for my postdoctoral work
on data-driven automated gating.
Given that I had barely scratched the surface with mixture models in the
classroom, I am becoming increasingly comfortable with them. With this in mind,
I wanted to explore their application to classification because there are times
when a single class is clearly made up of multiple subclasses that are not
necessarily adjacent.

As far as I am aware, there are two main approaches (there are lots and lots of
                                                     variants!) to applying finite mixture models to classfication:
  
  The Fraley and Raftery approach via the mclust R package

The Hastie and Tibshirani approach via the mda R package

Although the methods are similar, I opted for exploring the latter method. Here
is the general idea. There are classes, and each class is assumed to
be a Gaussian mixuture of subclasses. Hence, the model formulation is generative,
and the posterior probability of class membership is used to classify an
unlabeled observation. Each subclass is assumed to have its own mean vector, but
all subclasses share the same covariance matrix for model parsimony. The model
parameters are estimated via the EM algorithm.

Because the details of the likelihood in the paper are brief, I realized I was a
bit confused with how to write the likelihood in order to determine how much
each observation contributes to estimating the common covariance matrix in the
M-step of the EM algorithm. Had each subclass had its own covariance matrix, the
likelihood would simply be the product of the individual class likelihoods and
would have been straightforward. The source of my confusion was how to write
the complete data likelihood when the classes share parameters.

I decided to write up a document that explicitly defined the likelihood and
provided the details of the EM algorithm used to estimate the model parameters.
The document is available here
along with the LaTeX and R code.
If you are inclined to read the document, please let me know if any notation is
confusing or poorly defined. Note that I did not include the additional topics
on reduced-rank discrimination and shrinkage.

To see how well the mixture discriminant analysis (MDA) model worked, I
constructed a simple toy example consisting of 3 bivariate classes each having 3
subclasses. The subclasses were placed so that within a class, no subclass is
adjacent. The result is that no class is Gaussian. I was interested in seeing
if the MDA classifier could identify the subclasses and also comparing its
decision boundaries with those of linear discriminant analysis (LDA)
and quadratic discriminant analysis (QDA).
I used the implementation of the LDA and QDA classifiers in the MASS package.
From the scatterplots and decision boundaries given below,
the LDA and QDA classifiers yielded puzzling decision boundaries as expected.
Contrarily, we can see that the MDA classifier does a good job of identifying
the subclasses. It is important to note that all subclasses in this example have
the same covariance matrix, which caters to the assumption employed in the MDA
classifier. It would be interesting to see how sensitive the classifier is to
deviations from this assumption. Moreover, perhaps a more important investigation
would be to determine how well the MDA classifier performs as the feature
dimension increases relative to the sample size.

LDA Decision Boundaries

QDA Decision Boundaries

MDA Decision Boundaries

"` r Comparison of LDA, QDA, and MDA
library(MASS)
library(mvtnorm)
library(mda)
library(ggplot2)

set.seed(42)
n <- 500
Randomly sample data

x11 <- rmvnorm(n = n, mean = c(-4, -4))
x12 <- rmvnorm(n = n, mean = c(0, 4))
x13 <- rmvnorm(n = n, mean = c(4, -4))

x21 <- rmvnorm(n = n, mean = c(-4, 4))
x22 <- rmvnorm(n = n, mean = c(4, 4))
x23 <- rmvnorm(n = n, mean = c(0, 0))

x31 <- rmvnorm(n = n, mean = c(-4, 0))
x32 <- rmvnorm(n = n, mean = c(0, -4))
x33 <- rmvnorm(n = n, mean = c(4, 0))

x <- rbind(x11, x12, x13, x21, x22, x23, x31, x32, x33)
train_data <- data.frame(x, y = gl(3, 3 * n))
Trains classifiers

lda_out <- lda(y ~ ., data = train_data)
qda_out <- qda(y ~ ., data = train_data)
mda_out <- mda(y ~ ., data = train_data)
Generates test data that will be used to generate the decision boundaries via

# contours
contour_data <- expand.grid(X1 = seq(-8, 8, length = 300),
                            X2 = seq(-8, 8, length = 300))
Classifies the test data

lda_predict <- data.frame(contour_data,
                          y = as.numeric(predict(lda_out, contour_data)$class))
qda_predict <- data.frame(contour_data,
                          y = as.numeric(predict(qda_out, contour_data)$class))
mda_predict <- data.frame(contour_data,
                          y = as.numeric(predict(mda_out, contour_data)))
## Generates plots

p <- ggplot(train_data, aes(x = X1, y = X2, color = y)) + geom_point()
p + stat_contour(aes(x = X1, y = X2, z = y), data = lda_predict)
+ ggtitle("LDA Decision Boundaries")
p + stat_contour(aes(x = X1, y = X2, z = y), data = qda_predict)
+ ggtitle("QDA Decision Boundaries")
p + stat_contour(aes(x = X1, y = X2, z = y), data = mda_predict)
+ ggtitle("MDA Decision Boundaries")



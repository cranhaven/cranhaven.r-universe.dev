
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simpleMLP

<!-- badges: start -->
<!-- badges: end -->

[simpleMLP](https://CRAN.R-project.org/package=simpleMLP) is an
implementation of a multilayer perceptron, a type of feedforward, fully
connected neural network. It features 2 ReLU hidden layers and supports
hyperparameter tuning for learning rate, batch size, epochs, and hidden
units for both layers.

simpleMLP also allows you to directly load the MNIST database of
handwritten digits to quickly start training models.

## Neural Networks and Multilayer Perceptrons

Neural networks are a popular type of machine learning model used for
object recognition, natural language processing, and more. It uses a
composition of vector valued functions called the layers of the network
and are composed of individual units, also known as neurons. Inputs are
fed through the first layer, or the input layer, and travel through one
or more hidden layers before ending at the output layer. Each layer uses
a linear or nonlinear activation function on the input before outputting
it to the next layer. By using many layers of vector functions, neural
networks can generalize to approximate an arbitrary function f\*.

A multilayer perceptron is a type of neural network in which inputs pass
only in the direction of input to output without feeding back into
itself and each layer is fully connected to its following layer. This
means that every unit in one layer connects to every unit in the next
layer. This implementation includes two hidden layers which both use the
Rectified Linear Unit (ReLU) function, a popular nonlinear activation
function. It implements the backpropagation algorithm which trains the
network by computing gradients of its functions to minimize a specified
cost function. We use cross entropy loss as our cost function to
determine the performance of the model.

This model supports hyperparameter tuning. It is worth trying different
combinations of hyperparameters to observe how they affect overfitting
and underfitting. For more information, the following resources might be
helpful:

-   Ian Goodfellow, Yoshua Bengio, & Aaron Courville (2016). Deep
    Learning. MIT Press.
-   [Coursera Machine
    Learning](https://www.coursera.org/learn/machine-learning)

## Installation

You can install the released version of simpleMLP from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("simpleMLP")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Cullenpu/simpleMLP")
```

## Example

The training data must be a design matrix (i.e. each row is an
observation) and the targets must be a matrix of one-hot encodings.

To load the MNIST database, run the following:

``` r
library(simpleMLP)

mnist <- load_mnist()

train_data <- mnist[1]
train_target <- mnist[2]

validate_data <- mnist[3]
validate_target <- mnist[4]

test_data <- mnist[5]
test_target <- mnist[6]
```

After loading the dataset, the model can be trained like so:

``` r
# Initialize the network model. The data has dimension 784, there are
# 10 classes, and we choose 100 and 50 hidden units for the first and second
# hidden layers.
mlp_model <- init_nn(784, 100, 50, 10)

# Now we train the model. We decide on the following hyperparameters. Overall,
# the hyperparameters should be tuned as to avoid overfitting. We also plot the
# test cross entropy and accuracy to see how it changes over time.
alpha <- 0.01
epochs <- 1
batch_size <- 64
plot_acc <- TRUE

mlp_model <- train_nn(train_data, train_target, validate_data, validate_target,
                      mlp_model, alpha, epochs, batch_size, plot_acc)

# To see how well the model performs, we evaluate it on the test data.
evaluate(test_data, test_target, mlp_model)
```

Using the given hyperparameters, a final test accuracy of `0.955723` was
achieved, with the accuracy plot shown below.

![Accuracy Plot](examples/example_plot.png)

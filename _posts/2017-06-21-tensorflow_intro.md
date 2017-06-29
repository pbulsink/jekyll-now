---
title: "TensorFlow and R for NLP"
author: "Phil Bulsink"
date: "2017-06-21"
output: html_document
status: publish
published: true
layout: post
excerpt_separator: <!--more-->
maps: true
tags: R NLP RNN LSTM TensorFlow
---
 

 
Those of you who are interested in machine learning will likely have heard of Google's [TensorFlow](https://www.tensorflow.org/). While R is not officially supported, RStudio has developed a wrapper to be able to use TensorFlow in R. More information, and a few tutorials, are available on [the website](https://tensorflow.rstudio.com/), but I'll add to that list with some Natural Language Processing (NLP) examples, since they seem to not be overly abundant online.
 
<!--more-->
 
## Installation 
 
To install tensorflow, ensure you have a working copy of Python available. They recommend [Anaconda](https://www.continuum.io/downloads), but any distribution should work.
 
To insall, first get the package from GitHub:

{% highlight r %}
devtools::install_github("rstudio/tensorflow")
{% endhighlight %}
 
Then install TensorFlow using the package:

{% highlight r %}
library(tensorflow)
install_tensorflow()
{% endhighlight %}
 
Finally, test your installation:

{% highlight r %}
sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)
{% endhighlight %}



{% highlight text %}
## b'Hello, TensorFlow!'
{% endhighlight %}
 
There is a GPU version of TensorFlow available, if you have NVIDIA graphics cards. More information on this and other installation options are available on the [tensorflow site](https://tensorflow.rstudio.com/installation.html). 
 
## Example
 
We'll mirror the work done [in this blog post](https://medium.com/towards-data-science/lstm-by-example-using-tensorflow-feb0c1968537) from *Towards Data Science* and Rowel Atienza, as it's a good simple example. The post contains a number of graphics and a very good explanation of the model itself, but in short we are using an Long Short Term Memory (LSTM) model of a recurrant neural network (RNN) for next word prediction. 
 
The text used in the post and here is one of Aesop's fables, cleaned and shown below:
 

{% highlight r %}
text<-"long ago , the mice had a general council to consider what measures they could take to outwit their common enemy , the cat . some said this , and some said that but at last a young mouse got up and said he had a proposal to make , which he thought would meet the case . you will all agree , said he , that our chief danger consists in the sly and treacherous manner in which the enemy approaches us . now , if we could receive some signal of her approach , we could easily escape from her . i venture , therefore , to propose that a small bell be procured , and attached by a ribbon round the neck of the cat . by this means we should always know when she was about , and could easily retire while she was in the neighbourhood . this proposal met with general applause , until an old mouse got up and said that is all very well , but who is to bell the cat ? the mice looked at one another and nobody spoke . then the old mouse said it is easy to propose impossible remedies ."
{% endhighlight %}
 
### Text Preprocessing
 
The first thing to do is convert the text to a form that the model can read. Models can only operate on numerical systems, so each word and punctuation needs to be swapped with a number, producing a list (vector) of numbers instead of words representing the text. The list of words and their number will be referred to as a dictionary. We'll sort it to make it easier to comprehend as a regular paper dictionary.
 

{% highlight r %}
text <- unlist(strsplit(text, ' ', fixed=TRUE))
dictionary <- sort(unique(text))
#simple transformation to vector, not efficient, for clairty
text_v <- rep(0, length(text))
for(i in 1:length(dictionary)){
  text_v[which(text == dictionary[[i]])]<-i
}
{% endhighlight %}
 
Now, we can see that what was originally 'long ago , the mice had a general council to ...' is now coded as '49, 6, 1, 86, 57, 38, 4, 36, 29, 93'. While that's less human readable, it's easier for the model to understand.
 
### Setting up the RNN
The first thing to do is set up the neural network function iteself. Then, similar to the blog post, we'll initialize some variables and parameters:

{% highlight r %}
#parameters
learning_rate<- 0.001
training_iters <- 10000L
display_step <- 1000L
n_input <- 3L
n_hidden <- 512L
n_steps<-10L
 
#Variables
vocab_size <- length(dictionary)
 
#TensorFlow input
x <- tf$placeholder(tf$float32, shape(NULL, n_input))
y <- tf$placeholder(tf$float32, shape(NULL, vocab_size))
 
#Weights and Biases for RNN
weights <- tf$Variable(tf$random_normal(shape(n_hidden, vocab_size)))
biases <- tf$Variable(tf$random_normal(shape(vocab_size)))
 
RNN<-function(x, weights, biases){
 
  x <- tf$reshape(x, c(-1L, n_input))
  x <- tf$split(x, n_input, 1L)
  
  rnn_cell<-tf$nn$rnn_cell$MultiRNNCell(list(tf$nn$rnn_cell$BasicLSTMCell(n_hidden), tf$nn$rnn_cell$BasicLSTMCell(n_hidden)))
  
  outputs<-tf$nn$static_rnn(cell = rnn_cell, inputs = x, dtype = tf$float32)
  
  return(tf$matmul(outputs[[1]][[3]], weights) + biases)
}
 
pred<-RNN(x, weights, biases)
 
 
 
cost<-tf$reduce_mean(tf$nn$softmax_cross_entropy_with_logits(logits = pred, labels = y))
optimizer <- tf$train$RMSPropOptimizer(learning_rate=learning_rate)$minimize(cost)
{% endhighlight %}
 
We need to know what the correct prediction is, so we prepare that and a measure of accuracy:

{% highlight r %}
correct_pred <- tf$equal(tf$argmax(pred, 1L), tf$argmax(y, 1L))
accuracy <- tf$reduce_mean(tf$cast(correct_pred, tf$float32))
{% endhighlight %}
 
## Train the Model
 
With all of our variables and parameters prepared, we can initialize tensor flow, then start a Session.
 

{% highlight r %}
init <- tf$global_variables_initializer()
 
sess <- tf$Session()
sess$run(init)
{% endhighlight %}
 
Now to train our model:
 

{% highlight r %}
step <- 0
 
batch_start <- sample(1:(length(text_v)-n_input-n_steps), 1)
 
loss_total <- 0
acc_total <- 0
 
tic()
 
while(step < training_iters){
  batchx<-t(as.matrix(sapply(c(0:(n_steps-1)), function(x) text_v[(batch_start+x):(batch_start+x+n_input-1)])))
  batchy<-matrix(0, 10, 112)
  hoty<-sapply(c(0:(n_steps-1)), function(x) text_v[(batch_start+x+n_input)])
  for(i in 1:n_steps){
    batchy[i,hoty[i]]<-1
  }
  runs<-sess$run(c(optimizer, accuracy, cost, pred), feed_dict = dict(x = batchx, y=batchy))
  acc<-runs[[2]]
  loss<-runs[[3]]
  p<-runs[[4]]
  
  loss_total <- loss_total + loss
  acc_total <- acc_total + acc
  
  if(step %% display_step == 0){
    message(paste0("Step=", step, ", Avg. Loss=",round(loss_total/display_step, digits=4),
                   ", Avg. Acc.=", round(acc_total/display_step, digits=4),
                   ", [", dictionary[which.max(batchy[n_steps,])],
                   "] vs. [", dictionary[which.max(p[n_steps,])], "]\n"))
    acc_total<-0
    loss_total<-0
  }
  step <- step + 1
  batch_start <- sample(1:(length(text_v)-n_input-n_steps), 1)
}
{% endhighlight %}



{% highlight text %}
## Step=0, Avg. Loss=0.0063, Avg. Acc.=1e-04, [very] vs. [said]
## Step=1000, Avg. Loss=2.7074, Avg. Acc.=0.3077, [in] vs. [from]
## Step=2000, Avg. Loss=0.7323, Avg. Acc.=0.7568, [is] vs. [is]
## Step=3000, Avg. Loss=0.3445, Avg. Acc.=0.8811, [from] vs. [her]
## Step=4000, Avg. Loss=0.2218, Avg. Acc.=0.9214, [.] vs. [.]
## Step=5000, Avg. Loss=0.1741, Avg. Acc.=0.9347, [and] vs. [and]
## Step=6000, Avg. Loss=0.1361, Avg. Acc.=0.9462, [approach] vs. [approach]
## Step=7000, Avg. Loss=0.1219, Avg. Acc.=0.9535, [take] vs. [take]
## Step=8000, Avg. Loss=0.1074, Avg. Acc.=0.9589, [she] vs. [she]
## Step=9000, Avg. Loss=0.0958, Avg. Acc.=0.9633, [but] vs. [but]

## Optimization Finished
## 342.64 sec elapsed
{% endhighlight %}
 
 
## Play with the model
 
What can we do with this? We can use the trained model to generate text for us. This generation might seem familiar to the supplied text, or even repeat it, because of the small amount of input data. However, with a larger input, we could get some really novel speech patterns out.
 
If we supply some starting words, we'll use it to generate a few sentences.

{% highlight r %}
length_of_sentence<- 30
 
input_text<-"mouse mouse mouse"
input_text <- unlist(strsplit(input_text, ' ', fixed=TRUE))
input_v <- rep(0, length(input_text))
for(i in 1:length(dictionary)){
  input_v[which(input_text == dictionary[[i]])]<-i
}
 
sent_v<-input_v
 
 
for(i in 1:length_of_sentence){
    batchx<-matrix(input_v, nrow = 1)
    p<-which.max(sess$run(pred, feed_dict = dict(x = batchx)))
    sent_v<-c(sent_v, p)
    input_v<-c(input_v, p)[2:4]
}
 
sent_w<-rep("", length(sent_v))
for(i in 1:length(sent_w)){
    sent_w[i]<-dictionary[sent_v[i]]
}
 
paste(sent_w, collapse = " ")
{% endhighlight %}



{% highlight text %}
## [1] "mouse mouse mouse it up the and treacherous manner in which the enemy approaches us . now , if we could receive some signal of her approach , we could easily escape from"
{% endhighlight %}
Obviously, the story is just recycled to us; we're overfit to the data. Next post we'll look at a larger corpus of text as a starting point.

library(keras)
library(reticulate)
library(readr)
library(stringr)
library(stringi)
library(tokenizers)

np <- reticulate::import("numpy")

message('Setting Parameters')
#PARAMETERS:
vocab_size<-5000
firstword<-'START'
lastword<-'END'
text_path<-"C:/Users/pbulsink/Documents/GitHub/coursera_capstone/data/en_US/en_US.blogs.txt"
filters<-"\"#$%()*+-/<=>@[\\]^_`{|}~\t\n!?."
maxlength<-100
nodes<-256
embedding_vector<-100
lr<-0.01
batch_size <- 128
epochs <- 25

message('Prepping Text Data')
#Text Preparation
strings<-readr::read_lines(text_path)

strings<-iconv(strings, "latin1", 'ascii', sub = "")

sentences<-unlist(tokenizers::tokenize_sentences(strings, lowercase = TRUE, strip_punctuation = TRUE))
rm(strings)

sentences <- paste(firstword, sentences)

ttokenizer <- text_tokenizer(num_words=vocab_size-1, lower=FALSE, split = ' ', char_level = FALSE, filters = filters)
fit_text_tokenizer(ttokenizer, sentences)

sequences<-ttokenizer$texts_to_sequences(sentences)

#Sequences with length 1 have only START and no other known words in the vocab_size, can be removed
sequences<-sequences[sapply(sequences, length) > 1]

#something is wrong here if no iconv above.
word2id<-ttokenizer$word_index

#Some data stats.
message(paste0('Number of sentences: ', length(sentences)))
message(paste0('Number of unique Words: ', length(word2id)))
message(paste0('First Sentence: ', sentences[1]))
message(paste0('First Sequence: ', sequences[1]))

max_sequence_length<-max(sapply(sequences, length))
message(paste0('Longest Sentence: ', max_sequence_length))

#making sure we don't have a single runaway long sentence messing everything up.
if(max_sequence_length > maxlength){
  max_sequence_length<-maxlength
}

data<-pad_sequences(sequences, maxlen=max_sequence_length+1, padding = 'post', truncating = 'post') +1

rm(sequences, ttokenizer)
gc(verbose = FALSE)

#Add END to word2id.
word2id<-lapply(word2id, function(x) x+1)
word2id<-c(1, word2id)
names(word2id)[1]<-'END'

message(paste0('Data Size: ', dim(data)[1]," x ", dim(data)[2] ))

#Build the model. 
message('Building Model')
# Note that models are typically built (in R) using keras_model_sequential %>% ... 
# but this worked better with the paper.
words <- layer_input(batch_shape = c(batch_size, max_sequence_length), name = "input")
embeddings <- layer_embedding(words, input_dim = vocab_size, output_dim = embedding_vector, name = 'embeddings')
hidden_layer<-layer_lstm(embeddings, units = nodes, return_sequences = TRUE, batch_input_shape = c(batch_size, max_sequence_length, embedding_vector), name = 'lstm1')
dense_output <- time_distributed(hidden_layer, layer_dense(units = vocab_size), name='linear')
predictions <- time_distributed(dense_output, layer_activation(activation = 'softmax'), name='softmax')

model<- keras_model(inputs = words, outputs = predictions)

model %>% compile(loss = 'sparse_categorical_crossentropy',
                  optimizer = optimizer_rmsprop(lr = lr))

model$summary()

#Train the model
message('training model')
maxrows<-(nrow(data)%/%batch_size)*batch_size #must have n*batch_size rows

input_data<-data[1:maxrows,1:ncol(data)-1]
input_data<-array(input_data, dim=c(maxrows, ncol(data)-1))
output_data<-data[1:maxrows,2:ncol(data)]
output_data<-array(output_data, dim=c(maxrows,ncol(data)-1))

output_data<-np$expand_dims(output_data, -1L)
rm(data)
checkpointer <- callback_model_checkpoint(filepath="my_weights.hdf5", save_weights_only = TRUE, 
                                               save_best_only = TRUE, monitor = 'loss')
lr_set <- callback_reduce_lr_on_plateau(monitor = 'val_loss')


history <- model %>% fit(input_data, output_data, batch_size = batch_size, epochs = epochs, 
                         shuffle=FALSE, validation_split = 0.1, 
                         callbacks = list(checkpointer, lr_set),
                         verbose=1)

model$save_weights('my_language_model.hdf5')

message('building inference')
#INFERENCE MODEL
words <- layer_input(batch_shape = c(1,1), name='input')
embeddings <- layer_embedding(words, vocab_size, embedding_vector, name = 'embeddings')
hidden_states<-layer_lstm(embeddings, units = nodes, stateful = TRUE, batch_input_shape = c(1, max_sequence_length, embedding_vector), name = 'lstm')
dense_output <- layer_dense(hidden_states, vocab_size, name = 'linear')
predictions <- layer_activation(dense_output, activation = 'softmax', name = 'softmax')

inference_model <- keras_model(inputs = words, outputs = predictions)

inference_model$load_weights('my_language_model.hdf5')

startword<-np$zeros(c(1L,1L))
startword[1,1]<-word2id$START
next_word_probabilities <- inference_model$predict(startword)

#most probable next word:
top_inds <- head(order(next_word_probabilities, decreasing = TRUE), 10)
top_probs <- next_word_probabilities[top_ind]

top_words <- names(word2id[top_inds])

message(paste0("The top words after 'START' are: ", paste(top_words, collapse=', ')))

inference_model$reset_states()

for(j in 1:10){
  inference_model$reset_states()
  startword<-np$zeros(c(1L,1L))
  startword[1,1]<-word2id$START
  new_sentence<-character()
  for(i in 1:max_sequence_length){
    next_word_probabilities<- as.vector(inference_model$predict(startword))
    next_word_probabilities<- next_word_probabilities/sum(next_word_probabilities)
    next_word_ind<-sample(1:length(next_word_probabilities), 1, prob = next_word_probabilities)
    new_sentence<-c(new_sentence, names(word2id[next_word_ind]))
    
    startword[1,1]<-next_word_ind
  }
  
  message(paste0('New Sentence : ', j))
  message(paste0(new_sentence, collapse=' '))
}


  
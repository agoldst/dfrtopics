num_topics := 10
num_iters := 200
seed := 42

instances/docs.mallet: make_instance.R stoplist.txt uk_us.csv
	R -e 'source("make_instance.R"); make_instance()'

model: make_model.R instances/docs.mallet
	R -e "source('make_model.R'); m <- make_model(num_topics=$(num_topics),n_iters=$(num_iters),seed=$(seed))"

.PHONY: model

.DEFAULT_GOAL: model



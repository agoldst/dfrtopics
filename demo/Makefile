NUM_TOPICS := 64
NUM_ITERS := 600

instances/docs.mallet: make_instance.R stoplist.txt uk_us.csv
	R -e 'source("make_instance.R"); make_instance()'

model: make_model.R instances/docs.mallet
	R -e "source('make_model.R'); m <- make_model(num.topics=$(NUM_TOPICS),n.iters=$(NUM_ITERS))"

.PHONY: model

.DEFAULT_GOAL: model

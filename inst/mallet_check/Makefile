# These parameters should match those in test-model_documents.R

num_topics := 16
num_iters := 200
seed := 42
n_hyper_iters := 20
burn_in := 20
threads := 2
alpha_sum := 5
beta := 0.01

count_txt := ../python/count2txt.py
stoplist := ../stoplist/stoplist.txt

data_dir := ../dfr_data

data.csv: $(data_dir)/citations.CSV
	find $(data_dir)/wordcounts -name 'wordcounts*.CSV' | xargs python $(count_txt) > $@

docs.mallet: data.csv $(stoplist)
	mallet import-file --input $< --output $@ \
	    --stoplist-file $(stoplist) \
	    --keep-sequence

model: data.csv docs.mallet
	mallet train-topics --input docs.mallet \
	    --num-topics $(num_topics) \
	    --num-iterations $(num_iters) \
	    --random-seed $(seed) \
	    --optimize-interval $(n_hyper_iters) \
	    --optimize-burn-in $(burn_in) \
	    --num-threads $(threads) \
	    --alpha $(alpha_sum) \
	    --beta $(beta) \
	    --output-state state.gz \
	    --output-topic-keys keys.tsv \
	    --output-doc-topics doc_topics.tsv

clean:
	rm -f data.csv docs.mallet state.gz keys.tsv doc_topics.tsv

.PHONY: model clean

.DEFAULT_GOAL := model

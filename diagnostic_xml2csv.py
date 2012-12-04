#!/usr/bin/env python

from xml.dom import minidom
import csv


def main(script, xml_filename, topic_file, word_diagnostic_file): 
    doc = minidom.parse(xml_filename)
    topics = doc.getElementsByTagName('topic')

    f = open(topic_file,"w")
    topic_writer = csv.writer(f,delimiter=",")
    topic_headers = ["id", "tokens", "document_entropy", "word-length", "coherence", "uniform_dist", "corpus_dist", "eff_num_words", "token-doc-diff", "rank_1_docs", "allocation_ratio", "allocation_count"]
    topic_writer.writerow(topic_headers)

    g = open(word_diagnostic_file,"w")
    word_headers = ["rank","count","prob","cumulative","docs","word-length","coherence","uniform_dist","corpus_dist","token-doc-diff"]
    word_writer = csv.writer(g,delimiter=",")
    word_writer.writerow(["topic","word"] + word_headers)

    for t in topics:
        attrs = [t.getAttribute(a) for a in topic_headers]
        attrs[0] = int(attrs[0]) + 1   # re-index topics from 1, not 0
        topic_writer.writerow(attrs)

        words = t.getElementsByTagName('word')
        for w in words:
            wattrs = [w.getAttribute(a) for a in word_headers]
            word_writer.writerow([attrs[0],w.firstChild.nodeValue] + wattrs)

if __name__ == '__main__':
    import sys
    main(*sys.argv)

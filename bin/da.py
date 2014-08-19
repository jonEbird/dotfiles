#!/bin/env python

import os
import sys
import csv

# Script to help calculate Decision Analysis score
# Feed the script a CSV input with:
# 1. First column being a meaningful label
# 2. First row being the weights

sample_csv = """
WEIGHTS   ,  3, 10,  7,  3,  8,  8, 10
make      ,  3,  5,  9,  8,  1,  5,  5
Autotools ,  3,  6,  9,  8,  2,  5,  4
cmake     , 10, 10, 10,  3,  9, 10,  7
SCons     , 10, 10, 10,  8,  9, 10, 10
Make++    ,  3, 10,  3,  1,  1,  5,  6
Ninja     ,  8,  3,  7,  0,  1,  4,  5
Bitbake   ,  0,  5,  8,  0, 10,  8,  7
Shake     ,  9, 10,  5,  0,  8,  9,  4
"""

def csv2rows(fh):
    """ Pass an open file handle(fh) to read CSV records. Processing input
    and yielding rows back. """
    csv_reader = csv.reader(fh)
    for row in csv_reader:
        if not row:
            continue
        yield [ s.strip() for s in row ]

def crunch_numbers(fh):
    """ Pass file handle for a CSV data source in order to calculate a
    Decision Analysis (DA) score. First row (list) will be used for weights
    and subsequent rows are calculated and returned in a list of tuples
    (label, weighted_score) """
    results = []
    rows = iter(csv2rows(fh))
    weights = rows.next()[1:]
    for row in rows:
        results.append((row[0], sum(map(lambda xy: int(xy[0]) * int(xy[1]),
                                        zip(weights, row[1:]))) ))
    return results

if __name__ == '__main__':

    if len(sys.argv) > 1 and sys.argv[1] == "sample":
        fh = csv.StringIO(sample_csv)
    else:
        fh = sys.stdin

    results = crunch_numbers(fh)
    results.sort(key=lambda x: x[1], reverse=True)
    L = max(map(lambda x: len(x[0]), results))
    for name, weighted_score in results:
        print '%s,%s %d' % (name, ' ' * (L - len(name)), weighted_score)

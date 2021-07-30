#!/usr/bin/env python3
#coding=utf-8
"""..."""
__author__ = 'Simon J. Greenhill <simon@simon.net.nz>'
__copyright__ = 'Copyright (c) 2020 Simon J. Greenhill'
__license__ = 'New-style BSD'

import sys
import json
import codecs
from pathlib import Path

import requests
from treemaker import TreeMaker

GLOTTOLOG_URL = "http://glottolog.org/resource/languoid/id/%s.json"
CACHE_DIR = Path(".cache")


def get_table(filename):
    with codecs.open(filename, 'r', 'utf8') as handle:
        for line in handle:
            yield line.strip()


def get_overrides(filename):
    for row in get_table(filename):
        row = [_.strip() for _ in row.strip().split("\t")]
        assert len(row) == 2, 'invalid override row %r' % row
        yield row


def query(glottocode, url=GLOTTOLOG_URL):
    content = requests.get(url % glottocode)
    if content.status_code == 404:
        raise Exception("Glotto Code %s not found!" % glottocode)
    return content.json()


_isolcounter = 0
def get_classification(j):
    global _isolcounter
    classif = ", ".join([c['name'] for c in j['classification']])
    if len(classif) > 0:
        return classif
    else:
        # handle isolates
        _isolcounter += 1
        return "Isolate-%d" % _isolcounter


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(
        description='Builds a classification tree from a text table'
    )
    parser.add_argument("filename", help='filename for languages')
    parser.add_argument("output", help="output")
    parser.add_argument("--mode", help="set output mode (newick or nexus)", default="nexus", choices=['nexus', 'newick'])
    parser.add_argument("--override", help="override specific classifications", default=None)
    args = parser.parse_args()
    
    if not Path(args.filename).exists():
        raise IOError("Input filename %s doesn't exist!" % args.filename)
    
    if not CACHE_DIR.exists():
        CACHE_DIR.mkdir()
    
    classifications = {}
    # load in overrides if needed
    if args.override and Path(args.override).exists():
        classifications = {k: v for (k, v) in get_overrides(args.override)}
    
    tree = TreeMaker()
    for glottocode in get_table(args.filename):
        if glottocode in classifications:
            continue
        filename = CACHE_DIR / ('%s.json' % glottocode)
        if filename.exists():
            with codecs.open(filename, 'r', 'utf-8') as handle:
                j = json.load(handle)
        else:
            j = query(glottocode)
            with codecs.open(filename, 'w', 'utf-8') as handle:
                json.dump(j, handle, sort_keys=True, indent=4, separators=(',', ': '))
        
        try:
            classifications[glottocode] = get_classification(j)
        except:
            print("Error reading classification from %s" % filename)
            raise
    
    # sort by classification to make output easier to follow
    classif = [(v, k) for (k, v) in classifications.items()]
    for cl, gc in sorted(classif):
        print(gc, cl)
        tree.add(gc, cl)
    
    tree.write_to_file(args.output, mode=args.mode)
    



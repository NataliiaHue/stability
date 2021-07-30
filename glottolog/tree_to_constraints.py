#!/usr/bin/env python3
# coding=utf-8
"""..."""
__author__ = 'Simon J. Greenhill <simon@simon.net.nz>'
__copyright__ = 'Copyright (c) 2020 Simon J. Greenhill'
__license__ = 'New-style BSD'

import re
import codecs
import ete3

is_glottolog_tip = re.compile(r"""'(.*?)\[(\w{4}\d{4})\](.*?)'""")


def read_newick(filename):
    with codecs.open(filename, 'r', 'utf8') as handle:
        return handle.read()


def rename(tree):
    return is_glottolog_tip.sub("\\2", tree)


def make_nexus_data(tree):
    tips = tree.get_leaf_names()
    yield 'BEGIN DATA;'
    yield '    DIMENSIONS NTAX=%d NCHAR=1;' % len(tips)
    yield '    FORMAT DATATYPE=STANDARD SYMBOLS="10" GAP=- MISSING=?;'
    yield 'MATRIX'
    for t in sorted(tips):
        yield '%s 0' % t.ljust(10)
    yield '    ;'
    yield 'END;'


def make_nexus_sets(tree):
    yield 'BEGIN SETS;'
    for label_id, node in enumerate(tree.traverse('levelorder')):
        label = node.name if node.name else "node%d" % label_id
        tips = sorted([t for t in node.get_leaf_names()])
        if len(tips) > 1:
            yield '    taxaset %s = %s;' % (label, " ".join(tips))
    yield 'END;'
    yield ''


def make_xml(tree):
    seen = set()
    for label_id, node in enumerate(tree.traverse('levelorder')):
        label = node.name if node.name else "node%d" % label_id
        tips = sorted([t for t in node.get_leaf_names()])
        
        if node == tree:
            continue
        elif len(tips) == 1:
            continue

        yield '<distribution id="%s.prior" spec="beast.math.distributions.MRCAPrior" monophyletic="true" tree="@Tree.t:tree">'  % label
        yield '    <taxonset id="%s" spec="TaxonSet">' % label

        for tip in tips:
            if tip in seen:
                yield '        <taxon idref="%s"/>' % tip
            else:
                yield '        <taxon id="%s" spec="Taxon"/>' % tip
                seen.add(tip)
        yield '    </taxonset>'
        yield '</distribution>'
        yield ''



if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Does something.')
    parser.add_argument("filename", help='filename')
    parser.add_argument("output", help='output filename')
    args = parser.parse_args()

    nwk = read_newick(args.filename)

    if not nwk.endswith(";"):
        nwk = nwk + ";"

    nwk = rename(nwk)
    tree = ete3.Tree(nwk, format=8)
    
    nexus = ['#NEXUS', '']
    nexus.extend(make_nexus_data(tree))
    nexus.extend(["", ""])
    nexus.extend(make_nexus_sets(tree))

    with codecs.open(args.output, 'w', 'utf8') as out:
        for line in nexus:
            out.write(line + "\n")

    for line in make_xml(tree):
        print(line)


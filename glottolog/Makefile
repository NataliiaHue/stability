
all: languages.trees languages.nwk beast.nex

cache:
	mkdir -p $@

# don't hand curate this, generate it from the all_languages_data.csv file
languages.txt:
	# "head..." = get the first line of the data file
	# "sed s'ID..." = remove the first two columns in that line
	# "tr ..." = replace all delimitors in the csv (=";") with a new line
	# "sort" = sort alphanumerically.
	# "> $@" = put in target file languages.txt
	head -n 1 ../all_languages_data.csv | sed 's/ID;Feature;//g' | tr ';' '\n' | sort > $@

languages.trees: languages.txt
	python make_tree.py --override overrides.txt $< $@ > languages.classification

languages.nwk: languages.txt
	python make_tree.py --override overrides.txt --mode newick $< $@ > /dev/null

beast.nex: languages.nwk
	python tree_to_constraints.py $< $@ > beast_extra.xml

clean:
	rm -rf languages.* beast.nex beast_extra.xml

# Making a glottolog classification tree.


## Step 1: Install python libraries:

```shell
pip install -r requirements.txt
```

## Step 2: Create languages.txt file:

You need a text file called `languages.txt` that lists, one per line, the glottocodes of the languages you want:

```
aaaa1234
bbbb1234
```

## Step 3:  Create any overrides:

If you want to override any of the classifications in glottolog then you need to add these
to a file called `overrides.txt`. Leave this file blank if you don't. Otherwise you need
something that looks like this:

```
khal1270	Turkic, Common Turkic
```

## Step 4: Run make_tree.py:

```shell
python make_tree.py --override overrides.txt languages.txt languages.trees
````

... and your tree will now be in languages.trees
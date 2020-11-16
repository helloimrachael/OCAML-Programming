# OCAML-Programming

The project involves the design and implementation of a simple extension of the functional didactic language that allows you to manipulate dictionaries. A dictionary is a collection of values uniquely identified by a key: a dictionary is a collection of key-value pairs where the key is unique.

Dictionaries are characterized by several primitive operations:
- the insert operation inserts a key-value pair in the dictionary;
- the delete operation deletes a key-value pair from a dictionary;
- the has_key operation checks the existence of the key in a dictionary;
- the iterat operation (f, d) applies f to all key-value pairs present in d, returning a new dictionary with the values obtained as a result of the function;
- the fold operation (f, d) calculates the value obtained by applying the function sequentially to all the elements of the dictionary;
- the filter operation (key, list, d) returns as a result the dictionary obtained from the dictionary d by eliminating all the key-value pairs for which the key does not belong to the list of keys passed as a parameter.

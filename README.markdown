tables
======
tables is a primitive, naive MD5 lookup table generator.

The table is saved onto disk as a directory structure. This means, that the
plaintext corresponding to the hash `64e88b02ad08079e342d827715ab4eca`
would be in the file located at
`tree/6/4/e/8/8/b/0/2/a/d/0/8/0/7/9/e/3/4/2/d/8/2/7/7/1/5/a/b/4/e/c/a`.

## Usage
tables has two features: generating a table and doing lookup operations on it.

### Generating a table
tables needs two things to generate a table:
- A [hashcat rule-file](https://hashcat.net/wiki/doku.php?id=rule_based_attack)
- A wordlist

To generate the table, simply run:
```
./tables create path/to/rules path/to/words path/to/tree/toplevel
```

### Doing a lookup operation
To look up a file of hashes in a table, run:
```
./tables lookup path/to/hashes path/to/tree/toplevel
```

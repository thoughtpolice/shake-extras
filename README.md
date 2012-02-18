# Haskell bindings for LevelDB

[LevelDB][] is a fast key-value storage library by Google.

These are the awesome Haskell bindings.

[Homepage][main page].

# Features

Most of the features of the C bindings:

  * Automatic compression if you have [Snappy](http://snappy.googlecode.com) installed.
  * Snapshots
  * Repair functionality
  * Batch-based atomic writes
  * Iterator support
  * Properties and approximate sizes on the filesystem
  * Moderately high level bindings, along with low-level C based bindings

# Installation

Install the latest version of the bindings from Hackage:

    $ cabal install leveldb

To install the latest git version, you'll need `autoconf` so
you can regenerate `./configure`:

    $ autoconf
    $ cabal install

# Join in

File bugs in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/hs-leveldb.git`

There's also a [BitBucket mirror][bb]:

* `git clone https://bitbucket.org/thoughtpolice/hs-leveldb.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/hs-leveldb/master/AUTHORS.txt).

# License

BSD3. See `LICENSE.txt` for terms of copyright and redistribution.

[LevelDB]: http://code.google.com/p/leveldb/
[main page]: http://thoughtpolice.github.com/hs-leveldb
[issue tracker]: http://github.com/thoughtpolice/hs-leveldb/issues
[gh]: http://github.com/thoughtpolice/hs-leveldb
[bb]: http://bitbucket.org/thoughtpolice/hs-leveldb

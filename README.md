# hop

[![Build Status](https://travis-ci.org/NicolasDP/hop.png?branch=master)](https://travis-ci.org/NicolasDP/hop)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD\_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)


On hackage: [hop on hackage](http://hackage.haskell.org/package/hop)

# Interface

## Configure your repository

In your repository, use the command line:

    hop init

and then edit the hop file `.git/hop.conf`.

## List all the actual opened pull requests

    hop list

## Show a pull request

    hop show <pull request number>

## Try a pull request

This command will fetch the base of the pull request (for example master) and
will pull the pull request branch, and then rebase this branch on the base.

**Make sure you have saved your work before using this command**

    hop try <pull request number>

## Review a pull request

### Review the diff

The following commands are equivalent

    hop review <pull request number>
    hop review diff <pull request number>

### Review the commits

    hop review commits <pull request number>

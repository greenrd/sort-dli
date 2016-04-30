# sort-dli
Sorts already downloaded images according to the order they appear on a web page.

# Installation
    cabal install

# Run

Note that only the logical order of images in the HTML is considered, not the order they are displayed on the page, 
which might be different.

If you have renamed any images already, those images won't be sorted, because it only looks at the image filenames.

WARNING: The image files are always moved to a subdirectory, called `sorted`, of the current directory. So after running this,
you'll want to either move the contents of `sorted` into the right place, or `cd` somewhere else before running `sort-dli` again -
otherwise _at least some of your images will be overwritten!!_ This is probably a good time to point you to the lack of
warranty in `LICENSE`.

## Unix shell
This way can be used anywhere you have a Unix shell - i.e. Mac OS X, Linux and even Cygwin.

### "I know what I'm doing, just do it"
    sort-dli $URL [$DIR]|sh

### The safer way
First, we view and record what is going to happen:

    sort-dli $URL [$DIR]|tee s.sh

Then, if we like it, we execute it:

    `sh s.sh`
    
s.sh also serves as a record of what happened, should you realise that something went wrong and need to revert what happened.

## Windows, without a Unix shell
_This is untested._

    sort-dli $URL [$DIR] >s.bat

Edit `s.bat` and search and replace all instances of "mv", changing them to "move"; also, search and replace all instances of
the `'` character, changing it to the `"` character. Then run it. I am not going to make this easier because 
you can just install Cygwin (or indeed Linux).

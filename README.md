# sort-dli
Sorts already downloaded images according to the order they appear on a web page.

# Installation
cabal install

# Run
    sort-dli $URL [$DIR]|sh

Note that only the logical order of images in the HTML is considered, not the order they are displayed on the page, 
which might be different.

If you have renamed any images already, those images won't be sorted, because it only looks at the image filenames.

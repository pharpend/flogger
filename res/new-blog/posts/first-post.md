Hello! Welcome to your first post.

This document is written in
[markdown](http://daringfireball.net/projects/markdown/), which is a simple
markup language.

# Which markdown implementation?

That link describes the syntax for the original version of Markdown, which was a
hackish Perl script. Flogger uses
[snoyberg's markdown library](http://hackage.haskell.org/package/markdown),
which is written entirely in Haskell. It includes extra features like cross-site
scripting protection and fenced code blocks.

Since it was written by snoyberg, it's extremely portable and high performing,
but the documentation is deplorable to nonexistent.

Snoyberg's markdown is pretty similar to
[GitHub-flavored markdown](https://help.github.com/articles/github-flavored-markdown),
so you can read that for a primer.

# Using pandoc

On the other end of the spectrum, there is
[pandoc](http://johnmacfarlane.net/pandoc/), which is extremely well-documented
and fully-featured, but is terrible on the performance side. Additionally,
pandoc uses a comparatively restrictive license, which isn't compatible with
Flogger's license.

For those reasons, I didn't integrate pandoc into the library. However, if you
want to use pandoc to convert your favorite markup language into Markdown,
you're more than welcome to.

# flogger-client

This is a command-line client for for the Flogger CMS.

# About Flogger

Flogger is *extremely* RESTful. Among other things, that means the server does
minimal computations.

Most content management systems work off this model, for people reading your
blog -

* Server gets a request
* Server reads all the relevant data from an SQL database.
* Server embeds that data into an HTML page, with some CSS and JS.
* Server responds with that HTML/CSS/JS.

Flogger works slightly differently

* Server gets a request
* Server serves a static file

That means that everything is stored on the server in HTML/CSS/JS. There is no
database involved at any point in the process.

You have to "compile" your blog on your local machine, and then upload that
HTML/CSS/JS to the server.

# Installation

Installation is pretty easy.

1. If you haven't already, install `ghc` and `cabal-install` through your
   distribution's package manager.
2. Run `cabal install flogger-client`.

Make sure `$HOME/.cabal/bin` is in your `$PATH`.

# Usage

First, you need to create a new blog

    mkdir myBlog
    cd myBlog
    touch posts.yml
    floggerc --compile

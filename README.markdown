## Feedparse

`feedparse` provides support for fetching and parsing RSS and Atom
feeds.

## Usage

The `parse-feed` function takes a single URL, and attempts to fetch
it. The `parse-feed-string` parses a string into a feed.

Both functions return a `feed` object, which has slots for the feed
title, type, link, and items. A `feed-item` has slots for title, body,
link, and publishing date.

## Installation

First, fetch the repository into the QuickLisp local-projects (or
equivalent):

```
# cd /path/to/quicklisp/local-projects
cd ~/quicklisp/local-projects
git clone https://github.com/kisom/cl-feedparse.git
```

Then, from your Lisp interpreter:

```
CL-USER> (ql:quickload :cl-feedparse)
CL-USER> (feedparse:parse-feed "http://example.net/index.rss")
```

## Author

* K. Isom (kyle@tyrfingr.is)

## Copyright

Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)

# License

Licensed under the ISC License.

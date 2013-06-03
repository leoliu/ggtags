=========================
 Use GNU Global in Emacs
=========================
 
A package for working with `GNU Global
<http://www.gnu.org/software/global>`_ source tagging system in Emacs.

This package is part of `GNU ELPA <http://elpa.gnu.org>`_
(``M-x list-packages``).

Patches, feature requests and bug reports are welcome. Thanks.

Features
~~~~~~~~

#. Automatically run ``global -u`` when needed
#. Highlight valid tag at point
#. Built on top of ``compile.el`` (asynchonrous and other nice
   features)
#. Support all  output formats  of ``global``:  ``grep``, ``ctags-x``,
   ``cscope`` etc.
#. Abbreviated display of file names

Why GNU Global
~~~~~~~~~~~~~~

The opengrok project composed a feature comparison table between a few
tools. The `page
<http://hub.opensolaris.org/bin/view/Project+opengrok>`_ was taken
offline after 2013-03-24 but `here <http://i.imgur.com/IQCPQ0j.png>`_
is a backup.

Screenshot
~~~~~~~~~~

.. figure:: http://i.imgur.com/d430rmm.png
   :width: 500px
   :target: http://i.imgur.com/d430rmm.png
   :alt: ggtags.png

Tutorial
~~~~~~~~

Type ``M-x ggtags-mode`` to enable the minor mode, or as usual enable
it in your desired major mode hooks. When the mode is on the symbol at
point is underlined if it is a valid tag.

``M-.`` finds definitions or references according to the tag at point,
i.e. if point is at a definition tag find references and vice versa.
``C-u M-.`` is verbose and will ask you the name - with completion
- and the type of tag to search.

If multiple matches are found, navigation mode is entered. In this
mode, ``M-n`` and ``M-p`` moves to next and previous match, ``M-}``
and ``M-{`` to next and previous file respectively. ``M-o`` toggles
between full and abbreviated displays of file names in the auxiliary
popup window. When you locate the right match, press RET to finish
which hides the auxiliary window and exits navigation mode. You can
resume the search using ``M-,``. To abort the search press ``M-*``.

Normally after a few searches a dozen buffers are created visiting
files tracked by GNU Global. ``C-c M-k`` helps clean them up.

Development
~~~~~~~~~~~

The goal is to make working with GNU Global in Emacs as effortless and
intuitively as possible.

Bugs
~~~~

https://github.com/leoliu/ggtags/issues

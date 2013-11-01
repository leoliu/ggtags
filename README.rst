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
#. Support `exuberant ctags <http://ctags.sourceforge.net/>`_ backend.
#. Support all  output formats  of ``global``:  ``grep``, ``ctags-x``,
   ``cscope`` etc.
#. Abbreviated display of file names

Why GNU Global
~~~~~~~~~~~~~~

The opengrok project composed a feature comparison `table
<https://github.com/OpenGrok/OpenGrok/wiki/Comparison-with-Similar-Tools>`_
between a few tools.

Screenshot
~~~~~~~~~~

.. figure:: http://i.imgur.com/d430rmm.png
   :width: 500px
   :target: http://i.imgur.com/d430rmm.png
   :alt: ggtags.png

Config
~~~~~~

Enable ``ggtags-mode`` for C/C++/Java modes::

    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1))))

More languages/modes are supported if `GNU Global
<http://www.gnu.org/software/global>`_ is compiled with
``--with-exuberant-ctags`` to support `exuberant ctags
<http://ctags.sourceforge.net/>`_. Also set the environment variable
``GTAGSCONF`` to the correct location of ``gtags.conf``. For example::

  export GTAGSCONF=/usr/local/share/gtags/gtags.conf

See ``plugin-factory/README`` in GNU Global source for further
information.

Tutorial
~~~~~~~~

Type ``M-x ggtags-mode`` to enable the minor mode, or as usual enable
it in your desired major mode hooks. When the mode is on the symbol at
point is underlined if it is a valid (definition) tag.

``M-.`` finds definitions or references according to the tag at point,
i.e. if point is at a definition tag find references and vice versa.
``M-]`` finds references.

If multiple matches are found, navigation mode is entered, the
mode-line lighter changed, and a navigation menu-bar entry
presented. In this mode, ``M-n`` and ``M-p`` moves to next and
previous match, ``M-}`` and ``M-{`` to next and previous file
respectively. ``M-o`` toggles between full and abbreviated displays of
file names in the auxiliary popup window. When you locate the right
match, press RET to finish which hides the auxiliary window and exits
navigation mode. You can resume the search using ``M-,``. To abort the
search press ``M-*``.

Normally after a few searches a dozen buffers are created visiting
files tracked by GNU Global. ``C-c M-k`` helps clean them up.

Check the menu-bar entry ``Ggtags`` for other useful commands.

Development
~~~~~~~~~~~

The goal is to make working with GNU Global in Emacs as effortlessly
and intuitively as possible.

Bugs
~~~~

https://github.com/leoliu/ggtags/issues

known problems
++++++++++++++

When there is only one match a window is still popped up to be closed
immediately due to a `bug <http://debbugs.gnu.org/13594>`_ in
``compile.el``.

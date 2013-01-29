=========================
 Use GNU Global in Emacs
=========================
 
A package for working with `GNU Global
<http://www.gnu.org/software/global>`_ source tagging system inside
Emacs.

This package is part of `GNU ELPA <http://elpa.gnu.org>`_.

Feature requests and bug reports are welcome. Thanks.

Features
~~~~~~~~

#. Automatically run ``global -u`` when needed
#. Highlight valid tag at point
#. Built on top of ``compile.el`` (asynchonrous and other nice
   features)
#. Abbreviated display of file names

Screenshot
~~~~~~~~~~

.. figure:: http://i.imgur.com/d430rmm.png
   :width: 400px
   :target: http://i.imgur.com/d430rmm.png
   :alt: ggtags.png

Install
~~~~~~~

Place ``ggtags.el`` in the ``load-path`` and add to your init file::

  (require 'ggtags)

Use ``M-x ggtags-mode`` to enable the mode and ``M-.`` to find tags.

Bugs
~~~~

https://github.com/leoliu/ggtags/issues

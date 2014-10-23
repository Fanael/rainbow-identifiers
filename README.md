[![Melpa Status](http://melpa.org/packages/rainbow-identifiers-badge.svg)](http://melpa.org/#/rainbow-identifiers)
[![Melpa Stable Status](http://stable.melpa.org/packages/rainbow-identifiers-badge.svg)](http://stable.melpa.org/#/rainbow-identifiers)

# Rainbow identifiers mode

Rainbow identifiers mode is an Emacs minor mode providing highlighting of
identifiers based on their names. Each identifier gets a color based on a hash
of its name.

![Screenshot of rainbow identifiers mode on its own code](https://raw.githubusercontent.com/Fanael/rainbow-identifiers/master/rainbow-identifiers.png)

## Installation

The package is available in [MELPA](http://melpa.org/).

If you have MELPA in `package-archives`, use

    M-x package-install RET rainbow-identifiers RET

If you don't, open `rainbow-identifiers.el` in Emacs and call
`package-install-from-buffer`.

## Usage

To toggle the mode

    M-x rainbow-identifiers-mode

To turn it on automatically in most programming modes:

    (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

## Customization

To change the colors, change faces `rainbow-identifiers-identifier-<number>`.

To change the number of colors used, change the variable
`rainbow-identifiers-face-count`.

Since version 0.1.3 it's possible to change how colors/faces are chosen. By
default the old behavior is used, but it can be changed by setting the variable
`rainbow-identifiers-choose-face-function` to a function that takes a hash and
returns a face specifier. Currently there are two such functions predefined:
 * `rainbow-identifiers-predefined-choose-face`, the default, old behavior.
 * `rainbow-identifiers-cie-l*a*b*-choose-face`, will generate colors
   in the CIE L\*a\*b\* color space without depending on any face. The
   color generation can be influenced by changing the following
   variables:
    * `rainbow-identifiers-cie-l*a*b*-lightness`
    * `rainbow-identifiers-cie-l*a*b*-saturation`
    * `rainbow-identifiers-cie-l*a*b*-color-count`

Since version 0.2 it's possible to allow `rainbow-identifiers` to override some
of the highlighting done by the major mode or other minor modes by setting
`rainbow-identifiers-faces-to-override` to a list of faces `rainbow-identifiers`
can override.

Also since version 0.2 it's possible to filter which identifiers are highlighted
by adding functions to the `rainbow-identifiers-filter-functions` hook. Only the
identifiers for which all functions in the hook return non-nil are highlighted.

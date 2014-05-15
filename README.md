# Rainbow identifiers mode

Rainbow identifiers mode is an Emacs minor mode providing highlighting
of identifiers based on their names. Each identifier gets a color
based on a hash of its name.

![Screenshot of rainbow identifiers mode on its own code](https://raw.github.com/Fanael/rainbow-identifiers/master/rainbow-identifiers.png)

## Installation

The package is available in [MELPA](http://melpa.milkbox.net/).

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

To change the colors, change faces
`rainbow-identifiers-identifier-<number>`.

To change the number of colors used, change the variable
`rainbow-identifiers-face-count`.

Version 0.1.3 introduced a way to change how colors/faces are
chosen. By the fault the old behavior is used, but it can be changed
by setting the variable `rainbow-identifiers-choose-face-function` to
a function that takes a hash and returns a face specifier. Currently
there are two such functions predefined:
 * `rainbow-identifiers-predefined-choose-face`, the default, old behavior.
 * `rainbow-identifiers-cie-l*a*b*-choose-face`, will generate colors
   in the CIE L*a*b* color space without depending on any face. The
   color generation can be influenced by changing the following
   variables:
    * `rainbow-identifiers-cie-l*a*b*-lightness`
    * `rainbow-identifiers-cie-l*a*b*-saturation`
    * `rainbow-identifiers-cie-l*a*b*-color-count`

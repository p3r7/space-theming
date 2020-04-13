# space-theming

Port of [Spacemacs theming layer](https://www.spacemacs.org/layers/+themes/theming/README.html) ([code](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bthemes/theming)) to vanilla Emacs.

This package allows to easily override theme faces by configuration.

For more context, read the [accompanying blog post](https://www.eigenbahn.com/2020/04/06/emacs-theme-override).

Part of this documentation comes from the [original layer documentation](https://www.spacemacs.org/layers/+themes/theming/README.html).


## Installation

Not yet on [Melpa](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

```el
(use-package space-theming
  :quelpa (space-theming :fetcher github :repo "p3r7/space-theming"))
```

## Usage

### General

After the package is loaded, you need to put the following in your configuration (init.el):

```el
(space-theming-init-theming)
```

If a theme gets loaded before `space-theming-init-theming` gets called, you'd need to tell space-theming about it like so:

```el
(setq space-theming--current-theme <theme-loaded-at-startup>)
(space-theming-update-current-theme)
```

Starting on Emacs 27, you might have to set the following (to revert a change of behavior for underlying function `custom--inhibit-theme-enable`):

```el
(setq custom--inhibit-theme-enable nil)
```


### Basic

Individual attributes of theme faces can be overridden with var `space-theming-modifications`. It should be a list of the following form:

```el
((theme1 (face1 attributes...)
         (face2 attributes...)
         ...)
 (theme2 (face1 attributes...)
         (face2 attributes...)
         ...)
 ...)
```

This will apply the given attributes to the relevant faces whenever the appropriate theme is loaded. To update without changing the theme, use command `space-theming-update-current-theme`.


### Headers

In addition, this package provides vars to ease overriding header faces.

Those vars take a list of themes (or the special value `'all`).

| var                                           | description                                                            |
|-----------------------------------------------|------------------------------------------------------------------------|
| `space-theming-headings-inherit-from-default` | Themes for which headers should inherit the `default` face             |
| `space-theming-headings-same-size`            | Themes for which headers should all have the same size as regular text (`:height` attribute) |
| `theming-headings-bold`                       | Themes for which headers should be bold (`:weight` set to `bold`)      |

The list of faces to consider as headers is defined with var `space-theming--header-faces`.


## Example configuration

Assuming we're using `use-package`:

```el
(use-package space-theming
  ;; ...
  :demand

  :init
  (setq space-theming-modifications
        '((white-sand
           (cursor :background "#585858")
           (region :background "#a4a4a4" :foreground "white")
           (form-feed-line :strike-through "#a9a9a9"))
          (tango
           (hl-line :inherit nil :background "#dbdbd7")
           (form-feed-line :strike-through "#b7b8b5")
           (font-lock-comment-face :foreground "#b7b8b5"))))
  (setq space-theming-headings-inherit-from-default 'all)
  (setq space-theming-headings-same-size 'all)

  ;; make it work under emacs 27
  (setq custom--inhibit-theme-enable nil)

  :config
  (space-theming-init-theming)

  ;; NB: assuming theme white-sand got applied before space-theming got loaded
  (setq space-theming--current-theme 'white-sand)
  (space-theming-update-current-theme))
```

## About faces and attributes

An Emacs theme is analogous to a CSS stylesheet.

It is composed of a list of _faces_ (similar to CSS _classes_) to which properties called _attributes_ are associated.

Some of the more common attributes you might want to tweak are the following:

| attribute                     | description                                                  |
|-------------------------------|--------------------------------------------------------------|
| `:inherit`                    | name of a face to inherit attributes from                    |
| `:foreground` / `:background` | hexadecimal color strings                                    |
| `:height`                     | a floating point number (1.0 to inherit from parent)         |
| `:weight`                     | typically either `normal` or `bold`                          |
| `:underline`                  | typically `nil` or `t`                                       |
| `:slant`                      | typically `normal`, `oblique` or `italic`                    |
| `:box`                        | set to `t` to draw a box around characters in the foreground |

For more up-to date information, you can take a look at the [face attributes section](https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html#Face-Attributes) in the Emacs manual.

To get the list of available faces, run command `list-faces-display`.

To get the list of attributes values for a specific face, run command `describe-face <face_name>`.

The most primitive faces (from which themes generally make other faces inherit) are:

 - those from group `basic-faces` from faces.el, which are super generic: `default`, `highlight`, `link`...
 - those from group `font-lock-faces` from font-lock.el, which correspond to syntax highlighting in source code: `font-lock-keyword-face`, `font-lock-comment-face`, `font-lock-string-face`...


## Edge-case: applying to new frames

The package fails to apply our overrides to new frames associated to the same Emacs instance.

The way I dealt with this issue is by creating file ~/.emacs.d/client-init.el with the following content:

```el
(mapc
 (lambda (frame)
   (select-frame frame 't)
   (space-theming-update-current-theme))
 (frame-list))
```

And then creating new frames with:

    $ emacsclient -c -a "" -e "(load-file \"~/.emacs.d/client-init.el\")"

I also tried doing the following in init.el but this did not work:

```el
(add-hook 'after-make-frame-functions
            (lambda (_current-frame)
              (space-theming-update-current-theme)))
```


## Implementation details

### Naming differences with original layer

Public symbols:

| type of symbol     | name in original layer                  | name in space-theming                         |
|--------------------|-----------------------------------------|-----------------------------------------------|
| function           | `theming/init-theming`                  | `space-theming-init-theming`                  |
| function (command) | `spacemacs/update-theme`                | `space-theming-update-current-theme`          |
| var                | `theming-modifications`                 | `space-theming-modifications`                 |
| var                | `theming-headings-inherit-from-default` | `space-theming-headings-inherit-from-default` |
| var                | `theming-headings-same-size`            | `space-theming-headings-same-size`            |
| var                | `theming-headings-bold`                 | `space-theming-headings-bold`                 |

Private symbols:

| type of symbol | name in original layer                  | name in space-theming                         |
|----------------|-----------------------------------------|-----------------------------------------------|
| function       | `spacemacs//theming`                    | `space-theming--theming`                      |
| function       | `spacemacs//in-or-all `                 | `space-theming--in-or-all`                    |
| var            | `spacemacs--theming-header-faces`       | `space-theming--header-faces`                 |

Please note that var `spacemacs--theming-modified-faces` has not been ported.
It's use-case seems to be to undo modifications to current theme when the user manually calls `spacemacs/update-theme` after changing overrides for current theme (`theming-modifications`) without having changed theme in the meantime.

It added quite an overhead to a use-case that could be dealt with simply re-applying current theme.


### Current theme tracking

I made the package track the current theme being used, into var `space-theming--current-theme`. This is done by adding function `space-theming--track-theme` as an _:after_ advice to `load-theme`.

Originally, the Spacemacs layer was tracking it with var `spacemacs--cur-theme` that gets set in the [core theme switcher](https://github.com/syl20bnr/spacemacs/blob/master/core/core-themes-support.el).


## Legibility

This code uses form feeds (`^L` character) as separators.

Package [form-feed](https://github.com/wasamasa/form-feed) makes them appear as intended.

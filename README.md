# space-theming

Port of [Spacemacs theming layer](https://www.spacemacs.org/layers/+themes/theming/README.html) ([code](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bthemes/theming)) to vanilla Emacs.

This package allows to easily override theme faces by configuration.

For more context, read the [accompanying blog post](https://www.eigenbahn.com/2020/04/06/emacs-theme-override).


## Installation

Not yet on [Melpa](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

```el
(use-package space-theming
  :quelpa (space-theming :fetcher github :repo "p3r7/space-theming"))
```

## Usage

For more details see the [original layer documentation](https://www.spacemacs.org/layers/+themes/theming/README.html).

### Basic

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
           (font-lock-comment-face :foreground "#b7b8b5"))
          (chocolate
           (dired-directory :foreground "#EAEAFE") ; chocolate-hue-2
           (org-block :inherit default)
           (org-block-begin-line :foreground "#594A3B")
           (org-block-end-line :foreground "#594A3B")
           (region :background "#C77497" :foreground "black")
           (mode-line :background "#594A3B") ; chocolate-dark-yellow
           (mode-line-inactive :background "#2b241d") ; darker derivative of chocolate-dark-yellow
           (fringe :background "#2b241d") ; same as mode-line-inactive
           (show-paren-match :background "white" :foreground "black")
           (show-paren-mismatch :background "red" :foreground "white")
           (form-feed-line :strike-through "#705B5F") ;; :foreground of `font-lock-comment-delimiter-face'
           (bmkp-local-directory :foreground "#45AFBD")
           (bmkp-remote-file :foreground "#C55D67"))))

  :config
  (space-theming-init-theming))
```

If a theme gets loaded before `space-theming-init-theming` gets called, you'd need to put the following in your configuration.

```el
(setq space-theming--current-theme <theme-loaded-at-startup>)
(space-theming-update-current-theme)
```

Starting on Emacs 27, you might have to set the following:

```el
(setq custom--inhibit-theme-enable nil)
```

### Headers

In addition, this package provides vars to help with header faces overrides.

Those vars take a list of themes (or the special value `'all`).

| var                                           | description                                                            |
|-----------------------------------------------|------------------------------------------------------------------------|
| `space-theming-headings-inherit-from-default` | Themes for which headers should inherit the `default` face             |
| `space-theming-headings-same-size`            | Themes for which headers should all have the same size as regular text |
| `theming-headings-bold`                       | Themes for which headers should be `:bold`                                                                       |

The list of faces to consider as headers is defined with var `space-theming--header-faces`.


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
It's use-case seems to be to undo modifications to current theme when the user manually calls `spacemacs/update-theme` after changing overrides for current theme (`theming-modifications`) whithout having changed theme in the meantime.

It added quite an overhead to a use-case that could be dealt with simply re-applying current theme.


### Current theme tracking

I made the package track the current theme being used, into var `space-theming--current-theme`. This is done by adding function `space-theming--track-theme` as an _:after_ advice to `load-theme`.

Originally, the Spacemacs layer was tracking it with var `spacemacs--cur-theme` that gets set in the [core theme switcher](https://github.com/syl20bnr/spacemacs/blob/master/core/core-themes-support.el).

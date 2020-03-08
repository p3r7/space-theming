# space-theming

Port of [Spacemacs theming layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bthemes/theming
) to vanilla Emacs.

This package allows to easilly override theme faces by configuration.


## Installation

Not yet on [Melpa](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

```el
(use-package space-theming
  :quelpa (space-theming :fetcher github :repo "p3r7/space-theming"))
```

## Usage

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

I made the package track the current theme being used, into var `space-theming--current-theme`. This is done by adding function `space-theming--track-theme` as an _:after_ advice to `load-theme`.

Originally, the Spacemacs layer was referring it with var `spacemacs--cur-theme` that gets set in the [core theme switcher](https://github.com/syl20bnr/spacemacs/blob/master/core/core-themes-support.el).

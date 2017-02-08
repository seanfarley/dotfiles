;;; spaceline-colors.el --- Color theming for custom ATI spaceline

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar spaceline--theme-colors-alist
  '((gruvbox        ((active   . ((highlight . ((background . "#83a598") (foreground . "#1d2021")))
                                  (default   . ((background . "#3c3836") (foreground . "#f4e8ba")))
                                  (other     . ((background . "#1d2021") (foreground . "#928374")))
                                  (middle    . ((background . "#282828")))))

                     (inactive . ((default   . ((background . "#1d2021") (foreground . "#a89984")))
                                  (other     . ((background . "#1d2021") (foreground . "#a89984")))
                                  (middle    . ((background . "#282828")))))))

    (creamsody      ((active   . ((highlight . ((background . "#529F96") (foreground . "#1d2021")))
                                  (default   . ((background . "#DFE5C5") (foreground . "#1d2021")))
                                  (other     . ((background . "#282C32") (foreground . "#FDF4C1")))
                                  (middle    . ((background . "#282C32")))))

                     (inactive . ((default   . ((background . "#1d2021") (foreground . "#a89984")))
                                  (other     . ((background . "#1d2021") (foreground . "#a89984")))
                                  (middle    . ((background . "#282C32")))))))
    (atom-one-dark  ((active   . ((highlight . ((background . "#98C379") (foreground . "#1d2021")))
                                  (default   . ((background . "#3E4451") (foreground . "#AAAAAA")))
                                  (other     . ((background . "#282C32") (foreground . "#AAAAAA")))
                                  (middle    . ((background . "#282C32")))))

                     (inactive . ((default   . ((background . "#1d2021") (foreground . "#666D7A")))
                                  (other     . ((background . "#1d2021") (foreground . "#666D7A")))
                                  (middle    . ((background . "#282C32")))))))
    (forest-blue    ((active   . ((highlight . ((background . "#fc5a7b") (foreground . "#1d2021")))
                                  (default   . ((background . "#f42f56") (foreground . "#232a2f")))
                                  (other     . ((background . "#203439") (foreground . "#bfb8b1")))
                                  (middle    . ((background . "#203439")))))

                     (inactive . ((default   . ((background . "#253c41") (foreground . "#506064")))
                                  (other     . ((background . "#253c41") (foreground . "#506064")))
                                  (middle    . ((background . "#203439")))))))
    (suscolors      ((active   . ((highlight . ((background . "#d75f00") (foreground . "#262626")))
                                  (default   . ((background . "#d78700") (foreground . "#303030")))
                                  (other     . ((background . "#1d2021") (foreground . "#D8553B")))
                                  (middle    . ((background . "#1d2021")))))

                     (inactive . ((default   . ((background . "#1d2021") (foreground . "#D8553B")))
                                  (other     . ((background . "#1d2021") (foreground . "#EF3935")))
                                  (middle    . ((background . "#262626")))))))
    (liso           ((active   . ((highlight . ((background . "#C8FF03") (foreground . "#262626")))
                                  (default   . ((background . "#6E8C02") (foreground . "#303030")))
                                  (other     . ((background . "#6E8C02") (foreground . "#303030")))
                                  (middle    . ((background . "#272C2E")))));

                     (inactive . ((default   . ((background . "#131617") (foreground . "#31454F")))
                                  (other     . ((background . "#131617") (foreground . "#31454F")))
                                  (middle    . ((background . "#131617")))))))
    (peacock       ((active   . ((highlight . ((background . "#FF5D38") (foreground . "#262626")))
                                  (default   . ((background . "#3e3c38") (foreground . "#E0E4CC")))
                                  (other     . ((background . "#3e3c38") (foreground . "#E0E4CC")))
                                  (middle    . ((background . "#2B2A27")))));

                     (inactive . ((default   . ((background . "#3e3c38") (foreground . "#524e48")))
                                  (other     . ((background . "#3e3c38") (foreground . "#524e48")))
                                  (middle    . ((background . "#3e3c38")))))))
    ;; Light themes
    (spacemacs-light ((active   . ((highlight . ((background . "#9380b2") (foreground . "#edf2e9")))
                                 (default   . ((background . "#efeae9") (foreground . "#655370")))
                                 (other     . ((background . "#e3dedd") (foreground . "#655370")))
                                 (middle    . ((background . "#efeae9")))));

                    (inactive . ((default   . ((background . "#efeae9") (foreground . "#a094a2")))
                                 (other     . ((background . "#efeae9") (foreground . "#a094a2")))
                                 (middle    . ((background . "#efeae9")))))))
    ))

(defun spaceline--set-face (face alist)
  "Set FACE to be the foreground & background defined in ALIST."
  (let-alist alist (set-face-attribute face nil :foreground .foreground :background .background)))

(defun spaceline-update-faces (&rest args)
  "Update the faces for the current theme from `custom-enabled-themes'.
ARGS is needed to allow for this function to be used as advice"
  (let ((theme-alist (cadr (assoc (car custom-enabled-themes) spaceline--theme-colors-alist))))
    (when theme-alist
      (let-alist theme-alist
        (spaceline--set-face 'spaceline-highlight-face  .active.highlight)
        (spaceline--set-face 'powerline-active2         .active.middle)
        (spaceline--set-face 'mode-line                 .active.other)
        (spaceline--set-face 'powerline-active1         .active.default)

        (spaceline--set-face 'powerline-inactive1       .inactive.default)
        (spaceline--set-face 'mode-line-inactive        .inactive.other)
        (spaceline--set-face 'powerline-inactive2       .inactive.middle)))))

(provide 'spaceline-colors)
;;; spaceline-colors.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; arv-refcards.el --- Facilita l'accés a refcards -*- lexical-binding: t -*-

;; Author: Alexis Roda
;; Maintainer: Alexis Roda
;; Version: 0.1
;; Package-Requires: (emacs f)


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Agilitza l'accés a les refcards (documents PDF amb referències
;; ràpides de programes).
;;
;; Per utilitzar-lo cal afegir a l'arxiu de configuració d'Emacs:
;;
;;   (require 'arv-refcards)
;;
;; La comanda principal és `arv-refcards-display-refcard'.
;;
;; Es poden configurar les variables `arv-refcards-path' (directoris
;; on es busquen les refcards) i `arv-refcards-black-list' (refcards a
;; excloure, el nom de l'arxiu sense extensió).

;;; Code:

(defvar arv-refcards-path
  (list (f-join data-directory "refcards"))
  "Llista amb rutes de directoris que contenen refcards.")

(defvar arv-refcards-black-list nil
  "LLista de refcards a ignorar.")

(defun arv-refcards--get-refcards (dir-list)
  "Retorna les refcards trobades dins els directoris de `dir-list'.

`dir-list' és una llista de cadenes on cada element correspon a
un directori. Els directoris que no existeixen son ignorats.

Retorna una llista de parelles amb punt on el primer element és
el nom de l'arxiu sense extensió i el segon element és la ruta
absoluta de l'arxiu."

  (let ((res nil))
    (dolist (dir dir-list)
      (when (file-directory-p dir)
        (dolist (file (directory-files dir nil "\\.pdf$"))
          (setq res (cons (cons (file-name-sans-extension file) (f-join dir file)) res)))))
    res))

(defun arv-refcards--exlude-refcards (cards black-list)
  "Exclou refcards.

Exclou les refcards de `cards' que apareixen dins la llista
`black-list'.

`cards' és una llista de parelles (nom-refcard . ruta-refcard).

`black-list' és una llista de noms de refcards (el nom de l'arxiu
sense l'extensió)."
  (cl-remove-if (lambda (x) (member (car x) black-list)) cards))

(defun arv-refcards--sort (cards)
  "Ordena les refcards alfabèticament per nom.

`cards' és una llista de parelles (nom-refcard . ruta-refcard).

Retorna una llista de parelles (nom-refcard . ruta-refcard)
formada pels elements de `cards' ordenats ascendentment segons el
valor de `nom-refcard'."
  (sort cards (lambda (a b) (string< (car a) (car b)))))

;;; TODO: com gestionar refcards amb el mateix nom?

;;;###autoload
(defun arv-refcards-display-refcard ()
  "Mostra una refcard.

Demana a l'usuari que trii una refcard i la mostra."
  (interactive)
  (let* ((cards (arv-refcards--sort
                 (arv-refcards--exlude-refcards
                  (arv-refcards--get-refcards arv-refcards-path)
                  arv-refcards-black-list)))
         (card (completing-read "Refcard: " cards)))
    (find-file (cdr (assoc card cards)))))

(provide 'arv-refcards)

;;; arv-refcards.el ends here

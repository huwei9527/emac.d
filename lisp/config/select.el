;;; -*- lexical-binding : t ; byte-compile-dynamic : t -*-

;;; Commentary:

;;; Code:

(/require-lib select)

(/advice-add (gui-select-text) :around /gui-select-text-advice)
(/advice-add (gui-selection-value) :around /gui-selection-value-advice)

(/provide)
;;; config/select.el ends here

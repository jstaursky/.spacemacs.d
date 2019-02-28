;;; funcs.el --- Additional C/C++ Layer for Spacemacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun clang-format-region-mozilla (s e)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (clang-format-region s e "Mozilla"))

(defun clang-format-region-llvm (s e)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (clang-format-region s e "LLVM"))

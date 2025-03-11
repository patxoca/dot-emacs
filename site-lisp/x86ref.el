;;; arv-boilerplate.el --- insert boilerplate on file creation -*- lexical-binding: t -*-

;; Author: Alexis Roda
;; Maintainer: Alexis Roda
;; Version: 0.1
;; Package-Requires: (f)
;; Homepage:
;; Keywords:

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

;; Integra una copia local de la documentació de la web
;; https://stanislavs.org/helppc/ amb Emacs.
;;
;; Afegir a la configuració d'Emacs:
;;
;;   (use-package x86ref
;;     :ensure nil
;;     :load-path "conf.d/site-lisp"
;;     :commands 'x86qr
;;     :init
;;     (add-hook 'asm-mode-hook (lambda () (local-set-key (kbd "C-c C-d") #'x86qr))))

;;; Notes:

;; En cas de convertir aquest mòdul en un paquet independent, la
;; funció find-library-name pot ser útil per localitzar la ruta on
;; està instal·lat un paquet (o mòdul) i a partir d'ahi accedir a
;; arxius de dades distribuits junt amb el paquet:
;;
;;   (defvar my-package-data-dir
;;     (file-name-directory (find-library-name "my-package")))
;;
;; També puc incloure-ho dins la configuració d'Emacs.


;;; Code:

(require 'f)

;; mapeig de mnemònics a l'arxiu que el documenta. Cal que el mnemònic
;; estigui en minúscules.

(setq x86qr--mnemonic-index '(("aaa" . "aaa.html")
                              ("aad" . "aad.html")
                              ("aam" . "aam.html")
                              ("aas" . "aas.html")
                              ("adc" . "adc.html")
                              ("add" . "add.html")
                              ("and" . "and.html")
                              ("arpl" . "arpl.html")
                              ("bound" . "bound.html")
                              ("bsf" . "bsf.html")
                              ("bsr" . "bsr.html")
                              ("bswap" . "bswap.html")
                              ("bt" . "bt.html")
                              ("btc" . "btc.html")
                              ("btr" . "btr.html")
                              ("bts" . "bts.html")
                              ("call" . "call.html")
                              ("cbw" . "cbw.html")
                              ("cdq" . "cdq.html")
                              ("clc" . "clc.html")
                              ("cld" . "cld.html")
                              ("cli" . "cli.html")
                              ("clts" . "clts.html")
                              ("cmc" . "cmc.html")
                              ("cmp" . "cmp.html")
                              ("cmps" . "cmps.html")
                              ("cmpsb" . "cmps.html")
                              ("cmpsd" . "cmps.html")
                              ("cmpsw" . "cmps.html")
                              ("cmpxchg" . "cmpxchg.html")
                              ("cwd" . "cwd.html")
                              ("cwde" . "cwde.html")
                              ("daa" . "daa.html")
                              ("das" . "das.html")
                              ("dec" . "dec.html")
                              ("div" . "div.html")
                              ("enter" . "enter.html")
                              ("esc" . "esc.html")
                              ("fwait" . "wait.html")
                              ("hlt" . "hlt.html")
                              ("idiv" . "idiv.html")
                              ("imul" . "imul.html")
                              ("in" . "in.html")
                              ("inc" . "inc.html")
                              ("ins" . "ins.html")
                              ("insb" . "ins.html")
                              ("insd" . "ins.html")
                              ("insw" . "ins.html")
                              ("int" . "int.html")
                              ("into" . "into.html")
                              ("invd" . "invd.html")
                              ("invlpg" . "invlpg.html")
                              ("iret" . "iret.html")
                              ("iretd" . "iret.html")
                              ("ja" . "ja.html")
                              ("jae" . "jae.html")
                              ("jb" . "jb.html")
                              ("jbe" . "jbe.html")
                              ("jc" . "jc.html")
                              ("jcxz" . "jcxz.html")
                              ("je" . "je.html")
                              ("jecxz" . "jcxz.html")
                              ("jg" . "jg.html")
                              ("jge" . "jge.html")
                              ("jl" . "jl.html")
                              ("jle" . "jle.html")
                              ("jmp" . "jmp.html")
                              ("jna" . "jbe.html")
                              ("jnae" . "jb.html")
                              ("jnb" . "jae.html")
                              ("jnbe" . "ja.html")
                              ("jnc" . "jnc.html")
                              ("jne" . "jne.html")
                              ("jng" . "jle.html")
                              ("jnge" . "jl.html")
                              ("jnl" . "jge.html")
                              ("jnle" . "jg.html")
                              ("jno" . "jno.html")
                              ("jnp" . "jnp.html")
                              ("jns" . "jns.html")
                              ("jnz" . "jne.html")
                              ("jo" . "jo.html")
                              ("jp" . "jp.html")
                              ("jpe" . "jp.html")
                              ("jpo" . "jnp.html")
                              ("js" . "js.html")
                              ("jz" . "je.html")
                              ("lahf" . "lahf.html")
                              ("lar" . "lar.html")
                              ("lds" . "lds.html")
                              ("lea" . "lea.html")
                              ("leave" . "leave.html")
                              ("les" . "les.html")
                              ("lfs" . "lfs.html")
                              ("lgdt" . "lgdt.html")
                              ("lgs" . "lgs.html")
                              ("lidt" . "lidt.html")
                              ("lldt" . "lldt.html")
                              ("lmsw" . "lmsw.html")
                              ("lock" . "lock.html")
                              ("lods" . "lods.html")
                              ("lodsb" . "lods.html")
                              ("lodsd" . "lods.html")
                              ("lodsw" . "lods.html")
                              ("loop" . "loop.html")
                              ("loope" . "loope.html")
                              ("loopne" . "loopnz.html")
                              ("loopnz" . "loopnz.html")
                              ("loopz" . "loope.html")
                              ("lsl" . "lsl.html")
                              ("lss" . "lss.html")
                              ("ltr" . "ltr.html")
                              ("mov" . "mov.html")
                              ("movs" . "movs.html")
                              ("movsb" . "movs.html")
                              ("movsd" . "movs.html")
                              ("movsw" . "movs.html")
                              ("movsx" . "movsx.html")
                              ("movzx" . "movzx.html")
                              ("msw" . "msw.html")
                              ("mul" . "mul.html")
                              ("neg" . "neg.html")
                              ("nop" . "nop.html")
                              ("not" . "not.html")
                              ("or" . "or.html")
                              ("out" . "out.html")
                              ("outs" . "outs.html")
                              ("outsb" . "outs.html")
                              ("outsd" . "outs.html")
                              ("outsw" . "outs.html")
                              ("pop" . "pop.html")
                              ("popa" . "popa.html")
                              ("popad" . "popa.html")
                              ("popf" . "popf.html")
                              ("popfd" . "popf.html")
                              ("push" . "push.html")
                              ("pusha" . "pusha.html")
                              ("pushad" . "pusha.html")
                              ("pushf" . "pushf.html")
                              ("pushfd" . "pushf.html")
                              ("rcl" . "rcl.html")
                              ("rcr" . "rcr.html")
                              ("rep" . "rep.html")
                              ("repe" . "repe.html")
                              ("repne" . "repne.html")
                              ("repnz" . "repne.html")
                              ("repz" . "repe.html")
                              ("ret" . "ret.html")
                              ("retf" . "ret.html")
                              ("retn" . "ret.html")
                              ("rol" . "rol.html")
                              ("ror" . "ror.html")
                              ("sahf" . "sahf.html")
                              ("sal" . "sal.html")
                              ("sar" . "sar.html")
                              ("sbb" . "sbb.html")
                              ("scas" . "scas.html")
                              ("scasb" . "scas.html")
                              ("scasd" . "scas.html")
                              ("scasw" . "scas.html")
                              ("setae" . "setae.html")
                              ("setb" . "setb.html")
                              ("setbe" . "setbe.html")
                              ("setc" . "setc.html")
                              ("sete" . "sete.html")
                              ("setg" . "setg.html")
                              ("setge" . "setge.html")
                              ("setl" . "setl.html")
                              ("setle" . "setle.html")
                              ("setna" . "setbe.html")
                              ("setnae" . "setb.html")
                              ("setnb" . "setae.html")
                              ("setnc" . "setnc.html")
                              ("setne" . "setne.html")
                              ("setng" . "setle.html")
                              ("setnge" . "setl.html")
                              ("setnl" . "setge.html")
                              ("setnle" . "setg.html")
                              ("setno" . "setno.html")
                              ("setnp" . "setnp.html")
                              ("setns" . "setns.html")
                              ("setnz" . "setne.html")
                              ("seto" . "seto.html")
                              ("setp" . "setp.html")
                              ("setpe" . "setp.html")
                              ("setpo" . "setnp.html")
                              ("sets" . "sets.html")
                              ("setz" . "sete.html")
                              ("sgdt" . "sgdt.html")
                              ("shl" . "sal.html")
                              ("shld" . "shld.html")
                              ("shr" . "shr.html")
                              ("shrd" . "shld.html")
                              ("sidt" . "sidt.html")
                              ("sldt" . "sldt.html")
                              ("smsw" . "smsw.html")
                              ("stc" . "stc.html")
                              ("std" . "std.html")
                              ("sti" . "sti.html")
                              ("stos" . "stos.html")
                              ("stosb" . "stos.html")
                              ("stosd" . "stos.html")
                              ("stosw" . "stos.html")
                              ("str" . "str.html")
                              ("sub" . "sub.html")
                              ("test" . "test.html")
                              ("verr" . "verr.html")
                              ("verw" . "verw.html")
                              ("wait" . "wait.html")
                              ("wbinvd" . "wbinvd.html")
                              ("xchg" . "xchg.html")
                              ("xlat" . "xlat.html")
                              ("xlatb" . "xlat.html")
                              ("xor" . "xor.html")))

;; mnemònics pel consum de ido-completing-read
(setq x86qr--mnemonic-names
      (mapcar #'car x86qr--mnemonic-index))


(defun x86qr--read-mnemonic ()
  (let* ((s (downcase (symbol-name (symbol-at-point))))
         (default (if (seq-contains-p x86qr--mnemonic-names s)
                      s
                    nil)))
    (ido-completing-read "Instrucció: " x86qr--mnemonic-names nil nil default)))

;;;###autoload
(defun x86qr (instr)
  "Demana una instrucció i mostra la referència."
  (interactive (list (x86qr--read-mnemonic)))
  (let ((file (cdr (assoc instr x86qr--mnemonic-index))))
    (when file
      (eww-open-file (f-join arv/emacs-conf-dir "shared/helppc/" file) t))))

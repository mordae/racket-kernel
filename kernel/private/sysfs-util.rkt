#lang racket/base
;
; sysfs-Access Related Utilities
;

(require
  (for-syntax racket/base)
  (for-syntax racket/syntax))

(require racket/contract
         racket/string)

(require sysfs)

(provide define-sysfs-getter
         define-sysfs-setter)

(provide
  (contract-out
    (first-word->symbol (-> string? symbol?))))


(define-syntax-rule (define-sysfs-getter name leading trailing convert)
  (define (name node-name)
    (let ((result (sysfs-get leading node-name trailing)))
      (and result (convert result)))))


(define-syntax-rule (define-sysfs-setter name leading trailing convert)
  (define (name node-name value)
    (sysfs-set! leading node-name trailing #:value (convert value))))


(define (first-word->symbol str)
  (string->symbol (car (string-split str))))


; vim:set ts=2 sw=2 et:

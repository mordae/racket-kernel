#lang racket/base
;
; Private Foreign Function Definitions
;

(require racket/contract)

(require
  (rename-in ffi/unsafe (-> -->)))

(provide
  (contract-out
    (link-name->index
      (-> (or/c string? bytes?) (or/c #f exact-positive-integer?)))

    (link-index->name
      (-> exact-positive-integer? (or/c #f string?)))))


(define link-name->index
  (get-ffi-obj 'if_nametoindex #f
               (_fun #:save-errno 'posix
                     (name : _string*/utf-8)
                     --> (index : _uint)
                     --> (and (> index 0) index))))

(define link-index->name
  (get-ffi-obj 'if_indextoname #f
               (_fun #:save-errno 'posix
                     (index : _uint)
                     (buffer : (_bytes o 16))
                     --> (result : _bytes)
                     --> (and result (cast buffer _bytes _string/utf-8)))))


; vim:set ts=2 sw=2 et:

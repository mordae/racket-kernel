#lang racket/base
;
; Shared Netlink Socket
;

(require rtnl)

(provide socket)


(define socket
  (nl-socket))


; vim:set ts=2 sw=2 et:

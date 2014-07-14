#lang racket/base
;
; Network-Related sysfs Interfaces
;

(require racket/contract)

(require sysfs)

(require "sysfs-util.rkt")

(provide
  (all-defined-out))


(define-sysfs-getter bond-mode/name
                     "class/net" "bonding/mode" first-word->symbol)
(define-sysfs-setter set-bond-mode/name!
                     "class/net" "bonding/mode" values)

(define-sysfs-getter bond-downdelay/name
                     "class/net" "bonding/downdelay" string->number)
(define-sysfs-setter set-bond-downdelay/name!
                     "class/net" "bonding/downdelay" number->string)

(define-sysfs-getter bond-updelay/name
                     "class/net" "bonding/updelay" string->number)
(define-sysfs-setter set-bond-updelay/name!
                     "class/net" "bonding/updelay" number->string)

(define-sysfs-getter bond-miimon/name
                     "class/net" "bonding/miimon" string->number)
(define-sysfs-setter set-bond-miimon/name!
                     "class/net" "bonding/miimon" number->string)

(define-sysfs-getter bond-lacp-rate/name
                     "class/net" "bonding/lacp_rate" first-word->symbol)
(define-sysfs-setter set-bond-lacp-rate/name!
                     "class/net" "bonding/lacp_rate" symbol->string)

(define-sysfs-getter bond-xmit-hash-policy/name
                     "class/net" "bonding/xmit_hash_policy" first-word->symbol)
(define-sysfs-setter set-bond-xmit-hash-policy/name!
                     "class/net" "bonding/xmit_hash_policy" symbol->string)

(define-sysfs-getter link-bond/name
                     "class/net" "master/ifindex" string->number)


; vim:set ts=2 sw=2 et:
